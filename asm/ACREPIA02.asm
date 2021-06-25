*          DATA SET ACREPIA02  AT LEVEL 061 AS OF 04/10/15                      
*PHASE ACIA02A                                                                  
*INCLUDE SORTER                                                                 
*INCLUDE DATVAL                                                                 
         TITLE 'MCKIM INTERFACE TAPE'                                           
ACIA02   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**ACIA**,R9       R9=2ND BASE REGISTER                         
         L     RA,0(,R1)                                                        
*                                                                               
         USING ACWORKD,RA          RA=A(GLOBAL W/S)                             
         USING LWSD,RC             RC=A(SAVE W/S)                               
         USING BIGPRNTD,R8                                                      
*                                                                               
         LA    RC,SPACEND                                                       
         L     R8,VBIGPRNT                                                      
*                                                                               
         CLI   MODE,RUNFRST        RUN FIRST                                    
         BE    RUNF                                                             
         CLI   MODE,REQFRST        REQ FIRST                                    
         BE    REQF                                                             
         CLI   MODE,PROCTRNS       PROCESS TRANSACTIONS                         
         BE    PROC                                                             
         CLI   MODE,REQLAST        REQ LAST                                     
         BE    REQL                                                             
*                                                                               
EXIT     XMOD1 1                                                                
         EJECT ,                                                                
******************************************************************              
*        INITIALIZE ROUTINE                                      *              
******************************************************************              
*                                                                               
RUNF     DS    0H                                                               
         LA    RE,VTYPES           MOVE VTYPES TO W/S                           
         LA    R0,ADCONS                                                        
         LA    RF,VTYPLNQ                                                       
         LR    R1,RF                                                            
         MVCL  RE,R0                                                            
*                                                                               
         L     R2,=A(BOXRC)                                                     
         ST    RC,0(R2)            SAVE RC                                      
         L     R2,VEXTRAS                                                       
*                                                                               
         USING RUNXTRAD,R2                                                      
*                                                                               
         L     R2,ADMASTD                                                       
*                                                                               
         USING MASTD,R2                                                         
*                                                                               
         L     R2,MCBXAREA                                                      
         ST    R2,ADBOX            STORE ADDR OF BOX ROUTINE                    
         L     R2,=A(BXHOOK)                                                    
         ST    R2,HEADHOOK                                                      
         B     EXIT                                                             
         EJECT ,                                                                
******************************************************************              
*        INITIALIZE SORTER                                       *              
******************************************************************              
*                                                                               
REQF     DS    0H                                                               
         MVI   RCSUBPRG,0          ERROR HEADLINES                              
         MVC   MYSUBPRG,RCSUBPRG                                                
         MVC   PAGE,=H'1'                                                       
         MVI   FORCEHED,C'Y'                                                    
         ZAP   TOTAL1,=P'0'        CLEAR ACCUMS                                 
         ZAP   TOTAL2,=P'0'                                                     
         ZAP   TOTAL3,=P'0'                                                     
         ZAP   TOTAL4,=P'0'                                                     
         ZAP   TOTAL5,=P'0'                                                     
         ZAP   TOTAL6,=P'0'                                                     
         ZAP   TOTREP,=P'0'                                                     
         ZAP   TAPECNT,=P'0'                                                    
         GOTO1 VSORTER,DMCB,SORTCARD,RECCARD,0                                  
*                                                                               
         L     R6,=A(SORTREC)         R6 COVERS SORT RECORD                     
         ST    R6,ASRTREC                                                       
         B     EXIT                                                             
         EJECT ,                                                                
******************************************************************              
*        PROCESS INCOMING TRANSACTIONS - PUT OUT REC TO SORTER   *              
******************************************************************              
*                                                                               
         USING SRTRECD,R6                                                       
         USING DFTBLD,R4                                                        
*                                                                               
PROC     DS    0H                                                               
         L     R6,ASRTREC          R6 COVERS SORT RECORD                        
         XC    0(SRTLNQ,R6),0(R6)  CLEAR OUR SORT AREA                          
         L     R4,ALEDGDEF         R4 POINTS TO LEDGER DEFAULT TABLE            
*                                                                               
*              PROCESS RECORD KEY                                               
*                                                                               
         USING TRNRECD,R5                                                       
*                                                                               
PROC100  L     R5,ADTRANS          R5 POINTS TO ACCOUNT REC                     
         SH    R5,DATADISP                                                      
         MVC   SRTACC2,TRNKUNT     SAVE OFF HDS ACCT                            
         MVC   SRTCAC,TRNKULC      SAVE CONTRA ACCOUNT                          
         CLI   DFUL,X'FF'          ITEM NOT IN TABLE - ADD TO ERROR RPT         
         BE    PROCERR                                                          
         CLC   DFUL,TRNKUNT        MATCH ON UNIT/LEDGER                         
         BNE   PROC150                                                          
*                                                                               
         CLC   DFACC,SPACES        IF SPACES THEN ALL ACCTS ARE VALID           
         BE    PROC120                                                          
         ZIC   R1,DFACCLNQ                                                      
         BCTR  R1,0                                                             
         EXCLC R1,DFACC,TRNKACT    ELSE ACCT MUST MATCH ACCT IN TABLE           
         BNE   PROC150                                                          
*                                                                               
PROC120  MVC   WORK,XSPACES                                                     
         MVC   WORK(14),TRNKCUNT                                                
         TM    DFXSTAT,WRITEOFF    MIGHT HAVE OVERRIDE C/A FROM                 
         BZ    *+8                 WRITEOFF ELEMENT                             
         BAS   RE,GETD9EL                                                       
         CLC   DFCUL,SPACES        IF SPACES THEN ALL CONTRA UL'S GOOD          
         BE    PROC130                                                          
         CLC   DFCUL,WORK          ELSE CONTRA UL MUST MATCH TABLE              
         BNE   PROC150                                                          
*                                                                               
PROC130  CLC   DFCACC,SPACES       IF NO DEFAULT CONTRA ACCT                    
         BE    PROC200                                                          
         CLC   DFCACC,WORK+2       ELSE MUST MATCH C/A                          
         BE    PROC200                                                          
*                                                                               
PROC150  LA    R4,DFLNQ(R4)        BUMP TO NEXT TABLE ENTRY                     
         B     PROC100                                                          
*                                                                               
*              PROCESS TRANSACTION ELEMENT                                      
*                                                                               
         USING TRNELD,R5                                                        
*                                                                               
PROC200  DS    0H                                                               
         L     R5,ADTRANS          R5 NOW POINTS TO TRNS RECORD                 
         TM    TRNSTAT,X'80'       CHECK WHETHER CREDIT OR DEBIT                
         BZ    PROC250                                                          
         TM    DFSTAT,DR                                                        
         BO    PROC300                                                          
         LA    R4,DFLNQ(R4)                                                     
         B     PROC100                                                          
*                                                                               
PROC250  DS    0H                  SEE IF MATCH ON CREDIT                       
         TM    DFSTAT,CR                                                        
         BO    PROC300                                                          
         LA    R4,DFLNQ(R4)        IF NO MATCH ON STATUS THEN GET NEXT          
         B     PROC100                                                          
*                                                                               
PROC300  DS    0H                                                               
         CLI   DFTYPE,0            TABLE ENTRY COVER ALL BTCHS?                 
         BE    PROC350                                                          
         CLC   DFTYPE,TRNTYPE      BATCH TYPES MUST MATCH ON EXCEPTIONS         
         BE    PROC350                                                          
         LA    R4,DFLNQ(R4)        BUMP TO NEXT TABLE ENTRY                     
         B     PROC100                                                          
*                                                                               
PROC350  DS    0H                                                               
         MVC   SRTTYPE,TRNTYPE                                                  
*                                                                               
*              DETERMINE UNIT(MCKIM)/OFFICE(DDS)                                
*                                                                               
         MVC   SRTUNIT,XSPACES                                                  
         MVC   SRTUNIT(L'TRNOFFC),TRNOFFC   DEFAULT=TRNS OFFICE CODE            
*                                                                               
         CLC   TRNOFFC,=C'**'      SKIP PO'S                                    
         BE    EXIT                                                             
*                                                                               
         CLC   SRTACC2(2),=C'SJ'   IF UL=SJ THEN GET OFFICE FROM                
         BNE   *+14                CLIENT OR PRODUCT RECORD                     
         BAS   RE,FINDOFFC                                                      
         MVC   SRTUNIT(2),WORK                                                  
*                                                                               
         TM    DFXSTAT,SCCODE      SPECIAL CODE FOR UL=SC?                      
         BZ    PROC360                                                          
*                                                                               
         USING TRNRECD,R5          SC OFFICE CODE= 0&1ST CHAR OF ACCT           
*                                                                               
         L     R5,ADACC                                                         
         MVI   SRTUNIT,C'0'                                                     
         MVC   SRTUNIT+1(1),TRNKACT                                             
*                                                                               
PROC360  DS    0H                                                               
         CLC   SRTUNIT(2),=C'K '   OFFICE K ---> OFFICE A                       
         BNE   *+10                                                             
         MVC   SRTUNIT(2),=C'A '                                                
*                                                                               
         CLC   SRTUNIT(2),=C'B '   OFFICE B ---> OFFICE T                       
         BNE   *+10                                                             
         MVC   SRTUNIT(2),=C'T '                                                
*                                                                               
         USING TRNELD,R5                                                        
*                                                                               
         L     R5,ADTRANS          R5 NOW POINTS TO TRNS RECORD                 
         MVC   SRTSRCE,DFJENUM     JE#                                          
         BAS   RE,DESCRIP          ANY OVERRIDE DESCRIPTION                     
         MVC   SRTDESC,DFDESC      DESCRIPTION                                  
         OC    SRTDESC(14),WORK    ONLY ALLOW 14 BTYES FROM NAME                
         L     R2,AMONACC                                                       
*                                                                               
         USING ACMD,R2                                                          
*                                                                               
         MVC   SRTENTP,ACMMDTE     SAVE MOS                                     
         MVC   SRTACC,DFMKACC      DEFAULT ACCOUNT                              
         ZAP   SRTAMNT,TRNAMNT                                                  
         MVC   SRTSTAT,TRNSTAT                                                  
         MVC   SRTCLI,XSPACES                                                   
*                                                                               
         USING TRNRECD,R5                                                       
*                                                                               
PROC400  DS    0H                                                               
         L     R5,ADTRANS          R5 NOW POINTS TO TRNS RECORD                 
         SH    R5,DATADISP                                                      
         OC    DFCLI,DFCLI         IF NO CLI USED THEN SKIP THIS PART           
         BZ    PROC500                                                          
*                                                                               
         CLI   DFCLI,SJ24          LOCATED IN THE 24 ELEMENT?                   
         BNE   *+18                                                             
         BAS   RE,GET24EL                                                       
         MVC   SRTCLI,WORK         MOVE IN CLIENT CODE                          
         B     PROC500                                                          
*                                                                               
         CLI   DFCLI,ACCT          LOCATED IN ACCOUNT?                          
         BNE   *+8                                                              
         LA    R3,TRNKACT          POINT TO ACCOUNT                             
*                                                                               
         CLI   DFCLI,CNTR          LOCATED IN C/A?                              
         BNE   *+8                                                              
         LA    R3,TRNKCACT         POINT TO CONTRA ACCOUNT                      
         ZIC   R1,DFCLIDSP                                                      
         AR    R3,R1               BUMP TO CORRECT POSITION IN ACCT             
         ZIC   R1,DFCLILNQ                                                      
         SH    R1,=H'1'                                                         
         BM    PROC500                                                          
         EXMVC R1,SRTCLI,0(R3)     SAVE OFF CLIENT CODE                         
*                                                                               
PROC500  DS    0H                                                               
         L     R5,ADACC                                                         
         MVI   ELCODE,X'3F'        ONLINE MEMO ELEMENT                          
         BAS   RE,GETEL                                                         
         BNE   PROC700                                                          
*                                                                               
         USING OMEELD,R5                                                        
*                                                                               
         ZIC   R3,OMELN                                                         
         SH    R3,=H'2'                                                         
         LA    R1,OMEMO            POINT TO COMMENT                             
*                                                                               
PROC550  DS    0H                                                               
         CLC   0(3,R1),=C'GL='                                                  
         BE    *+16                                                             
         LA    R1,1(R1)                                                         
         BCT   R3,PROC550                                                       
         B     PROC700             MISSING GL= IN ONLINE ELEMENT                
         MVC   SRTACC,3(R1)        SAVE 4 CHAR ACCT NUMBER                      
*                                                                               
PROC700  DS    0H                                                               
         CLC   SRTACC,SPACES                                                    
         BE    PROCERR2            MUST HAVE EITHER AN ONLINE MEMO              
*                                  OR MUST HAVE A TABLE DEFAULT ACCT            
         USING TRNELD,R5                                                        
*                                                                               
         L     R5,ADTRANS          USE TRNS DATE IF NO 60 ELEMENT               
         GOTO1 DATCON,DMCB,(2,TRNDATE),(1,SRTTRDTE)                             
         SH    R5,DATADISP                                                      
*                                                                               
         USING TRSELD,R5                                                        
*                                                                               
         MVI   ELCODE,X'60'        DATE ADDED TO FILE (DA DATE)                 
         BAS   RE,GETEL                                                         
         BNE   PROC750                                                          
         GOTO1 DATCON,DMCB,(2,TRSDATE),(1,SRTTRDTE)                             
*                                                                               
PROC750  GOTO1 VSORTER,DMCB,=C'PUT',(R6)                                        
         B     EXIT                                                             
*                                                                               
*              PRINT OUT ITEM ON ERROR REPORT                                   
*                                                                               
         USING TRNELD,R5                                                        
*                                                                               
PROCERR  DS    0H                                                               
         L     R5,ADTRANS                                                       
         CLC   TRNOFFC,=C'**'      SKIP PO'S                                    
         BE    EXIT                                                             
*                                                                               
         USING PERRD,R7                                                         
         USING TRNRECD,R5          GET INFO FROM KEY                            
*                                                                               
         LA    R7,XP                                                            
         MVC   XP,XSPACES                                                       
         L     R5,ADTRANS                                                       
         SH    R5,DATADISP                                                      
         MVC   PERRACC,TRNKUNT                                                  
         MVC   PERRCACC,TRNKCUNT                                                
*                                                                               
         USING TRNELD,R5                                                        
*                                                                               
         L     R5,ADTRANS          R5 NOW POINTS TO TRNS RECORD                 
         MVC   PERROFFC,TRNOFFC                                                 
         MVC   PERRSTAT,=C'CR'                                                  
         TM    TRNSTAT,X'80'                                                    
         BZ    *+10                                                             
         MVC   PERRSTAT,=C'DR'                                                  
         EDIT  (1,TRNTYPE),(3,PERRTYPE)                                         
         EDIT  (P6,TRNAMNT),(12,PERRAMT),2,CR=YES                               
         GOTO1 DATCON,DMCB,(1,TRNDATE),(5,PERRDATE)                             
         MVC   PERRREF,TRNREF                                                   
         MVC   PERRBREF,TRNBTCH                                                 
         GOTO1 ACREPORT                                                         
         B     EXIT                                                             
*                                                                               
         USING PERRD,R7                                                         
*                                                                               
PROCERR2 DS    0H                                                               
         LA    R7,XP                                                            
         MVC   XP,XSPACES                                                       
         L     R5,ADACC                                                         
         MVC   PERRACC,1(R5)                                                    
         MVC   PERRCACC(40),=CL40'** MISSING ONLINE MEMO GL ACCT **'            
         GOTO1 ACREPORT                                                         
         B     EXIT                                                             
         EJECT ,                                                                
******************************************************************              
*        RETRIEVE RECORDS FROM SORTER                            *              
******************************************************************              
*                                                                               
         USING SRTRECD,R6                                                       
*                                                                               
REQL     DS    0H                                                               
         MVI   RCSUBPRG,1          REGULAR HEADINGS                             
         MVC   MYSUBPRG,RCSUBPRG                                                
         MVC   PAGE,=H'1'                                                       
         MVI   FORCEHED,C'Y'                                                    
         MVC   SAVESRT,XSPACES                                                  
         BAS   RE,INITTAPE                                                      
         MVI   TMPBYTE,0           FLAG TO SEE IF ANY DATA                      
*                                                                               
REQL100  DS    0H                                                               
         GOTO1 VSORTER,DMCB,=C'GET'                                             
         L     R6,DMCB+4                                                        
         ST    R6,ASRTREC          ADDRESS OF LAST SORT                         
         LTR   R6,R6                                                            
         BZ    REQX                END OF RECORDS FROM SORT                     
         MVI   TMPBYTE,X'FF'       YEP, THERE WUZ SOME DATA                     
         BAS   RE,CHKTOT                                                        
         BAS   RE,PRNTRC                                                        
         MVC   SAVESRT,0(R6)       SAVE SORT RECORD                             
         B     REQL100                                                          
*                                                                               
REQX     DS    0H                                                               
         GOTO1 VSORTER,DMCB,=C'END'                                             
         CLI   TMPBYTE,0                                                        
         BNE   REQX1                                                            
         MVC   XP,XSPACES                                                       
         MVC   XP+5(60),=CL60'** NO DATA FOR THIS REQUEST PERIOD **'            
         BAS   RE,PRNTXP                                                        
         CLI   QOPT1,C'Y'                                                       
         BNE   EXIT                                                             
         CLOSE TAPE01                                                           
         B     EXIT                                                             
*                                                                               
REQX1    BAS   RE,BLDTAPE                                                       
         MVI   TOTSW,X'FF'         ALL TOTALS                                   
         BAS   RE,REPORT           PRINT REMAINING TRANSACTION                  
         BAS   RE,TOTALS           PRINT TOTALS                                 
         CLI   QOPT1,C'Y'                                                       
         BNE   EXIT                                                             
         CLOSE TAPE01                                                           
         B     EXIT                                                             
         EJECT ,                                                                
******************************************************************              
*              PROCESS ADDITIONAL DESCRIPTION                    *              
******************************************************************              
*                                                                               
         USING DFTBLD,R4           R4 -> CURRENT TABLE ENTRY                    
*                                                                               
DESCRIP  NTR1                                                                   
         MVC   WORK,XSPACES                                                     
         TM    DFXSTAT,ACCNAME                                                  
         BZ    DES500                                                           
         L     R5,ADACC                                                         
         MVI   ELCODE,X'20'        NAME ELEMENT                                 
         BAS   RE,GETEL                                                         
         BNE   EXIT                                                             
*                                                                               
         USING NAMELD,R5           GET NAME OF ACCOUNT                          
*                                                                               
         ZIC   R1,NAMLN                                                         
         SH    R1,=H'3'                                                         
         BM    EXIT                                                             
         EXMVC R1,WORK,NAMEREC                                                  
         B     EXIT                                                             
*                                                                               
DES500   TM    DFXSTAT,CANAME                                                   
         BZ    EXIT                                                             
         L     R5,ADSUBAC                                                       
         MVI   ELCODE,X'43'        CONTRA ACCOUNT HEADER ELEMENT                
         BAS   RE,GETEL                                                         
         BNE   EXIT                                                             
*                                                                               
         USING CACELD,R5           GET NAME OF ACCOUNT                          
*                                                                               
         ZIC   R1,CACLN                                                         
         SH    R1,=Y(CACLN1Q+1)                                                 
         BM    EXIT                                                             
         EXMVC R1,WORK,CACNAME                                                  
         B     EXIT                                                             
         EJECT ,                                                                
******************************************************************              
*        GET CLIENT CODE FROM X'D9' ELEMENT ON SJ RECORD         *              
******************************************************************              
*                                                                               
         USING TRNRECD,R5                                                       
*                                                                               
GETD9EL  NTR1                                                                   
         MVC   WORK,XSPACES                                                     
         L     R5,ADTRANS                                                       
         SH    R5,DATADISP                                                      
         MVI   ELCODE,X'D9'        RECEIVABLE ALLOCATION ELEMENT                
         BAS   RE,GETEL                                                         
         BNE   EXIT                                                             
*                                                                               
         USING RALELD,R5                                                        
*                                                                               
         CLI   RALTYPE,RALTWOF     SAVE ACCT ONLY IF A WRITE OFF                
         BNE   EXIT                                                             
         MVC   WORK(L'RALWULA),RALWULA                                          
         B     EXIT                                                             
         EJECT ,                                                                
******************************************************************              
*        GET CLIENT CODE FROM X'24' ELEMENT ON SJ RECORD         *              
******************************************************************              
*                                                                               
         USING TRNRECD,R5                                                       
*                                                                               
GET24EL  NTR1                                                                   
         MVC   WORK,XSPACES                                                     
         MVC   RKEY,XSPACES                                                     
*                                                                               
         L     R5,ADTRANS                                                       
         SH    R5,DATADISP                                                      
         CLC   TRNKUNT(2),=C'SJ'                                                
         BNE   *+10                                                             
         MVC   RKEY(L'TRNKCULA),TRNKCULA                                        
         CLC   TRNKCUNT(2),=C'SJ'                                               
         BNE   *+10                                                             
         MVC   RKEY(L'TRNKCULC),TRNKCULC                                        
         CLC   RKEY,XSPACES                                                     
         BE    EXIT                                                             
*                                                                               
GET24A   L     R5,AIOAREA1                                                      
         GOTO1 DATAMGR,DMCB,DMREAD,ACCFIL,RKEY,(R5)                             
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         MVI   ELCODE,X'24'                                                     
         BAS   RE,GETEL                                                         
         BNE   GET24B                                                           
*                                                                               
         USING PPRELD,R5                                                        
*                                                                               
         MVC   WORK(6),PPRRECVA+2  SAVE OFF RECEIVABLE ACCT                     
         CLC   WORK,SPACES                                                      
         BH    GET24X                                                           
*                                                                               
GET24B   DS    0H                                                               
         CLC   RKEY+9(3),XSPACES   JOB LEVEL RECORD?                            
         BE    GET24C                                                           
         MVC   RKEY+9(33),XSPACES  GO READ PRODUCT RECORD NEXT                  
         B     GET24A                                                           
*                                                                               
GET24C   CLC   RKEY+6(3),XSPACES   PRODUCT RECORD?                              
         BE    GET24X                                                           
         MVC   RKEY+6(36),XSPACES  SET NEXT READ FOR CLIENT REC                 
         B     GET24A                                                           
*                                                                               
GET24X   L     R5,ADTRANS                                                       
         SH    R5,DATADISP                                                      
         MVC   RKEY,0(R5)          RESTORE TRANSACTION SEQUENCE                 
         L     R5,AIOAREA1                                                      
         GOTO1 DATAMGR,DMCB,DMREAD,ACCFIL,RKEY,(R5)                             
         CLI   DMCB+8,0                                                         
         BE    EXIT                                                             
         DC    H'0'                                                             
         EJECT ,                                                                
******************************************************************              
*        GET OFFICE FROM X'24' ELEMENT                           *              
******************************************************************              
*                                                                               
         USING TRNRECD,R5                                                       
*                                                                               
FINDOFFC NTR1                                                                   
         MVC   WORK,XSPACES                                                     
         MVC   RKEY,XSPACES                                                     
*                                                                               
         L     R5,ADTRANS                                                       
         SH    R5,DATADISP                                                      
         MVC   RKEY(9),TRNKCULA    READ PRODUCT RECORD                          
*                                                                               
FIND50   L     R5,AIOAREA1                                                      
         GOTO1 DATAMGR,DMCB,DMREAD,ACCFIL,RKEY,(R5)                             
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         MVI   ELCODE,X'24'                                                     
         BAS   RE,GETEL                                                         
         BNE   FIND100                                                          
*                                                                               
         USING PPRELD,R5                                                        
*                                                                               
         MVC   WORK(L'PPRGAOFF),PPRGAOFF      SAVE OFFICE                       
         CLC   WORK,XSPACES                                                     
         BH    FINDX                                                            
*                                                                               
FIND100  CLC   RKEY+6(3),SPACES    PRODUCT RECORD?                              
         BE    FINDX                                                            
         MVC   RKEY+6(36),SPACES   SET NEXT READ FOR CLIENT REC                 
         B     FIND50                                                           
*                                                                               
FINDX    L     R5,ADTRANS                                                       
         SH    R5,DATADISP                                                      
         MVC   RKEY,0(R5)          RESTORE TRANSACTION SEQUENCE                 
         L     R5,AIOAREA1                                                      
         GOTO1 DATAMGR,DMCB,DMREAD,ACCFIL,RKEY,(R5)                             
         CLI   DMCB+8,0                                                         
         BE    EXIT                                                             
         DC    H'0'                                                             
         EJECT ,                                                                
******************************************************************              
*        CHECK FOR TOTALS                                        *              
******************************************************************              
*                                                                               
         USING SRTRECD,R6                                                       
*                                                                               
CHKTOT   NTR1                                                                   
         MVI   TOTSW,0                                                          
         CLC   SAVESRT,XSPACES     SEE IF FIRST TIME IN                         
         BE    EXIT                                                             
         L     R6,ASRTREC          R6 COVERS SORT RECORD                        
*                                                                               
*        CLC   SRTACC2(2),=C'SE'   DONT SUMMARIZE FOR UL=SE                     
*        BNE   *+8                                                              
*        BAS   RE,BLDTAPE                                                       
*                                                                               
         CLC   SRTKEY,SAVESRT                                                   
         BE    CHK50               ANY CHANGE IN ALL LEVELS?                    
*        CLC   SRTACC2(2),=C'SE'   DONT SUMMARIZE FOR UL=SE                     
*        BE    *+8                                                              
         BAS   RE,BLDTAPE                                                       
         OI    TOTSW,TOT6                                                       
         CLC   SRTKEY(SRTCLI-SRTKEY+L'SRTCLI),SAVESRT                           
         BE    CHK50                                                            
         OI    TOTSW,TOT5                                                       
         CLC   SRTKEY(SRTACC-SRTKEY+L'SRTACC),SAVESRT                           
         BE    CHK50                                                            
         OI    TOTSW,TOT4                                                       
         CLC   SRTKEY(SRTENTP-SRTKEY+L'SRTENTP),SAVESRT                         
         BE    CHK50                                                            
         OI    TOTSW,TOT3                                                       
         CLC   SRTKEY(SRTSRCE-SRTKEY+L'SRTSRCE),SAVESRT                         
         BE    CHK50                                                            
         OI    TOTSW,TOT2                                                       
         CLC   SRTKEY(SRTUNIT-SRTKEY+L'SRTUNIT),SAVESRT                         
         BE    CHK50                                                            
         OI    TOTSW,TOT1                                                       
*                                                                               
CHK50    DS    0H                                                               
         BAS   RE,REPORT           PRINT REMAINING TRANSACTION                  
         BAS   RE,TOTALS           PRINT TOTALS                                 
*                                                                               
CHKX     DS    0H                                                               
         OC    TOTSW,TOTSW                                                      
         BZ    EXIT                                                             
         LA    RE,PRNTREC          CLEAR OUT BUFFER                             
         LH    RF,=Y(PRLNQ)                                                     
         LA    R0,XSPACES                                                       
         SR    R1,R1                                                            
         ICM   R1,8,XSPACES                                                     
         MVCL  RE,R0                                                            
         LA    R4,PRNTREC                                                       
*                                                                               
         USING PRNRECD,R4                                                       
*                                                                               
         ZAP   PRTPTOT,=P'0'       INITIALIZE ACCUM                             
         B     EXIT                                                             
         EJECT ,                                                                
******************************************************************              
*        PRINT TOTALS                                            *              
******************************************************************              
*                                                                               
         USING PRNRECD,R4                                                       
         USING PLINED,R7                                                        
*                                                                               
TOTALS   NTR1                                                                   
         OC    TOTSW,TOTSW         SEE IF ANY TOTALS TO PRINT                   
         BZ    EXIT                                                             
         LA    R4,PRNTREC                                                       
         LA    R7,XP                                                            
         MVC   XP,XSPACES                                                       
         BAS   RE,PRNTXP                                                        
*                                                                               
         TM    TOTSW,TOT6                                                       
         BZ    TOT100                                                           
         MVC   PTOTAL,=CL30'TOTAL FOR'                                          
         GOTO1 DATCON,DMCB,(0,PRTRNDTE),(5,PTOTAL+10)                           
         EDIT  (P8,TOTAL6),(14,PAMOUNT),2,CR=YES                                
         BAS   RE,PRNTXP                                                        
         ZAP   TOTAL6,=P'0'                                                     
*                                                                               
TOT100   DS    0H                                                               
         TM    TOTSW,TOT5                                                       
         BZ    TOT200                                                           
         CLC   PRPROJCT,SPACES                                                  
         BE    TOT150                                                           
         MVC   PTOTAL,=CL30'TOTAL FOR PROJECT'                                  
         MVC   PTOTAL+18(8),PRPROJCT                                            
         EDIT  (P8,TOTAL5),(14,PAMOUNT),2,CR=YES                                
         BAS   RE,PRNTXP                                                        
TOT150   ZAP   TOTAL5,=P'0'                                                     
*                                                                               
TOT200   DS    0H                                                               
         TM    TOTSW,TOT4                                                       
         BZ    TOT300                                                           
         MVC   PTOTAL,=CL30'TOTAL FOR'                                          
         MVC   PTOTAL+10(8),PRACC                                               
         EDIT  (P8,TOTAL4),(14,PAMOUNT),2,CR=YES                                
         BAS   RE,PRNTXP                                                        
         ZAP   TOTAL4,=P'0'                                                     
*                                                                               
TOT300   DS    0H                                                               
         TM    TOTSW,TOT3                                                       
         BZ    TOT400                                                           
         MVC   PTOTAL,=CL30'TOTAL FOR'                                          
         GOTO1 DATCON,DMCB,(0,PRTRNDTE),(6,PTOTAL+10)                           
         EDIT  (P8,TOTAL3),(14,PAMOUNT),2,CR=YES                                
         BAS   RE,PRNTXP                                                        
         ZAP   TOTAL3,=P'0'                                                     
*                                                                               
TOT400   DS    0H                                                               
         TM    TOTSW,TOT2                                                       
         BZ    TOT500                                                           
         MVC   PTOTAL,=CL30'TOTAL FOR JE#'                                      
         MVC   PTOTAL+14(2),PRSOURCE                                            
         EDIT  (P8,TOTAL2),(14,PAMOUNT),2,CR=YES                                
         BAS   RE,PRNTXP                                                        
         ZAP   TOTAL2,=P'0'                                                     
*                                                                               
TOT500   DS    0H                                                               
         TM    TOTSW,TOT1                                                       
         BZ    TOTALX                                                           
         MVC   PTOTAL,=CL30'TOTAL FOR UNIT'                                     
         MVC   PTOTAL+15(4),PRUNIT                                              
         EDIT  (P8,TOTAL1),(14,PAMOUNT),2,CR=YES                                
         BAS   RE,PRNTXP                                                        
         ZAP   TOTAL1,=P'0'                                                     
         MVI   FORCEHED,C'Y'                                                    
*                                                                               
         TM    TOTSW,TOTRP                                                      
         BZ    TOTALX                                                           
         MVI   FORCEHED,C'N'                                                    
         MVC   PTOTAL,=CL30'TOTAL FOR REPORT'                                   
         EDIT  (P8,TOTREP),(14,PAMOUNT),2,CR=YES                                
         BAS   RE,PRNTXP                                                        
         BAS   RE,PRNTXP                                                        
*                                                                               
         MVC   PTOTAL,=CL30'TOTAL RECORDS TO TAPE:'                             
         EDIT  (P8,TAPECNT),(12,PAMOUNT)                                        
         BAS   RE,PRNTXP                                                        
*                                                                               
TOTALX   DS    0H                                                               
         CLI   FORCEHED,C'Y'                                                    
         BE    EXIT                DONT ADD EXTRA LINE IF HEADUP                
         BAS   RE,PRNTXP                                                        
         B     EXIT                                                             
         EJECT ,                                                                
******************************************************************              
*        PRINT REPORT                                            *              
******************************************************************              
*                                                                               
         USING PRNRECD,R4                                                       
         USING PLINED,R7                                                        
*                                                                               
REPORT   NTR1                                                                   
         LA    R4,PRNTREC                                                       
         LA    R7,XP                                                            
         MVC   XP,XSPACES                                                       
         MVC   CURUNIT,PRUNIT                                                   
         MVC   PSOURCE,PRSOURCE    JE NUMBER                                    
         EDIT  (1,PRTYPE),(3,PTYPE)                                             
         MVC   PACC,PRACC          MCKIM ACCT                                   
         MVC   PULACC,PRACC2       HDS ACCT                                     
         GOTO1 DATCON,DMCB,(0,PRTRNDTE),(6,PMOA)                                
         GOTO1 DATCON,DMCB,(0,PRTRNDTE),(5,PTRNDTE)                             
         MVC   PREMS,PRREMS                                                     
         MVC   PPROJCT,PRPROJCT                                                 
         EDIT  (P8,PRAMNTP),(14,PAMOUNT),2,CR=YES                               
*        CLC   PRACC2(2),=C'SE'    **HARDCODE FOR UL=SE**                       
*        BE    REP50                                                            
         OC    TOTSW,TOTSW                                                      
         BZ    REP100                                                           
*                                                                               
REP50    EDIT  (P8,PRTPTOT),(14,PTAPE),2,CR=YES                                 
*        CLC   PRACC2(2),=C'SE'    **HARDCODE FOR UL=SE**                       
*        BNE   REP100                                                           
*        ZAP   PRTPTOT,=P'0'                                                    
*                                                                               
REP100   BAS   RE,PRNTXP                                                        
         AP    TOTAL1,PRAMNTP                                                   
         AP    TOTAL2,PRAMNTP                                                   
         AP    TOTAL3,PRAMNTP                                                   
         AP    TOTAL4,PRAMNTP                                                   
         AP    TOTAL5,PRAMNTP                                                   
         AP    TOTAL6,PRAMNTP                                                   
         AP    TOTREP,PRAMNTP                                                   
         B     EXIT                                                             
         EJECT ,                                                                
******************************************************************              
*        PRINT OUT RECORD                                        *              
******************************************************************              
*                                                                               
         USING SRTRECD,R6                                                       
         USING PRNRECD,R4                                                       
*                                                                               
PRNTRC   NTR1                                                                   
         L     R6,ASRTREC                                                       
         LA    R4,PRNTREC                                                       
*                                                                               
         MVC   PRHDS,=C'HDS '                                                   
         MVC   PRUNIT,SRTUNIT      OFFICE                                       
         MVC   PRACC(4),SRTACC     ACCOUNT                                      
         MVC   PRTYPE,SRTTYPE                                                   
         MVC   PRACC2,SRTACC2                                                   
         GOTO1 DATCON,DMCB,(1,SRTTRDTE),(X'20',PRTRNDTE)                        
         MVC   PRPROJCT(6),SRTCLI                                               
         ZAP   DUB,SRTAMNT                                                      
         TM    SRTSTAT,X'80'       CHECK FOR -CR'S                              
         BNZ   *+10                                                             
         MP    DUB,=P'-1'          IF A CR THEN FLIP SIGN                       
         ZAP   PRAMNTP,DUB         SAVE AMOUNT IN PACKED FORMAT                 
         AP    PRTPTOT,DUB         ADD TO TAPE ACCUM                            
         UNPK  PRAMOUNT,DUB        TRANSACTION AMOUNT                           
         MVC   PRSOURCE,SRTSRCE                                                 
         MVC   PRREMS,SRTDESC      SAVE REMARK                                  
*                                                                               
         CLC   SRTACC2(2),=C'SG'   CRUNCH C/A CODE INTO NARRATIVE IF            
         BNE   PRN100              UL=SG                                        
         MVC   WRK2,SPACES                                                      
         MVC   WRK2(L'SRTCAC),SRTCAC                                            
         MVC   WRK2+20(30),SRTDESC                                              
         GOTO1 ADSQUASH,DMCB,WRK2,L'WRK2                                        
         MVC   PRREMS,WRK2                                                      
*                                                                               
PRN100   GOTO1 DATCON,DMCB,(5,PRENTDTE),(X'20',PRENTDTE)                        
         MVC   WORK,SRTENTP                                                     
         MVI   WORK+2,X'01'        SET  YYMM01                                  
         GOTO1 DATCON,DMCB,(1,WORK),(X'20',WORK+3)                              
         MVC   PRENTPER,WORK+3     SET  YYMM                                    
         MVC   PRTRNPER,WORK+3                                                  
         MVC   PRACTPER,=C'0000'                                                
         CLC   PRENTPER,PRTRNDTE   SEE IF MOA =TRNS MONTH                       
         BE    EXIT                                                             
*                                                                               
         MVC   WORK(4),PRENTPER    ELSE MAKE TRNS=LAST DAY OF MOA               
         MVC   WORK+4(1),=C'01'    CHARACTER YYMM01                             
         GOTO1 DATCON,DMCB,(X'30',WORK),(X'20',PRTRNDTE),(1,0)                  
         B     EXIT                                                             
         EJECT ,                                                                
******************************************************************              
*        INITIALIZE TAPE OUTPUT                                  *              
******************************************************************              
*                                                                               
         USING PRNRECD,R4                                                       
*                                                                               
INITTAPE NTR1                                                                   
         LA    RE,PRNTREC          CLEAR OUT BUFFER                             
         LH    RF,=Y(PRLNQ)                                                     
         LA    R0,XSPACES                                                       
         SR    R1,R1                                                            
         ICM   R1,8,XSPACES                                                     
         MVCL  RE,R0                                                            
         LA    R4,PRNTREC                                                       
         ZAP   PRTPTOT,=P'0'       INITIALIZE ACCUM                             
*                                                                               
         CLI   QOPT1,C'Y'                                                       
         BNE   EXIT                                                             
         GOTO1 DYNALLOC,DMCB,(0,=CL8'TAPE01'),DSPARM                            
         OPEN  (TAPE01,OUTPUT)                                                  
         B     EXIT                                                             
         EJECT ,                                                                
******************************************************************              
*        BUILD TAPE RECORD                                       *              
******************************************************************              
*                                                                               
         USING PRNRECD,R4                                                       
         USING TPRECD,R5                                                        
*                                                                               
BLDTAPE  NTR1                                                                   
         CLI   QOPT1,C'Y'          ONLY PRODUCE TAPE IF QOPT1=Y                 
         BNE   EXIT                                                             
         LA    R4,PRNTREC                                                       
         LA    R5,TAPEREC                                                       
*                                                                               
         LA    RE,TAPEREC          CLEAR OUT TAPE AREA                          
         LH    RF,=Y(TPLNQ)                                                     
         LA    R0,XSPACES                                                       
         SR    R1,R1                                                            
         ICM   R1,8,XSPACES                                                     
         MVCL  RE,R0                                                            
*                                                                               
         MVC   TPHDS,PRHDS         BUILD TAPE RECORD                            
         MVC   TPNUM,XSPACES                                                    
         MVC   TPBALPHA,XSPACES                                                 
         MVC   TPUNIT,PRUNIT                                                    
         MVC   TPACC,PRACC                                                      
         MVC   TPTRNDTE,PRTRNDTE                                                
         MVC   TPPROJCT,PRPROJCT                                                
         MVC   TPREF,XSPACES                                                    
         ZAP   DUB,PRTPTOT                                                      
         UNPK  TPAMOUNT,DUB        TOTAL TRANSACTION AMOUNT                     
         MVC   TPINTER,XSPACES                                                  
         MVC   TPREMS,PRREMS                                                    
         MVC   TPSOURCE,PRSOURCE                                                
         MVC   TPENTDTE,PRENTDTE                                                
         MVC   TPENTPER,PRENTPER                                                
         MVC   TPACTPER,PRACTPER                                                
         MVC   TPTRNPER,PRTRNDTE                                                
*                                                                               
         PUT   TAPE01,TAPEREC      PUT RECORD TO TAPE                           
         AP    TAPECNT,=P'1'                                                    
         B     EXIT                                                             
         EJECT ,                                                                
******************************************************************              
*        PRINT A LINE                                            *              
******************************************************************              
*                                                                               
PRNTXP   NTR1                                                                   
         MVC   XHEAD4+7(L'CURUNIT),CURUNIT                                      
         GOTO1 ACREPORT                                                         
         B     EXIT                                                             
         EJECT ,                                                                
******************************************************************              
*        CONSTANT DECLARATIONS                                   *              
******************************************************************              
*                                                                               
         GETEL R5,DATADISP,ELCODE                                               
*                                                                               
RECCARD  DC    C'RECORD TYPE=F,LENGTH=85 '                                      
SORTCARD DC    C'SORT FIELDS=(1,21,A),FORMAT=BI,WORK=1 '                        
*                                                                               
TAPE01   DCB   DSORG=PS,                                               X        
               MACRF=PM,                                               X        
               DDNAME=TAPE01,                                          X        
               RECFM=FB,                                               X        
               BLKSIZE=230,                                            X        
               LRECL=230                                                        
*                                                                               
DSPARM   DC    CL20'ACCTAPE.AC0IAMI1'                                           
         EJECT ,                                                                
******************************************************************              
*        EXTERNAL ADDRESS LIST                                   *              
******************************************************************              
*                                                                               
ADCONS   DS    0F                                                               
         DC    V(SORTER)                                                        
         DC    A(SORTREC)                                                       
         DC    A(SORTAREA)                                                      
         DC    A(IOAREA1)                                                       
         DC    A(IOAREA2)                                                       
         DC    A(LDGDEFN)                                                       
         DC    V(DATVAL)                                                        
         DC    X'FF'                                                            
         EJECT ,                                                                
******************************************************************              
*        LITERAL DECLARATIONS                                    *              
******************************************************************              
*                                                                               
ACCFIL   DC    CL8'ACCOUNT'                                                     
         SPACE 1                                                                
         LTORG                                                                  
         EJECT ,                                                                
******************************************************************              
*        BOX HOOK                                                *              
******************************************************************              
*                                                                               
         USING BOXD,R4                                                          
*                                                                               
BXHOOK   DS    0D                                                               
         NMOD1 0,*BHOOK                                                         
         L     RC,BOXRC            RESTORE REG C                                
         L     R4,ADBOX                                                         
         MVC   BOXCOLS(198),XSPACES                                             
         MVC   BOXROWS,SPACES                                                   
         CLI   MYSUBPRG,0                                                       
         BE    BOXX                                                             
         MVI   BOXROWS+6,C'T'            SET ROWS                               
         MVI   BOXROWS+9,C'M'                                                   
         MVI   BOXROWS+56,C'B'                                                  
         MVI   BOXCOLS,C'L'              SET LH MARGIN                          
         MVI   BOXCOLS+6,C'C'                                                   
         MVI   BOXCOLS+15,C'C'                                                  
         MVI   BOXCOLS+25,C'C'                                                  
         MVI   BOXCOLS+34,C'C'                                                  
         MVI   BOXCOLS+45,C'C'                                                  
         MVI   BOXCOLS+55,C'C'                                                  
         MVI   BOXCOLS+72,C'C'                                                  
         MVI   BOXCOLS+105,C'C'                                                 
         MVI   BOXCOLS+122,C'C'                                                 
         MVI   BOXCOLS+139,C'R'                                                 
         MVI   BOXYORN,C'Y'                                                     
         MVI   BOXWT,1                                                          
         MVI   BOXINIT,0                                                        
*                                                                               
BOXX     XMOD1 1                                                                
*                                                                               
BOXRC    DC    A(0)                                                             
*                                                                               
         LTORG                                                                  
         EJECT ,                                                                
******************************************************************              
*        EQUATES                                                 *              
******************************************************************              
*                                                                               
CR       EQU   X'40'               CREDIT                                       
DR       EQU   X'80'               DEBIT                                        
*                                                                               
*              CLIENT CODE EQUATES                                              
*                                                                               
ACCT     EQU   X'01'               GET CLIENT FROM ACCOUNT CODE                 
CNTR     EQU   X'02'               GET CLIENT FROM CONTRA ACCT CODE             
SJ24     EQU   X'04'               GET CLIENT FROM X'24' ELEM ON SJ REC         
*                                                                               
*              EXTRA STATUS EQUATES                                             
*                                                                               
ACCNAME  EQU   X'01'               MERGE ACCT NAME INTO DESCRIPTION             
CANAME   EQU   X'02'               MERGE C/A NAME INTO DESCRIPTION              
WRITEOFF EQU   X'04'               USE ACCT IN WO ELEM INSTEAD OF C/A           
SCCODE   EQU   X'08'               OFFICE=C'0'+1ST CHAR OF SC ACCT              
         EJECT ,                                                                
******************************************************************              
*              U/L=SB                                            *              
******************************************************************              
*                                                                               
LDGDEFN  DS    0F                                                               
         DC    CL14'SB031002025'                   ACCOUNT                      
         DC    AL1(12)                             L'ACCT TO MATCH              
         DC    AL1(CR)                             STATUS                       
         DC    AL1(45)                             DDS BATCH TYPE               
         DC    CL14'SQ'                            C/A                          
         DC    C'75'                               MCKIM JE NUMBER              
         DC    C'2025'                             MCKIM GL ACCOUNT             
         DC    AL1(0,0,0)                          CLIENT STATUS BYTES          
         DC    AL1(0)                              EXTRA STATUS                 
         DC    CL30'PAYABLES OFFSETS'              DESCRIPTION                  
*                                                                               
*                                                                               
         DC    CL14'SB031002025'                   ACCOUNT                      
         DC    AL1(12)                             L'ACCT TO MATCH              
         DC    AL1(DR+CR)                          STATUS                       
         DC    AL1(45)                             DDS BATCH TYPE               
         DC    CL14'ST'                            C/A                          
         DC    C'75'                               MCKIM JE NUMBER              
         DC    C'2025'                             MCKIM GL ACCOUNT             
         DC    AL1(0,0,0)                          CLIENT STATUS BYTES          
         DC    AL1(0)                              EXTRA STATUS                 
         DC    CL30'PAYABLES OFFSETS'              DESCRIPTION                  
*                                                                               
*                                                                               
         DC    CL14'SB031002026'                   ACCOUNT                      
         DC    AL1(12)                             L'ACCT TO MATCH              
         DC    AL1(CR)                             STATUS                       
         DC    AL1(45)                             DDS BATCH TYPE               
         DC    CL14'SP'                            C/A                          
         DC    C'75'                               MCKIM JE NUMBER              
         DC    C'2026'                             MCKIM GL ACCOUNT             
         DC    AL1(0,0,0)                          CLIENT STATUS BYTES          
         DC    AL1(0)                              EXTRA STATUS                 
         DC    CL30'PAYABLES OFFSETS'              DESCRIPTION                  
*                                                                               
*                                                                               
         DC    CL14'SB031002026'                   ACCOUNT                      
         DC    AL1(12)                             L'ACCT TO MATCH              
         DC    AL1(DR+CR)                          STATUS                       
         DC    AL1(45)                             DDS BATCH TYPE               
         DC    CL14'SS'                            C/A                          
         DC    C'75'                               MCKIM JE NUMBER              
         DC    C'2026'                             MCKIM GL ACCOUNT             
         DC    AL1(0,0,0)                          CLIENT STATUS BYTES          
         DC    AL1(0)                              EXTRA STATUS                 
         DC    CL30'PAYABLES OFFSETS'              DESCRIPTION                  
         EJECT ,                                                                
******************************************************************              
*              U/L=SC                                            *              
******************************************************************              
*                                                                               
         DC    CL14'SCM2610AC'                     ACCOUNT                      
         DC    AL1(12)                             L'ACCT TO MATCH              
         DC    AL1(CR)                             STATUS                       
         DC    AL1(3)                              DDS BATCH TYPE               
         DC    CL14'SJ'                            C/A                          
         DC    C'47'                               MCKIM JE NUMBER              
         DC    C'    '                             MCKIM GL ACCOUNT             
         DC    AL1(0,0,0)                          CLIENT STATUS BYTES          
         DC    AL1(0)                              EXTRA STATUS                 
         DC    CL30'PST ALLOCATION'                DESCRIPTION                  
*                                                                               
*                                                                               
         DC    CL14'SCM2610AT'                     ACCOUNT                      
         DC    AL1(12)                             L'ACCT TO MATCH              
         DC    AL1(CR)                             STATUS                       
         DC    AL1(3)                              DDS BATCH TYPE               
         DC    CL14'SJ'                            C/A                          
         DC    C'47'                               MCKIM JE NUMBER              
         DC    C'    '                             MCKIM GL ACCOUNT             
         DC    AL1(0,0,0)                          CLIENT STATUS BYTES          
         DC    AL1(0)                              EXTRA STATUS                 
         DC    CL30'PST ALLOCATION'                DESCRIPTION                  
*                                                                               
*                                                                               
         DC    CL14'SCM2610AV'                     ACCOUNT                      
         DC    AL1(12)                             L'ACCT TO MATCH              
         DC    AL1(CR)                             STATUS                       
         DC    AL1(3)                              DDS BATCH TYPE               
         DC    CL14'SJ'                            C/A                          
         DC    C'47'                               MCKIM JE NUMBER              
         DC    C'    '                             MCKIM GL ACCOUNT             
         DC    AL1(0,0,0)                          CLIENT STATUS BYTES          
         DC    AL1(0)                              EXTRA STATUS                 
         DC    CL30'PST ALLOCATION'                DESCRIPTION                  
*                                                                               
*                                                                               
         DC    CL14'SCM2610AW'                     ACCOUNT                      
         DC    AL1(12)                             L'ACCT TO MATCH              
         DC    AL1(CR)                             STATUS                       
         DC    AL1(3)                              DDS BATCH TYPE               
         DC    CL14'SJ'                            C/A                          
         DC    C'47'                               MCKIM JE NUMBER              
         DC    C'    '                             MCKIM GL ACCOUNT             
         DC    AL1(0,0,0)                          CLIENT STATUS BYTES          
         DC    AL1(0)                              EXTRA STATUS                 
         DC    CL30'PST ALLOCATION'                DESCRIPTION                  
*                                                                               
*                                                                               
         DC    CL14'SCM2610BC'                     ACCOUNT                      
         DC    AL1(12)                             L'ACCT TO MATCH              
         DC    AL1(CR)                             STATUS                       
         DC    AL1(3)                              DDS BATCH TYPE               
         DC    CL14'SJ'                            C/A                          
         DC    C'47'                               MCKIM JE NUMBER              
         DC    C'    '                             MCKIM GL ACCOUNT             
         DC    AL1(0,0,0)                          CLIENT STATUS BYTES          
         DC    AL1(0)                              EXTRA STATUS                 
         DC    CL30'PST ALLOCATION'                DESCRIPTION                  
*                                                                               
*                                                                               
         DC    CL14'SCM2610BP'                     ACCOUNT                      
         DC    AL1(12)                             L'ACCT TO MATCH              
         DC    AL1(CR)                             STATUS                       
         DC    AL1(3)                              DDS BATCH TYPE               
         DC    CL14'SJ'                            C/A                          
         DC    C'47'                               MCKIM JE NUMBER              
         DC    C'    '                             MCKIM GL ACCOUNT             
         DC    AL1(0,0,0)                          CLIENT STATUS BYTES          
         DC    AL1(0)                              EXTRA STATUS                 
         DC    CL30'PST ALLOCATION'                DESCRIPTION                  
*                                                                               
*                                                                               
         DC    CL14'SCM2610BT'                     ACCOUNT                      
         DC    AL1(12)                             L'ACCT TO MATCH              
         DC    AL1(CR)                             STATUS                       
         DC    AL1(3)                              DDS BATCH TYPE               
         DC    CL14'SJ'                            C/A                          
         DC    C'47'                               MCKIM JE NUMBER              
         DC    C'    '                             MCKIM GL ACCOUNT             
         DC    AL1(0,0,0)                          CLIENT STATUS BYTES          
         DC    AL1(0)                              EXTRA STATUS                 
         DC    CL30'PST ALLOCATION'                DESCRIPTION                  
*                                                                               
*                                                                               
         DC    CL14'SCM2610BV'                     ACCOUNT                      
         DC    AL1(12)                             L'ACCT TO MATCH              
         DC    AL1(CR)                             STATUS                       
         DC    AL1(3)                              DDS BATCH TYPE               
         DC    CL14'SJ'                            C/A                          
         DC    C'47'                               MCKIM JE NUMBER              
         DC    C'    '                             MCKIM GL ACCOUNT             
         DC    AL1(0,0,0)                          CLIENT STATUS BYTES          
         DC    AL1(0)                              EXTRA STATUS                 
         DC    CL30'PST ALLOCATION'                DESCRIPTION                  
*                                                                               
*                                                                               
         DC    CL14'SCM2610BW'                     ACCOUNT                      
         DC    AL1(12)                             L'ACCT TO MATCH              
         DC    AL1(CR)                             STATUS                       
         DC    AL1(3)                              DDS BATCH TYPE               
         DC    CL14'SJ'                            C/A                          
         DC    C'47'                               MCKIM JE NUMBER              
         DC    C'    '                             MCKIM GL ACCOUNT             
         DC    AL1(0,0,0)                          CLIENT STATUS BYTES          
         DC    AL1(0)                              EXTRA STATUS                 
         DC    CL30'PST ALLOCATION'                DESCRIPTION                  
*                                                                               
*                                                                               
         DC    CL14'SCM2610MC'                     ACCOUNT                      
         DC    AL1(12)                             L'ACCT TO MATCH              
         DC    AL1(CR)                             STATUS                       
         DC    AL1(3)                              DDS BATCH TYPE               
         DC    CL14'SJ'                            C/A                          
         DC    C'47'                               MCKIM JE NUMBER              
         DC    C'    '                             MCKIM GL ACCOUNT             
         DC    AL1(0,0,0)                          CLIENT STATUS BYTES          
         DC    AL1(0)                              EXTRA STATUS                 
         DC    CL30'PST ALLOCATION'                DESCRIPTION                  
*                                                                               
*                                                                               
         DC    CL14'SCM2610MP'                     ACCOUNT                      
         DC    AL1(12)                             L'ACCT TO MATCH              
         DC    AL1(CR)                             STATUS                       
         DC    AL1(3)                              DDS BATCH TYPE               
         DC    CL14'SJ'                            C/A                          
         DC    C'47'                               MCKIM JE NUMBER              
         DC    C'    '                             MCKIM GL ACCOUNT             
         DC    AL1(0,0,0)                          CLIENT STATUS BYTES          
         DC    AL1(0)                              EXTRA STATUS                 
         DC    CL30'PST ALLOCATION'                DESCRIPTION                  
*                                                                               
*                                                                               
         DC    CL14'SCM2610MT'                     ACCOUNT                      
         DC    AL1(12)                             L'ACCT TO MATCH              
         DC    AL1(CR)                             STATUS                       
         DC    AL1(3)                              DDS BATCH TYPE               
         DC    CL14'SJ'                            C/A                          
         DC    C'47'                               MCKIM JE NUMBER              
         DC    C'    '                             MCKIM GL ACCOUNT             
         DC    AL1(0,0,0)                          CLIENT STATUS BYTES          
         DC    AL1(0)                              EXTRA STATUS                 
         DC    CL30'PST ALLOCATION'                DESCRIPTION                  
*                                                                               
*                                                                               
         DC    CL14'SCM2610MV'                     ACCOUNT                      
         DC    AL1(12)                             L'ACCT TO MATCH              
         DC    AL1(CR)                             STATUS                       
         DC    AL1(3)                              DDS BATCH TYPE               
         DC    CL14'SJ'                            C/A                          
         DC    C'47'                               MCKIM JE NUMBER              
         DC    C'    '                             MCKIM GL ACCOUNT             
         DC    AL1(0,0,0)                          CLIENT STATUS BYTES          
         DC    AL1(0)                              EXTRA STATUS                 
         DC    CL30'PST ALLOCATION'                DESCRIPTION                  
*                                                                               
*                                                                               
         DC    CL14'SCM2610MW'                     ACCOUNT                      
         DC    AL1(12)                             L'ACCT TO MATCH              
         DC    AL1(CR)                             STATUS                       
         DC    AL1(3)                              DDS BATCH TYPE               
         DC    CL14'SJ'                            C/A                          
         DC    C'47'                               MCKIM JE NUMBER              
         DC    C'    '                             MCKIM GL ACCOUNT             
         DC    AL1(0,0,0)                          CLIENT STATUS BYTES          
         DC    AL1(0)                              EXTRA STATUS                 
         DC    CL30'PST ALLOCATION'                DESCRIPTION                  
*                                                                               
*                                                                               
         DC    CL14'SCM2610OC'                     ACCOUNT                      
         DC    AL1(12)                             L'ACCT TO MATCH              
         DC    AL1(CR)                             STATUS                       
         DC    AL1(3)                              DDS BATCH TYPE               
         DC    CL14'SJ'                            C/A                          
         DC    C'47'                               MCKIM JE NUMBER              
         DC    C'    '                             MCKIM GL ACCOUNT             
         DC    AL1(0,0,0)                          CLIENT STATUS BYTES          
         DC    AL1(0)                              EXTRA STATUS                 
         DC    CL30'PST ALLOCATION'                DESCRIPTION                  
*                                                                               
*                                                                               
         DC    CL14'SCM2610OP'                     ACCOUNT                      
         DC    AL1(12)                             L'ACCT TO MATCH              
         DC    AL1(CR)                             STATUS                       
         DC    AL1(3)                              DDS BATCH TYPE               
         DC    CL14'SJ'                            C/A                          
         DC    C'47'                               MCKIM JE NUMBER              
         DC    C'    '                             MCKIM GL ACCOUNT             
         DC    AL1(0,0,0)                          CLIENT STATUS BYTES          
         DC    AL1(0)                              EXTRA STATUS                 
         DC    CL30'PST ALLOCATION'                DESCRIPTION                  
*                                                                               
*                                                                               
         DC    CL14'SCM2610OT'                     ACCOUNT                      
         DC    AL1(12)                             L'ACCT TO MATCH              
         DC    AL1(CR)                             STATUS                       
         DC    AL1(3)                              DDS BATCH TYPE               
         DC    CL14'SJ'                            C/A                          
         DC    C'47'                               MCKIM JE NUMBER              
         DC    C'    '                             MCKIM GL ACCOUNT             
         DC    AL1(0,0,0)                          CLIENT STATUS BYTES          
         DC    AL1(0)                              EXTRA STATUS                 
         DC    CL30'PST ALLOCATION'                DESCRIPTION                  
*                                                                               
*                                                                               
         DC    CL14'SCM2610OV'                     ACCOUNT                      
         DC    AL1(12)                             L'ACCT TO MATCH              
         DC    AL1(CR)                             STATUS                       
         DC    AL1(3)                              DDS BATCH TYPE               
         DC    CL14'SJ'                            C/A                          
         DC    C'47'                               MCKIM JE NUMBER              
         DC    C'    '                             MCKIM GL ACCOUNT             
         DC    AL1(0,0,0)                          CLIENT STATUS BYTES          
         DC    AL1(0)                              EXTRA STATUS                 
         DC    CL30'PST ALLOCATION'                DESCRIPTION                  
*                                                                               
*                                                                               
         DC    CL14'SCM2610OW'                     ACCOUNT                      
         DC    AL1(12)                             L'ACCT TO MATCH              
         DC    AL1(CR)                             STATUS                       
         DC    AL1(3)                              DDS BATCH TYPE               
         DC    CL14'SJ'                            C/A                          
         DC    C'47'                               MCKIM JE NUMBER              
         DC    C'    '                             MCKIM GL ACCOUNT             
         DC    AL1(0,0,0)                          CLIENT STATUS BYTES          
         DC    AL1(0)                              EXTRA STATUS                 
         DC    CL30'PST ALLOCATION'                DESCRIPTION                  
*                                                                               
*                                                                               
         DC    CL14'SCM2610QC'                     ACCOUNT                      
         DC    AL1(12)                             L'ACCT TO MATCH              
         DC    AL1(CR)                             STATUS                       
         DC    AL1(3)                              DDS BATCH TYPE               
         DC    CL14'SJ'                            C/A                          
         DC    C'47'                               MCKIM JE NUMBER              
         DC    C'    '                             MCKIM GL ACCOUNT             
         DC    AL1(0,0,0)                          CLIENT STATUS BYTES          
         DC    AL1(0)                              EXTRA STATUS                 
         DC    CL30'PST ALLOCATION'                DESCRIPTION                  
*                                                                               
*                                                                               
         DC    CL14'SCM2610QT'                     ACCOUNT                      
         DC    AL1(12)                             L'ACCT TO MATCH              
         DC    AL1(CR)                             STATUS                       
         DC    AL1(3)                              DDS BATCH TYPE               
         DC    CL14'SJ'                            C/A                          
         DC    C'47'                               MCKIM JE NUMBER              
         DC    C'    '                             MCKIM GL ACCOUNT             
         DC    AL1(0,0,0)                          CLIENT STATUS BYTES          
         DC    AL1(0)                              EXTRA STATUS                 
         DC    CL30'PST ALLOCATION'                DESCRIPTION                  
*                                                                               
*                                                                               
         DC    CL14'SCM2610QV'                     ACCOUNT                      
         DC    AL1(12)                             L'ACCT TO MATCH              
         DC    AL1(CR)                             STATUS                       
         DC    AL1(3)                              DDS BATCH TYPE               
         DC    CL14'SJ'                            C/A                          
         DC    C'47'                               MCKIM JE NUMBER              
         DC    C'    '                             MCKIM GL ACCOUNT             
         DC    AL1(0,0,0)                          CLIENT STATUS BYTES          
         DC    AL1(0)                              EXTRA STATUS                 
         DC    CL30'PST ALLOCATION'                DESCRIPTION                  
*                                                                               
*                                                                               
         DC    CL14'SCM2610QW'                     ACCOUNT                      
         DC    AL1(12)                             L'ACCT TO MATCH              
         DC    AL1(CR)                             STATUS                       
         DC    AL1(3)                              DDS BATCH TYPE               
         DC    CL14'SJ'                            C/A                          
         DC    C'47'                               MCKIM JE NUMBER              
         DC    C'    '                             MCKIM GL ACCOUNT             
         DC    AL1(0,0,0)                          CLIENT STATUS BYTES          
         DC    AL1(0)                              EXTRA STATUS                 
         DC    CL30'PST ALLOCATION'                DESCRIPTION                  
*                                                                               
*                                                                               
         DC    CL14'SCL2610A'                      ACCOUNT                      
         DC    AL1(12)                             L'ACCT TO MATCH              
         DC    AL1(CR)                             STATUS                       
         DC    AL1(3)                              DDS BATCH TYPE               
         DC    CL14'SJ'                            C/A                          
         DC    C'47'                               MCKIM JE NUMBER              
         DC    C'    '                             MCKIM GL ACCOUNT             
         DC    AL1(0,0,0)                          CLIENT STATUS BYTES          
         DC    AL1(0)                              EXTRA STATUS                 
         DC    CL30'PST ALLOCATION'                DESCRIPTION                  
*                                                                               
*                                                                               
         DC    CL14'SCL2610B'                      ACCOUNT                      
         DC    AL1(12)                             L'ACCT TO MATCH              
         DC    AL1(CR)                             STATUS                       
         DC    AL1(3)                              DDS BATCH TYPE               
         DC    CL14'SJ'                            C/A                          
         DC    C'47'                               MCKIM JE NUMBER              
         DC    C'    '                             MCKIM GL ACCOUNT             
         DC    AL1(0,0,0)                          CLIENT STATUS BYTES          
         DC    AL1(0)                              EXTRA STATUS                 
         DC    CL30'PST ALLOCATION'                DESCRIPTION                  
*                                                                               
*                                                                               
         DC    CL14'SCL2610M'                      ACCOUNT                      
         DC    AL1(12)                             L'ACCT TO MATCH              
         DC    AL1(CR)                             STATUS                       
         DC    AL1(3)                              DDS BATCH TYPE               
         DC    CL14'SJ'                            C/A                          
         DC    C'47'                               MCKIM JE NUMBER              
         DC    C'    '                             MCKIM GL ACCOUNT             
         DC    AL1(0,0,0)                          CLIENT STATUS BYTES          
         DC    AL1(0)                              EXTRA STATUS                 
         DC    CL30'PST ALLOCATION'                DESCRIPTION                  
*                                                                               
*                                                                               
         DC    CL14'SCL2610O'                      ACCOUNT                      
         DC    AL1(12)                             L'ACCT TO MATCH              
         DC    AL1(CR)                             STATUS                       
         DC    AL1(3)                              DDS BATCH TYPE               
         DC    CL14'SJ'                            C/A                          
         DC    C'47'                               MCKIM JE NUMBER              
         DC    C'    '                             MCKIM GL ACCOUNT             
         DC    AL1(0,0,0)                          CLIENT STATUS BYTES          
         DC    AL1(0)                              EXTRA STATUS                 
         DC    CL30'PST ALLOCATION'                DESCRIPTION                  
*                                                                               
*                                                                               
         DC    CL14'SCL2610Q'                      ACCOUNT                      
         DC    AL1(12)                             L'ACCT TO MATCH              
         DC    AL1(CR)                             STATUS                       
         DC    AL1(3)                              DDS BATCH TYPE               
         DC    CL14'SJ'                            C/A                          
         DC    C'47'                               MCKIM JE NUMBER              
         DC    C'    '                             MCKIM GL ACCOUNT             
         DC    AL1(0,0,0)                          CLIENT STATUS BYTES          
         DC    AL1(0)                              EXTRA STATUS                 
         DC    CL30'PST ALLOCATION'                DESCRIPTION                  
*                                                                               
*                                                                               
         DC    CL14'SCS2610AE'                     ACCOUNT                      
         DC    AL1(12)                             L'ACCT TO MATCH              
         DC    AL1(CR)                             STATUS                       
         DC    AL1(3)                              DDS BATCH TYPE               
         DC    CL14'SJ'                            C/A                          
         DC    C'47'                               MCKIM JE NUMBER              
         DC    C'    '                             MCKIM GL ACCOUNT             
         DC    AL1(0,0,0)                          CLIENT STATUS BYTES          
         DC    AL1(0)                              EXTRA STATUS                 
         DC    CL30'PST ALLOCATION'                DESCRIPTION                  
*                                                                               
*                                                                               
         DC    CL14'SCS2610AF'                     ACCOUNT                      
         DC    AL1(12)                             L'ACCT TO MATCH              
         DC    AL1(CR)                             STATUS                       
         DC    AL1(3)                              DDS BATCH TYPE               
         DC    CL14'SJ'                            C/A                          
         DC    C'47'                               MCKIM JE NUMBER              
         DC    C'    '                             MCKIM GL ACCOUNT             
         DC    AL1(0,0,0)                          CLIENT STATUS BYTES          
         DC    AL1(0)                              EXTRA STATUS                 
         DC    CL30'PST ALLOCATION'                DESCRIPTION                  
*                                                                               
*                                                                               
         DC    CL14'SCS2610BE'                     ACCOUNT                      
         DC    AL1(12)                             L'ACCT TO MATCH              
         DC    AL1(CR)                             STATUS                       
         DC    AL1(3)                              DDS BATCH TYPE               
         DC    CL14'SJ'                            C/A                          
         DC    C'47'                               MCKIM JE NUMBER              
         DC    C'    '                             MCKIM GL ACCOUNT             
         DC    AL1(0,0,0)                          CLIENT STATUS BYTES          
         DC    AL1(0)                              EXTRA STATUS                 
         DC    CL30'PST ALLOCATION'                DESCRIPTION                  
*                                                                               
*                                                                               
         DC    CL14'SCS2610BF'                     ACCOUNT                      
         DC    AL1(12)                             L'ACCT TO MATCH              
         DC    AL1(CR)                             STATUS                       
         DC    AL1(3)                              DDS BATCH TYPE               
         DC    CL14'SJ'                            C/A                          
         DC    C'47'                               MCKIM JE NUMBER              
         DC    C'    '                             MCKIM GL ACCOUNT             
         DC    AL1(0,0,0)                          CLIENT STATUS BYTES          
         DC    AL1(0)                              EXTRA STATUS                 
         DC    CL30'PST ALLOCATION'                DESCRIPTION                  
*                                                                               
*                                                                               
         DC    CL14'SCS2610ME'                     ACCOUNT                      
         DC    AL1(12)                             L'ACCT TO MATCH              
         DC    AL1(CR)                             STATUS                       
         DC    AL1(3)                              DDS BATCH TYPE               
         DC    CL14'SJ'                            C/A                          
         DC    C'47'                               MCKIM JE NUMBER              
         DC    C'    '                             MCKIM GL ACCOUNT             
         DC    AL1(0,0,0)                          CLIENT STATUS BYTES          
         DC    AL1(0)                              EXTRA STATUS                 
         DC    CL30'PST ALLOCATION'                DESCRIPTION                  
*                                                                               
*                                                                               
         DC    CL14'SCS2610MF'                     ACCOUNT                      
         DC    AL1(12)                             L'ACCT TO MATCH              
         DC    AL1(CR)                             STATUS                       
         DC    AL1(3)                              DDS BATCH TYPE               
         DC    CL14'SJ'                            C/A                          
         DC    C'47'                               MCKIM JE NUMBER              
         DC    C'    '                             MCKIM GL ACCOUNT             
         DC    AL1(0,0,0)                          CLIENT STATUS BYTES          
         DC    AL1(0)                              EXTRA STATUS                 
         DC    CL30'PST ALLOCATION'                DESCRIPTION                  
*                                                                               
*                                                                               
         DC    CL14'SCS2610OE'                     ACCOUNT                      
         DC    AL1(12)                             L'ACCT TO MATCH              
         DC    AL1(CR)                             STATUS                       
         DC    AL1(3)                              DDS BATCH TYPE               
         DC    CL14'SJ'                            C/A                          
         DC    C'47'                               MCKIM JE NUMBER              
         DC    C'    '                             MCKIM GL ACCOUNT             
         DC    AL1(0,0,0)                          CLIENT STATUS BYTES          
         DC    AL1(0)                              EXTRA STATUS                 
         DC    CL30'PST ALLOCATION'                DESCRIPTION                  
*                                                                               
*                                                                               
         DC    CL14'SCS2610OF'                     ACCOUNT                      
         DC    AL1(12)                             L'ACCT TO MATCH              
         DC    AL1(CR)                             STATUS                       
         DC    AL1(3)                              DDS BATCH TYPE               
         DC    CL14'SJ'                            C/A                          
         DC    C'47'                               MCKIM JE NUMBER              
         DC    C'    '                             MCKIM GL ACCOUNT             
         DC    AL1(0,0,0)                          CLIENT STATUS BYTES          
         DC    AL1(0)                              EXTRA STATUS                 
         DC    CL30'PST ALLOCATION'                DESCRIPTION                  
*                                                                               
*                                                                               
         DC    CL14'SCS2610QE'                     ACCOUNT                      
         DC    AL1(12)                             L'ACCT TO MATCH              
         DC    AL1(CR)                             STATUS                       
         DC    AL1(3)                              DDS BATCH TYPE               
         DC    CL14'SJ'                            C/A                          
         DC    C'47'                               MCKIM JE NUMBER              
         DC    C'    '                             MCKIM GL ACCOUNT             
         DC    AL1(0,0,0)                          CLIENT STATUS BYTES          
         DC    AL1(0)                              EXTRA STATUS                 
         DC    CL30'PST ALLOCATION'                DESCRIPTION                  
*                                                                               
*                                                                               
         DC    CL14'SCS2610QF'                     ACCOUNT                      
         DC    AL1(12)                             L'ACCT TO MATCH              
         DC    AL1(CR)                             STATUS                       
         DC    AL1(3)                              DDS BATCH TYPE               
         DC    CL14'SJ'                            C/A                          
         DC    C'47'                               MCKIM JE NUMBER              
         DC    C'    '                             MCKIM GL ACCOUNT             
         DC    AL1(0,0,0)                          CLIENT STATUS BYTES          
         DC    AL1(0)                              EXTRA STATUS                 
         DC    CL30'PST ALLOCATION'                DESCRIPTION                  
*                                                                               
*                                                                               
         DC    CL14'SCU2610AN'                     ACCOUNT                      
         DC    AL1(12)                             L'ACCT TO MATCH              
         DC    AL1(CR)                             STATUS                       
         DC    AL1(3)                              DDS BATCH TYPE               
         DC    CL14'SJ'                            C/A                          
         DC    C'47'                               MCKIM JE NUMBER              
         DC    C'    '                             MCKIM GL ACCOUNT             
         DC    AL1(0,0,0)                          CLIENT STATUS BYTES          
         DC    AL1(0)                              EXTRA STATUS                 
         DC    CL30'PST ALLOCATION'                DESCRIPTION                  
*                                                                               
*                                                                               
         DC    CL14'SCU2610AG'                     ACCOUNT                      
         DC    AL1(12)                             L'ACCT TO MATCH              
         DC    AL1(CR)                             STATUS                       
         DC    AL1(3)                              DDS BATCH TYPE               
         DC    CL14'SJ'                            C/A                          
         DC    C'47'                               MCKIM JE NUMBER              
         DC    C'    '                             MCKIM GL ACCOUNT             
         DC    AL1(0,0,0)                          CLIENT STATUS BYTES          
         DC    AL1(0)                              EXTRA STATUS                 
         DC    CL30'PST ALLOCATION'                DESCRIPTION                  
*                                                                               
*                                                                               
         DC    CL14'SCU2610BN'                     ACCOUNT                      
         DC    AL1(12)                             L'ACCT TO MATCH              
         DC    AL1(CR)                             STATUS                       
         DC    AL1(3)                              DDS BATCH TYPE               
         DC    CL14'SJ'                            C/A                          
         DC    C'47'                               MCKIM JE NUMBER              
         DC    C'    '                             MCKIM GL ACCOUNT             
         DC    AL1(0,0,0)                          CLIENT STATUS BYTES          
         DC    AL1(0)                              EXTRA STATUS                 
         DC    CL30'PST ALLOCATION'                DESCRIPTION                  
*                                                                               
*                                                                               
         DC    CL14'SCU2610BG'                     ACCOUNT                      
         DC    AL1(12)                             L'ACCT TO MATCH              
         DC    AL1(CR)                             STATUS                       
         DC    AL1(3)                              DDS BATCH TYPE               
         DC    CL14'SJ'                            C/A                          
         DC    C'47'                               MCKIM JE NUMBER              
         DC    C'    '                             MCKIM GL ACCOUNT             
         DC    AL1(0,0,0)                          CLIENT STATUS BYTES          
         DC    AL1(0)                              EXTRA STATUS                 
         DC    CL30'PST ALLOCATION'                DESCRIPTION                  
*                                                                               
*                                                                               
         DC    CL14'SCU2610MN'                     ACCOUNT                      
         DC    AL1(12)                             L'ACCT TO MATCH              
         DC    AL1(CR)                             STATUS                       
         DC    AL1(3)                              DDS BATCH TYPE               
         DC    CL14'SJ'                            C/A                          
         DC    C'47'                               MCKIM JE NUMBER              
         DC    C'    '                             MCKIM GL ACCOUNT             
         DC    AL1(0,0,0)                          CLIENT STATUS BYTES          
         DC    AL1(0)                              EXTRA STATUS                 
         DC    CL30'PST ALLOCATION'                DESCRIPTION                  
*                                                                               
*                                                                               
         DC    CL14'SCU2610MG'                     ACCOUNT                      
         DC    AL1(12)                             L'ACCT TO MATCH              
         DC    AL1(CR)                             STATUS                       
         DC    AL1(3)                              DDS BATCH TYPE               
         DC    CL14'SJ'                            C/A                          
         DC    C'47'                               MCKIM JE NUMBER              
         DC    C'    '                             MCKIM GL ACCOUNT             
         DC    AL1(0,0,0)                          CLIENT STATUS BYTES          
         DC    AL1(0)                              EXTRA STATUS                 
         DC    CL30'PST ALLOCATION'                DESCRIPTION                  
*                                                                               
*                                                                               
         DC    CL14'SCU2610ON'                     ACCOUNT                      
         DC    AL1(12)                             L'ACCT TO MATCH              
         DC    AL1(CR)                             STATUS                       
         DC    AL1(3)                              DDS BATCH TYPE               
         DC    CL14'SJ'                            C/A                          
         DC    C'47'                               MCKIM JE NUMBER              
         DC    C'    '                             MCKIM GL ACCOUNT             
         DC    AL1(0,0,0)                          CLIENT STATUS BYTES          
         DC    AL1(0)                              EXTRA STATUS                 
         DC    CL30'PST ALLOCATION'                DESCRIPTION                  
*                                                                               
*                                                                               
         DC    CL14'SCU2610OG'                     ACCOUNT                      
         DC    AL1(12)                             L'ACCT TO MATCH              
         DC    AL1(CR)                             STATUS                       
         DC    AL1(3)                              DDS BATCH TYPE               
         DC    CL14'SJ'                            C/A                          
         DC    C'47'                               MCKIM JE NUMBER              
         DC    C'    '                             MCKIM GL ACCOUNT             
         DC    AL1(0,0,0)                          CLIENT STATUS BYTES          
         DC    AL1(0)                              EXTRA STATUS                 
         DC    CL30'PST ALLOCATION'                DESCRIPTION                  
*                                                                               
*                                                                               
         DC    CL14'SCU2610QN'                     ACCOUNT                      
         DC    AL1(12)                             L'ACCT TO MATCH              
         DC    AL1(CR)                             STATUS                       
         DC    AL1(3)                              DDS BATCH TYPE               
         DC    CL14'SJ'                            C/A                          
         DC    C'47'                               MCKIM JE NUMBER              
         DC    C'    '                             MCKIM GL ACCOUNT             
         DC    AL1(0,0,0)                          CLIENT STATUS BYTES          
         DC    AL1(0)                              EXTRA STATUS                 
         DC    CL30'PST ALLOCATION'                DESCRIPTION                  
*                                                                               
*                                                                               
         DC    CL14'SCU2610QG'                     ACCOUNT                      
         DC    AL1(12)                             L'ACCT TO MATCH              
         DC    AL1(CR)                             STATUS                       
         DC    AL1(3)                              DDS BATCH TYPE               
         DC    CL14'SJ'                            C/A                          
         DC    C'47'                               MCKIM JE NUMBER              
         DC    C'    '                             MCKIM GL ACCOUNT             
         DC    AL1(0,0,0)                          CLIENT STATUS BYTES          
         DC    AL1(0)                              EXTRA STATUS                 
         DC    CL30'PST ALLOCATION'                DESCRIPTION                  
*                                                                               
*                                                                               
         DC    CL14'SCV2610AR'                     ACCOUNT                      
         DC    AL1(12)                             L'ACCT TO MATCH              
         DC    AL1(CR)                             STATUS                       
         DC    AL1(3)                              DDS BATCH TYPE               
         DC    CL14'SJ'                            C/A                          
         DC    C'47'                               MCKIM JE NUMBER              
         DC    C'    '                             MCKIM GL ACCOUNT             
         DC    AL1(0,0,0)                          CLIENT STATUS BYTES          
         DC    AL1(0)                              EXTRA STATUS                 
         DC    CL30'PST ALLOCATION'                DESCRIPTION                  
*                                                                               
*                                                                               
         DC    CL14'SCV2610BR'                     ACCOUNT                      
         DC    AL1(12)                             L'ACCT TO MATCH              
         DC    AL1(CR)                             STATUS                       
         DC    AL1(3)                              DDS BATCH TYPE               
         DC    CL14'SJ'                            C/A                          
         DC    C'47'                               MCKIM JE NUMBER              
         DC    C'    '                             MCKIM GL ACCOUNT             
         DC    AL1(0,0,0)                          CLIENT STATUS BYTES          
         DC    AL1(0)                              EXTRA STATUS                 
         DC    CL30'PST ALLOCATION'                DESCRIPTION                  
*                                                                               
*                                                                               
         DC    CL14'SCV2610MR'                     ACCOUNT                      
         DC    AL1(12)                             L'ACCT TO MATCH              
         DC    AL1(CR)                             STATUS                       
         DC    AL1(3)                              DDS BATCH TYPE               
         DC    CL14'SJ'                            C/A                          
         DC    C'47'                               MCKIM JE NUMBER              
         DC    C'    '                             MCKIM GL ACCOUNT             
         DC    AL1(0,0,0)                          CLIENT STATUS BYTES          
         DC    AL1(0)                              EXTRA STATUS                 
         DC    CL30'PST ALLOCATION'                DESCRIPTION                  
*                                                                               
*                                                                               
         DC    CL14'SCV2610OR'                     ACCOUNT                      
         DC    AL1(12)                             L'ACCT TO MATCH              
         DC    AL1(CR)                             STATUS                       
         DC    AL1(3)                              DDS BATCH TYPE               
         DC    CL14'SJ'                            C/A                          
         DC    C'47'                               MCKIM JE NUMBER              
         DC    C'    '                             MCKIM GL ACCOUNT             
         DC    AL1(0,0,0)                          CLIENT STATUS BYTES          
         DC    AL1(0)                              EXTRA STATUS                 
         DC    CL30'PST ALLOCATION'                DESCRIPTION                  
*                                                                               
*                                                                               
         DC    CL14'SCV2610QR'                     ACCOUNT                      
         DC    AL1(12)                             L'ACCT TO MATCH              
         DC    AL1(CR)                             STATUS                       
         DC    AL1(3)                              DDS BATCH TYPE               
         DC    CL14'SJ'                            C/A                          
         DC    C'47'                               MCKIM JE NUMBER              
         DC    C'    '                             MCKIM GL ACCOUNT             
         DC    AL1(0,0,0)                          CLIENT STATUS BYTES          
         DC    AL1(0)                              EXTRA STATUS                 
         DC    CL30'PST ALLOCATION'                DESCRIPTION                  
*                                                                               
*                                                                               
         DC    CL14'SC'                            ACCOUNT                      
         DC    AL1(0)                              L'ACCT TO MATCH              
         DC    AL1(CR)                             STATUS                       
         DC    AL1(3)                              DDS BATCH TYPE               
         DC    CL14'SJ'                            C/A                          
         DC    C'46'                               MCKIM JE NUMBER              
         DC    C'    '                             MCKIM GL ACCOUNT             
         DC    AL1(0,0,0)                          CLIENT STATUS BYTES          
         DC    AL1(0)                              EXTRA STATUS                 
         DC    CL30'DOCKET CLEARING'               DESCRIPTION                  
*                                                                               
*                                                                               
         DC    CL14'SC'                            ACCOUNT                      
         DC    AL1(0)                              L'ACCT TO MATCH              
         DC    AL1(CR)                             STATUS                       
         DC    AL1(33)                             DDS BATCH TYPE               
         DC    CL14' '                             C/A                          
         DC    C'23'                               MCKIM JE NUMBER              
         DC    C'    '                             MCKIM GL ACCOUNT             
         DC    AL1(0,0,0)                          CLIENT STATUS BYTES          
         DC    AL1(0)                              EXTRA STATUS                 
         DC    CL30'CLEARANCES'                    DESCRIPTION                  
*                                                                               
*                                                                               
         DC    CL14'SC'                            ACCOUNT                      
         DC    AL1(0)                              L'ACCT TO MATCH              
         DC    AL1(CR)                             STATUS                       
         DC    AL1(34)                             DDS BATCH TYPE               
         DC    CL14' '                             C/A                          
         DC    C'23'                               MCKIM JE NUMBER              
         DC    C'    '                             MCKIM GL ACCOUNT             
         DC    AL1(0,0,0)                          CLIENT STATUS BYTES          
         DC    AL1(0)                              EXTRA STATUS                 
         DC    CL30'CLEARANCES'                    DESCRIPTION                  
*                                                                               
*                                                                               
         DC    CL14'SC'                            ACCOUNT                      
         DC    AL1(0)                              L'ACCT TO MATCH              
         DC    AL1(CR)                             STATUS                       
         DC    AL1(36)                             DDS BATCH TYPE               
         DC    CL14' '                             C/A                          
         DC    C'31'                               MCKIM JE NUMBER              
         DC    C'    '                             MCKIM GL ACCOUNT             
         DC    AL1(0,0,0)                          CLIENT STATUS BYTES          
         DC    AL1(0)                              EXTRA STATUS                 
         DC    CL30'MANUAL CHEQUE'                 DESCRIPTION                  
*                                                                               
*                                                                               
         DC    CL14'SC'                            ACCOUNT                      
         DC    AL1(0)                              L'ACCT TO MATCH              
         DC    AL1(DR+CR)                          STATUS                       
         DC    AL1(37)                             DDS BATCH TYPE               
         DC    CL14' '                             C/A                          
         DC    C'30'                               MCKIM JE NUMBER              
         DC    C'    '                             MCKIM GL ACCOUNT             
         DC    AL1(0,0,0)                          CLIENT STATUS BYTES          
         DC    AL1(SCCODE)                         EXTRA STATUS                 
         DC    CL30'CANCELLED CHEQUES'             DESCRIPTION                  
*                                                                               
*                                                                               
         DC    CL14'SCM2340'                       ACCOUNT                      
         DC    AL1(12)                             L'ACCT TO MATCH              
         DC    AL1(CR)                             STATUS                       
         DC    AL1(45)                             DDS BATCH TYPE               
         DC    CL14' '                             C/A                          
         DC    C'29'                               MCKIM JE NUMBER              
         DC    C'    '                             MCKIM GL ACCOUNT             
         DC    AL1(0,0,0)                          CLIENT STATUS BYTES          
         DC    AL1(0)                              EXTRA STATUS                 
         DC    CL30'CD LOST/REFUNDS'               DESCRIPTION                  
*                                                                               
*                                                                               
         DC    CL14'SCCC06M'                       ACCOUNT                      
         DC    AL1(12)                             L'ACCT TO MATCH              
         DC    AL1(CR)                             STATUS                       
         DC    AL1(45)                             DDS BATCH TYPE               
         DC    CL14'SCM2340'                       C/A                          
         DC    C'29'                               MCKIM JE NUMBER              
         DC    C'    '                             MCKIM GL ACCOUNT             
         DC    AL1(0,0,0)                          CLIENT STATUS BYTES          
         DC    AL1(0)                              EXTRA STATUS                 
         DC    CL30'CD LOST/REFUNDS'               DESCRIPTION                  
*                                                                               
*                                                                               
         DC    CL14'SCCC06N'                       ACCOUNT                      
         DC    AL1(12)                             L'ACCT TO MATCH              
         DC    AL1(CR)                             STATUS                       
         DC    AL1(45)                             DDS BATCH TYPE               
         DC    CL14'SCM2340'                       C/A                          
         DC    C'29'                               MCKIM JE NUMBER              
         DC    C'    '                             MCKIM GL ACCOUNT             
         DC    AL1(0,0,0)                          CLIENT STATUS BYTES          
         DC    AL1(0)                              EXTRA STATUS                 
         DC    CL30'CD LOST/REFUNDS'               DESCRIPTION                  
*                                                                               
*                                                                               
         DC    CL14'SCCC06O'                       ACCOUNT                      
         DC    AL1(12)                             L'ACCT TO MATCH              
         DC    AL1(CR)                             STATUS                       
         DC    AL1(45)                             DDS BATCH TYPE               
         DC    CL14'SCM2340'                       C/A                          
         DC    C'29'                               MCKIM JE NUMBER              
         DC    C'    '                             MCKIM GL ACCOUNT             
         DC    AL1(0,0,0)                          CLIENT STATUS BYTES          
         DC    AL1(0)                              EXTRA STATUS                 
         DC    CL30'CD LOST/REFUNDS'               DESCRIPTION                  
*                                                                               
*                                                                               
         DC    CL14'SC'                            ACCOUNT                      
         DC    AL1(0)                              L'ACCT TO MATCH              
         DC    AL1(CR)                             STATUS                       
         DC    AL1(45)                             DDS BATCH TYPE               
         DC    CL14'SQ'                            C/A                          
         DC    C'28'                               MCKIM JE NUMBER              
         DC    C'6830'                             MCKIM GL ACCOUNT             
         DC    AL1(0,0,0)                          CLIENT STATUS BYTES          
         DC    AL1(0)                              EXTRA STATUS                 
         DC    CL30'FOREIGN ADJUSTMENT'            DESCRIPTION                  
*                                                                               
*                                                                               
         DC    CL14'SC'                            ACCOUNT                      
         DC    AL1(0)                              L'ACCT TO MATCH              
         DC    AL1(CR)                             STATUS                       
         DC    AL1(49)                             DDS BATCH TYPE               
         DC    CL14' '                             C/A                          
         DC    C'27'                               MCKIM JE NUMBER              
         DC    C'    '                             MCKIM GL ACCOUNT             
         DC    AL1(0,0,0)                          CLIENT STATUS BYTES          
         DC    AL1(0)                              EXTRA STATUS                 
         DC    CL30'CD LOST/REFUNDS'               DESCRIPTION                  
*                                                                               
*                                                                               
         DC    CL14'SC'                            ACCOUNT                      
         DC    AL1(0)                              L'ACCT TO MATCH              
         DC    AL1(CR)                             STATUS                       
         DC    AL1(50)                             DDS BATCH TYPE               
         DC    CL14' '                             C/A                          
         DC    C'27'                               MCKIM JE NUMBER              
         DC    C'    '                             MCKIM GL ACCOUNT             
         DC    AL1(0,0,0)                          CLIENT STATUS BYTES          
         DC    AL1(0)                              EXTRA STATUS                 
         DC    CL30'CD LOST/REFUNDS'               DESCRIPTION                  
*                                                                               
*                                                                               
         DC    CL14'SC'                            ACCOUNT                      
         DC    AL1(0)                              L'ACCT TO MATCH              
         DC    AL1(DR)                             STATUS                       
         DC    AL1(0)                              DDS BATCH TYPE               
         DC    CL14' '                             C/A                          
         DC    C'21'                               MCKIM JE NUMBER              
         DC    C'    '                             MCKIM GL ACCOUNT             
         DC    AL1(0,0,0)                          CLIENT STATUS BYTES          
         DC    AL1(SCCODE)                         EXTRA STATUS                 
         DC    CL30'CASH RECEIPTS'                 DESCRIPTION                  
*                                                                               
*                                                                               
         DC    CL14'SC'                            ACCOUNT                      
         DC    AL1(0)                              L'ACCT TO MATCH              
         DC    AL1(CR)                             STATUS                       
         DC    AL1(0)                              DDS BATCH TYPE               
         DC    CL14' '                             C/A                          
         DC    C'22'                               MCKIM JE NUMBER              
         DC    C'    '                             MCKIM GL ACCOUNT             
         DC    AL1(0,0,0)                          CLIENT STATUS BYTES          
         DC    AL1(SCCODE)                         EXTRA STATUS                 
         DC    CL30'CHEQUE PAYMENTS'               DESCRIPTION                  
         EJECT ,                                                                
******************************************************************              
*              U/L=SE                                            *              
******************************************************************              
*                                                                               
         DC    CL14'SE'                            ACCOUNT                      
         DC    AL1(0)                              L'ACCT TO MATCH              
         DC    AL1(DR)                             STATUS                       
         DC    AL1(14)                             DDS BATCH TYPE               
         DC    CL14'SJ'                            C/A                          
         DC    C'49'                               MCKIM JE NUMBER              
         DC    C'    '                             MCKIM GL ACCOUNT             
         DC    AL1(SJ24,0,0)                       CLIENT STATUS BYTES          
         DC    AL1(ACCNAME)                        EXTRA STATUS                 
         DC    CL30'               - DOCKET TO EXP'    DESCRIPTION              
*                                                                               
*                                                                               
         DC    CL14'SE'                            ACCOUNT                      
         DC    AL1(0)                              L'ACCT TO MATCH              
         DC    AL1(DR)                             STATUS                       
         DC    AL1(15)                             DDS BATCH TYPE               
         DC    CL14'SJ'                            C/A                          
         DC    C'48'                               MCKIM JE NUMBER              
         DC    C'    '                             MCKIM GL ACCOUNT             
         DC    AL1(SJ24,0,0)                       CLIENT STATUS BYTES          
         DC    AL1(ACCNAME)                        EXTRA STATUS                 
         DC    CL30'               - EXP TO DOCKET'    DESCRIPTION              
*                                                                               
*                                                                               
*                                                                               
         DC    CL14'SE094006789'                   ACCOUNT                      
         DC    AL1(12)                             L'ACCT TO MATCH              
         DC    AL1(CR)                             STATUS                       
         DC    AL1(20)                             DDS BATCH TYPE               
         DC    CL14'SR'                            C/A                          
         DC    C'53'                               MCKIM JE NUMBER              
         DC    C'6789'                             MCKIM GL ACCOUNT             
         DC    AL1(CNTR,2,6)                       CLIENT STATUS BYTES          
         DC    AL1(0)                              EXTRA STATUS                 
         DC    CL30'FOCUS REC''B BAL. FWD.'        DESCRIPTION                  
*                                                                               
*                                                                               
         DC    CL14'SE083505740'                   ACCOUNT                      
         DC    AL1(12)                             L'ACCT TO MATCH              
         DC    AL1(CR)                             STATUS                       
         DC    AL1(26)                             DDS BATCH TYPE               
         DC    CL14' '                             C/A                          
         DC    C'6 '                               MCKIM JE NUMBER              
         DC    C'5740'                             MCKIM GL ACCOUNT             
         DC    AL1(SJ24,0,0)                       CLIENT STATUS BYTES          
         DC    AL1(0)                              EXTRA STATUS                 
         DC    CL30'ACCTS RCVBLE W/O ADJ.'         DESCRIPTION                  
*                                                                               
*                                                                               
         DC    CL14'SE083956880'                   ACCOUNT                      
         DC    AL1(12)                             L'ACCT TO MATCH              
         DC    AL1(CR)                             STATUS                       
         DC    AL1(26)                             DDS BATCH TYPE               
         DC    CL14' '                             C/A                          
         DC    C'35'                               MCKIM JE NUMBER              
         DC    C'6880'                             MCKIM GL ACCOUNT             
         DC    AL1(SJ24,0,0)                       CLIENT STATUS BYTES          
         DC    AL1(0)                              EXTRA STATUS                 
         DC    CL30'BAD DEBT W/O'                  DESCRIPTION                  
*                                                                               
*                                                                               
         DC    CL14'SE1220'                        ACCOUNT                      
         DC    AL1(12)                             L'ACCT TO MATCH              
         DC    AL1(DR)                             STATUS                       
         DC    AL1(30)                             DDS BATCH TYPE               
         DC    CL14'SR'                            C/A                          
         DC    C'53'                               MCKIM JE NUMBER              
         DC    C'1220'                             MCKIM GL ACCOUNT             
         DC    AL1(CNTR,2,6)                       CLIENT STATUS BYTES          
         DC    AL1(0)                              EXTRA STATUS                 
         DC    CL30'AR ADJUSTMENTS'                DESCRIPTION                  
*                                                                               
*                                                                               
         DC    CL14'SE094006812'                   ACCOUNT                      
         DC    AL1(12)                             L'ACCT TO MATCH              
         DC    AL1(DR)                             STATUS                       
         DC    AL1(30)                             DDS BATCH TYPE               
         DC    CL14'SR'                            C/A                          
         DC    C'26'                               MCKIM JE NUMBER              
         DC    C'1245'                             MCKIM GL ACCOUNT             
         DC    AL1(CNTR,2,6)                       CLIENT STATUS BYTES          
         DC    AL1(0)                              EXTRA STATUS                 
         DC    CL30'WRITE-OFF I/CO BILLING - WCD'  DESCRIPTION                  
*                                                                               
*                                                                               
         DC    CL14'SE094006818'                   ACCOUNT                      
         DC    AL1(12)                             L'ACCT TO MATCH              
         DC    AL1(DR+CR)                          STATUS                       
         DC    AL1(30)                             DDS BATCH TYPE               
         DC    CL14'SR'                            C/A                          
         DC    C'26'                               MCKIM JE NUMBER              
         DC    C'6818'                             MCKIM GL ACCOUNT             
         DC    AL1(CNTR,2,6)                       CLIENT STATUS BYTES          
         DC    AL1(0)                              EXTRA STATUS                 
         DC    CL30'AR SMALL BAL WRITE-OFFS'       DESCRIPTION                  
*                                                                               
         DC    CL14'SE094006818'                   ACCOUNT                      
         DC    AL1(12)                             L'ACCT TO MATCH              
         DC    AL1(DR+CR)                          STATUS                       
         DC    AL1(45)                             DDS BATCH TYPE               
         DC    CL14'SR'                            C/A                          
         DC    C'6 '                               MCKIM JE NUMBER              
         DC    C'6818'                             MCKIM GL ACCOUNT             
         DC    AL1(CNTR,2,6)                       CLIENT STATUS BYTES          
         DC    AL1(0)                              EXTRA STATUS                 
         DC    CL30'ACCT RCVBLE W/O ADJ'           DESCRIPTION                  
*                                                                               
*                                                                               
         DC    CL14'SE094006830'                   ACCOUNT                      
         DC    AL1(12)                             L'ACCT TO MATCH              
         DC    AL1(DR+CR)                          STATUS                       
         DC    AL1(30)                             DDS BATCH TYPE               
         DC    CL14'SR'                            C/A                          
         DC    C'28'                               MCKIM JE NUMBER              
         DC    C'6830'                             MCKIM GL ACCOUNT             
         DC    AL1(CNTR,2,6)                       CLIENT STATUS BYTES          
         DC    AL1(0)                              EXTRA STATUS                 
         DC    CL30'FOREIGN EXCH. ADJUSTMENT'      DESCRIPTION                  
*                                                                               
*                                                                               
         DC    CL14'SE094006830'                   ACCOUNT                      
         DC    AL1(12)                             L'ACCT TO MATCH              
         DC    AL1(DR+CR)                          STATUS                       
         DC    AL1(45)                             DDS BATCH TYPE               
         DC    CL14'ST'                            C/A                          
         DC    C'28'                               MCKIM JE NUMBER              
         DC    C'6830'                             MCKIM GL ACCOUNT             
         DC    AL1(0,0,0)                          CLIENT STATUS BYTES          
         DC    AL1(0)                              EXTRA STATUS                 
         DC    CL30'FOREIGN ADJUSTMENT'            DESCRIPTION                  
*                                                                               
*                                                                               
         DC    CL14'SE094006840'                   ACCOUNT                      
         DC    AL1(12)                             L'ACCT TO MATCH              
         DC    AL1(DR+CR)                          STATUS                       
         DC    AL1(30)                             DDS BATCH TYPE               
         DC    CL14'SR'                            C/A                          
         DC    C'28'                               MCKIM JE NUMBER              
         DC    C'6830'                             MCKIM GL ACCOUNT             
         DC    AL1(CNTR,2,6)                       CLIENT STATUS BYTES          
         DC    AL1(0)                              EXTRA STATUS                 
         DC    CL30'FOREIGN EXCH. ADJUSTMENT'      DESCRIPTION                  
*                                                                               
*                                                                               
         DC    CL14'SE083956880'                   ACCOUNT                      
         DC    AL1(12)                             L'ACCT TO MATCH              
         DC    AL1(CR)                             STATUS                       
         DC    AL1(26)                             DDS BATCH TYPE               
         DC    CL14' '                             C/A                          
         DC    C'35'                               MCKIM JE NUMBER              
         DC    C'6880'                             MCKIM GL ACCOUNT             
         DC    AL1(SJ24,0,0)                       CLIENT STATUS BYTES          
         DC    AL1(0)                              EXTRA STATUS                 
         DC    CL30'BAD DEBT W/O'                  DESCRIPTION                  
*                                                                               
*                                                                               
         DC    CL14'SE094006789'                   ACCOUNT                      
         DC    AL1(12)                             L'ACCT TO MATCH              
         DC    AL1(DR+CR)                          STATUS                       
         DC    AL1(30)                             DDS BATCH TYPE               
         DC    CL14'SR'                            C/A                          
         DC    C'53'                               MCKIM JE NUMBER              
         DC    C'6789'                             MCKIM GL ACCOUNT             
         DC    AL1(CNTR,2,6)                       CLIENT STATUS BYTES          
         DC    AL1(0)                              EXTRA STATUS                 
         DC    CL30'RCVBLE TRANSFER-FROM ACCT.'    DESCRIPTION                  
*                                                                               
*                                                                               
         DC    CL14'SE094006789'                   ACCOUNT                      
         DC    AL1(12)                             L'ACCT TO MATCH              
         DC    AL1(CR)                             STATUS                       
         DC    AL1(26)                             DDS BATCH TYPE               
         DC    CL14'SJ'                            C/A                          
         DC    C'53'                               MCKIM JE NUMBER              
         DC    C'6789'                             MCKIM GL ACCOUNT             
         DC    AL1(0,0,0)                          CLIENT STATUS BYTES          
         DC    AL1(0)                              EXTRA STATUS                 
         DC    CL30'RCVBLE TRANSFER-TO ACCT.'      DESCRIPTION                  
*                                                                               
*                                                                               
         DC    CL14'SE094006789'                   ACCOUNT                      
         DC    AL1(12)                             L'ACCT TO MATCH              
         DC    AL1(CR)                             STATUS                       
         DC    AL1(45)                             DDS BATCH TYPE               
         DC    CL14'SR'                            C/A                          
         DC    C'53'                               MCKIM JE NUMBER              
         DC    C'6789'                             MCKIM GL ACCOUNT             
         DC    AL1(CNTR,2,6)                       CLIENT STATUS BYTES          
         DC    AL1(0)                              EXTRA STATUS                 
         DC    CL30'FINANSYS RCVBLE B/F'           DESCRIPTION                  
*                                                                               
*                                                                               
         DC    CL14'SE083956880'                   ACCOUNT                      
         DC    AL1(12)                             L'ACCT TO MATCH              
         DC    AL1(DR+CR)                          STATUS                       
         DC    AL1(30)                             DDS BATCH TYPE               
         DC    CL14'SR'                            C/A                          
         DC    C'35'                               MCKIM JE NUMBER              
         DC    C'6880'                             MCKIM GL ACCOUNT             
         DC    AL1(CNTR,2,6)                       CLIENT STATUS BYTES          
         DC    AL1(0)                              EXTRA STATUS                 
         DC    CL30'BAD DEBTS'                     DESCRIPTION                  
*                                                                               
*                                                                               
         DC    CL14'SE094006830'                   ACCOUNT                      
         DC    AL1(12)                             L'ACCT TO MATCH              
         DC    AL1(DR+CR)                          STATUS                       
         DC    AL1(45)                             DDS BATCH TYPE               
         DC    CL14'SS'                            C/A                          
         DC    C'28'                               MCKIM JE NUMBER              
         DC    C'6830'                             MCKIM GL ACCOUNT             
         DC    AL1(0,0,0)                          CLIENT STATUS BYTES          
         DC    AL1(0)                              EXTRA STATUS                 
         DC    CL30'FOREIGN ADJUSTMENT'            DESCRIPTION                  
*                                                                               
*                                                                               
         DC    CL14'SE094006830'                   ACCOUNT                      
         DC    AL1(12)                             L'ACCT TO MATCH              
         DC    AL1(DR+CR)                          STATUS                       
         DC    AL1(45)                             DDS BATCH TYPE               
         DC    CL14'SP'                            C/A                          
         DC    C'28'                               MCKIM JE NUMBER              
         DC    C'6830'                             MCKIM GL ACCOUNT             
         DC    AL1(0,0,0)                          CLIENT STATUS BYTES          
         DC    AL1(0)                              EXTRA STATUS                 
         DC    CL30'FOREIGN ADJUSTMENT'            DESCRIPTION                  
*                                                                               
*                                                                               
         DC    CL14'SE094006830'                   ACCOUNT                      
         DC    AL1(12)                             L'ACCT TO MATCH              
         DC    AL1(DR+CR)                          STATUS                       
         DC    AL1(45)                             DDS BATCH TYPE               
         DC    CL14'SQ'                            C/A                          
         DC    C'28'                               MCKIM JE NUMBER              
         DC    C'6830'                             MCKIM GL ACCOUNT             
         DC    AL1(0,0,0)                          CLIENT STATUS BYTES          
         DC    AL1(0)                              EXTRA STATUS                 
         DC    CL30'FOREIGN ADJUSTMENT'            DESCRIPTION                  
*                                                                               
*                                                                               
         DC    CL14'SE094006850'                   ACCOUNT                      
         DC    AL1(12)                             L'ACCT TO MATCH              
         DC    AL1(CR)                             STATUS                       
         DC    AL1(45)                             DDS BATCH TYPE               
         DC    CL14'SR'                            C/A                          
         DC    C'29'                               MCKIM JE NUMBER              
         DC    C'6850'                             MCKIM GL ACCOUNT             
         DC    AL1(CNTR,2,6)                       CLIENT STATUS BYTES          
         DC    AL1(0)                              EXTRA STATUS                 
         DC    CL30'CD EARNED/LOST'                DESCRIPTION                  
*                                                                               
*                                                                               
         DC    CL14'SE094006855'                   ACCOUNT                      
         DC    AL1(12)                             L'ACCT TO MATCH              
         DC    AL1(CR)                             STATUS                       
         DC    AL1(45)                             DDS BATCH TYPE               
         DC    CL14'SR'                            C/A                          
         DC    C'29'                               MCKIM JE NUMBER              
         DC    C'6855'                             MCKIM GL ACCOUNT             
         DC    AL1(CNTR,2,6)                       CLIENT STATUS BYTES          
         DC    AL1(0)                              EXTRA STATUS                 
         DC    CL30'CD ALLOWED'                    DESCRIPTION                  
*                                                                               
*                                                                               
         DC    CL14'SE6853'                        ACCOUNT                      
         DC    AL1(12)                             L'ACCT TO MATCH              
         DC    AL1(CR)                             STATUS                       
         DC    AL1(45)                             DDS BATCH TYPE               
         DC    CL14'SR'                            C/A                          
         DC    C'29'                               MCKIM JE NUMBER              
         DC    C'6850'                             MCKIM GL ACCOUNT             
         DC    AL1(CNTR,2,6)                       CLIENT STATUS BYTES          
         DC    AL1(0)                              EXTRA STATUS                 
         DC    CL30'CD LOST'                       DESCRIPTION                  
*                                                                               
*                                                                               
         DC    CL14'SE094006812'                   ACCOUNT                      
         DC    AL1(12)                             L'ACCT TO MATCH              
         DC    AL1(CR)                             STATUS                       
         DC    AL1(45)                             DDS BATCH TYPE               
         DC    CL14'SR'                            C/A                          
         DC    C'6 '                               MCKIM JE NUMBER              
         DC    C'1245'                             MCKIM GL ACCOUNT             
         DC    AL1(CNTR,2,6)                       CLIENT STATUS BYTES          
         DC    AL1(0)                              EXTRA STATUS                 
         DC    CL30'ACCTS RCVBLE W/O ADJ.'         DESCRIPTION                  
         EJECT ,                                                                
******************************************************************              
*              U/L=SF                                            *              
******************************************************************              
*                                                                               
         DC    CL14'SF'                            ACCOUNT                      
         DC    AL1(0)                              L'ACCT TO MATCH              
         DC    AL1(DR+CR)                          STATUS                       
         DC    AL1(45)                             DDS BATCH TYPE               
         DC    CL14' '                             C/A                          
         DC    C'6 '                               MCKIM JE NUMBER              
         DC    C'    '                             MCKIM GL ACCOUNT             
         DC    AL1(0,0,0)                          CLIENT STATUS BYTES          
         DC    AL1(0)                              EXTRA STATUS                 
         DC    CL30'ACCTS RCVBLE WRITE-OFF ADJ.'   DESCRIPTION                  
*                                                                               
*                                                                               
         DC    CL14'SF'                            ACCOUNT                      
         DC    AL1(0)                              L'ACCT TO MATCH              
         DC    AL1(DR+CR)                          STATUS                       
         DC    AL1(0)                              DDS BATCH TYPE               
         DC    CL14' '                             C/A                          
         DC    C'6 '                               MCKIM JE NUMBER              
         DC    C'1299'                             MCKIM GL ACCOUNT             
         DC    AL1(0,0,0)                          CLIENT STATUS BYTES          
         DC    AL1(0)                              EXTRA STATUS                 
         DC    CL30'ACCTS RCVBLE WRITE-OFF ADJ.'   DESCRIPTION                  
         EJECT ,                                                                
******************************************************************              
*              U/L=SG                                            *              
******************************************************************              
*                                                                               
         DC    CL14'SG'                            ACCOUNT                      
         DC    AL1(0)                              L'ACCT TO MATCH              
         DC    AL1(DR)                             STATUS                       
         DC    AL1(1)                              DDS BATCH TYPE               
         DC    CL14' '                             C/A                          
         DC    C'45'                               MCKIM JE NUMBER              
         DC    C'    '                             MCKIM GL ACCOUNT             
         DC    AL1(0,0,0)                          CLIENT STATUS BYTES          
         DC    AL1(0)                              EXTRA STATUS                 
         DC    CL30'PRODUCTION ALLOCATION'         DESCRIPTION                  
*                                                                               
*                                                                               
         DC    CL14'SG'                            ACCOUNT                      
         DC    AL1(0)                              L'ACCT TO MATCH              
         DC    AL1(DR)                             STATUS                       
         DC    AL1(3)                              DDS BATCH TYPE               
         DC    CL14' '                             C/A                          
         DC    C'46'                               MCKIM JE NUMBER              
         DC    C'    '                             MCKIM GL ACCOUNT             
         DC    AL1(0,0,0)                          CLIENT STATUS BYTES          
         DC    AL1(0)                              EXTRA STATUS                 
         DC    CL30'DOCKET CLEARING'               DESCRIPTION                  
*                                                                               
*                                                                               
         DC    CL14'SG'                            ACCOUNT                      
         DC    AL1(0)                              L'ACCT TO MATCH              
         DC    AL1(DR)                             STATUS                       
         DC    AL1(9)                              DDS BATCH TYPE               
         DC    CL14' '                             C/A                          
         DC    C'24'                               MCKIM JE NUMBER              
         DC    C'    '                             MCKIM GL ACCOUNT             
         DC    AL1(0,0,0)                          CLIENT STATUS BYTES          
         DC    AL1(0)                              EXTRA STATUS                 
         DC    CL30'FINAL BILLING (ACCTS RCVBLE)'  DESCRIPTION                  
*                                                                               
*                                                                               
         DC    CL14'SG'                            ACCOUNT                      
         DC    AL1(0)                              L'ACCT TO MATCH              
         DC    AL1(CR)                             STATUS                       
         DC    AL1(26)                             DDS BATCH TYPE               
         DC    CL14'SJHOECOR'                      C/A                          
         DC    C'6 '                               MCKIM JE NUMBER              
         DC    C'    '                             MCKIM GL ACCOUNT             
         DC    AL1(0,0,0)                          CLIENT STATUS BYTES          
         DC    AL1(0)                              EXTRA STATUS                 
         DC    CL30'ACCTS RCVBLE W/O ADJ.'         DESCRIPTION                  
*                                                                               
*                                                                               
         DC    CL14'SG'                            ACCOUNT                      
         DC    AL1(0)                              L'ACCT TO MATCH              
         DC    AL1(DR)                             STATUS                       
         DC    AL1(33)                             DDS BATCH TYPE               
         DC    CL14'SV'                            C/A                          
         DC    C'45'                               MCKIM JE NUMBER              
         DC    C'    '                             MCKIM GL ACCOUNT             
         DC    AL1(0,0,0)                          CLIENT STATUS BYTES          
         DC    AL1(0)                              EXTRA STATUS                 
         DC    CL30'PRODUCTION ALLOCATION'         DESCRIPTION                  
*                                                                               
*                                                                               
         DC    CL14'SG'                            ACCOUNT                      
         DC    AL1(0)                              L'ACCT TO MATCH              
         DC    AL1(CR)                             STATUS                       
         DC    AL1(26)                             DDS BATCH TYPE               
         DC    CL14' '                             C/A                          
         DC    C'35'                               MCKIM JE NUMBER              
         DC    C'    '                             MCKIM GL ACCOUNT             
         DC    AL1(0,0,0)                          CLIENT STATUS BYTES          
         DC    AL1(0)                              EXTRA STATUS                 
         DC    CL30'BAD DEBT W/O'                  DESCRIPTION                  
*                                                                               
*                                                                               
         DC    CL14'SG'                            ACCOUNT                      
         DC    AL1(0)                              L'ACCT TO MATCH              
         DC    AL1(DR)                             STATUS                       
         DC    AL1(49)                             DDS BATCH TYPE               
         DC    CL14' '                             C/A                          
         DC    C'27'                               MCKIM JE NUMBER              
         DC    C'    '                             MCKIM GL ACCOUNT             
         DC    AL1(0,0,0)                          CLIENT STATUS BYTES          
         DC    AL1(0)                              EXTRA STATUS                 
         DC    CL30'GST INPUT TAX CREDIT - RCVBLE' DESCRIPTION                  
*                                                                               
*                                                                               
         DC    CL14'SG'                            ACCOUNT                      
         DC    AL1(0)                              L'ACCT TO MATCH              
         DC    AL1(DR)                             STATUS                       
         DC    AL1(50)                             DDS BATCH TYPE               
         DC    CL14' '                             C/A                          
         DC    C'27'                               MCKIM JE NUMBER              
         DC    C'    '                             MCKIM GL ACCOUNT             
         DC    AL1(0,0,0)                          CLIENT STATUS BYTES          
         DC    AL1(0)                              EXTRA STATUS                 
         DC    CL30'GST INPUT TAX CREDIT - RCVBLE' DESCRIPTION                  
*                                                                               
*                                                                               
         DC    CL14'SG'                            ACCOUNT                      
         DC    AL1(0)                              L'ACCT TO MATCH              
         DC    AL1(DR)                             STATUS                       
         DC    AL1(0)                              DDS BATCH TYPE               
         DC    CL14' '                             C/A                          
         DC    C'23'                               MCKIM JE NUMBER              
         DC    C'    '                             MCKIM GL ACCOUNT             
         DC    AL1(0,0,0)                          CLIENT STATUS BYTES          
         DC    AL1(0)                              EXTRA STATUS                 
         DC    CL30'GST INPUT TAX CREDIT - RCVBLE' DESCRIPTION                  
*                                                                               
*                                                                               
         DC    CL14'SG'                            ACCOUNT                      
         DC    AL1(0)                              L'ACCT TO MATCH              
         DC    AL1(CR)                             STATUS                       
         DC    AL1(0)                              DDS BATCH TYPE               
         DC    CL14' '                             C/A                          
         DC    C'24'                               MCKIM JE NUMBER              
         DC    C'    '                             MCKIM GL ACCOUNT             
         DC    AL1(0,0,0)                          CLIENT STATUS BYTES          
         DC    AL1(0)                              EXTRA STATUS                 
         DC    CL30'FINAL BILLING (ACCTS RCVBLE)'  DESCRIPTION                  
         EJECT ,                                                                
******************************************************************              
*              U/L=SI                                            *              
******************************************************************              
*                                                                               
         DC    CL14'SI'                            ACCOUNT                      
         DC    AL1(0)                              L'ACCT TO MATCH              
         DC    AL1(CR)                             STATUS                       
         DC    AL1(1)                              DDS BATCH TYPE               
         DC    CL14'SJ'                            C/A                          
         DC    C'45'                               MCKIM JE NUMBER              
         DC    C'    '                             MCKIM GL ACCOUNT             
         DC    AL1(0,0,0)                          CLIENT STATUS BYTES          
         DC    AL1(0)                              EXTRA STATUS                 
         DC    CL30'PRODUCTION ALLOCATION'         DESCRIPTION                  
*                                                                               
*                                                                               
         DC    CL14'SI'                            ACCOUNT                      
         DC    AL1(0)                              L'ACCT TO MATCH              
         DC    AL1(DR+CR)                          STATUS                       
         DC    AL1(5)                              DDS BATCH TYPE               
         DC    CL14' '                             C/A                          
         DC    C'22'                               MCKIM JE NUMBER              
         DC    C'    '                             MCKIM GL ACCOUNT             
         DC    AL1(0,0,0)                          CLIENT STATUS BYTES          
         DC    AL1(0)                              EXTRA STATUS                 
         DC    CL30'CHEQUE PAYMENTS'               DESCRIPTION                  
*                                                                               
*                                                                               
         DC    CL14'SI'                            ACCOUNT                      
         DC    AL1(0)                              L'ACCT TO MATCH              
         DC    AL1(CR)                             STATUS                       
         DC    AL1(0)                              DDS BATCH TYPE               
         DC    CL14' '                             C/A                          
         DC    C'24'                               MCKIM JE NUMBER              
         DC    C'    '                             MCKIM GL ACCOUNT             
         DC    AL1(CNTR,0,6)                       CLIENT STATUS BYTES          
         DC    AL1(0)                              EXTRA STATUS                 
         DC    CL30'FINAL BILLING (ACCTS RCVBLE)'  DESCRIPTION                  
*                                                                               
*                                                                               
         DC    CL14'SI'                            ACCOUNT                      
         DC    AL1(0)                              L'ACCT TO MATCH              
         DC    AL1(CR)                             STATUS                       
         DC    AL1(33)                             DDS BATCH TYPE               
         DC    CL14'SJ'                            C/A                          
         DC    C'45'                               MCKIM JE NUMBER              
         DC    C'    '                             MCKIM GL ACCOUNT             
         DC    AL1(0,0,0)                          CLIENT STATUS BYTES          
         DC    AL1(0)                              EXTRA STATUS                 
         DC    CL30'PRODUCTION ALLOCATION'         DESCRIPTION                  
         EJECT ,                                                                
******************************************************************              
*              U/L=SJ                                            *              
******************************************************************              
*                                                                               
         DC    CL14'SJ'                            ACCOUNT                      
         DC    AL1(0)                              L'ACCT TO MATCH              
         DC    AL1(DR)                             STATUS                       
         DC    AL1(1)                              DDS BATCH TYPE               
         DC    CL14'SV'                            C/A                          
         DC    C'45'                               MCKIM JE NUMBER              
         DC    C'1500'                             MCKIM GL ACCOUNT             
         DC    AL1(SJ24,0,0)                       CLIENT STATUS BYTES          
         DC    AL1(0)                              EXTRA STATUS                 
         DC    CL30'PRODUCTION ALLOCATION'         DESCRIPTION                  
*                                                                               
*                                                                               
         DC    CL14'SJ'                            ACCOUNT                      
         DC    AL1(0)                              L'ACCT TO MATCH              
         DC    AL1(DR)                             STATUS                       
         DC    AL1(1)                              DDS BATCH TYPE               
         DC    CL14'SW'                            C/A                          
         DC    C'45'                               MCKIM JE NUMBER              
         DC    C'1500'                             MCKIM GL ACCOUNT             
         DC    AL1(SJ24,0,0)                       CLIENT STATUS BYTES          
         DC    AL1(0)                              EXTRA STATUS                 
         DC    CL30'PRODUCTION ALLOCATION'         DESCRIPTION                  
*                                                                               
*                                                                               
         DC    CL14'SJ'                            ACCOUNT                      
         DC    AL1(0)                              L'ACCT TO MATCH              
         DC    AL1(DR)                             STATUS                       
         DC    AL1(3)                              DDS BATCH TYPE               
         DC    CL14'SVSMINFINE'                    C/A                          
         DC    C'47'                               MCKIM JE NUMBER              
         DC    C'1500'                             MCKIM GL ACCOUNT             
         DC    AL1(SJ24,0,0)                       CLIENT STATUS BYTES          
         DC    AL1(0)                              EXTRA STATUS                 
         DC    CL30'PST ALLOCATION'                DESCRIPTION                  
*                                                                               
*                                                                               
         DC    CL14'SJ'                            ACCOUNT                      
         DC    AL1(0)                              L'ACCT TO MATCH              
         DC    AL1(DR)                             STATUS                       
         DC    AL1(3)                              DDS BATCH TYPE               
         DC    CL14'SVSMINFINF'                    C/A                          
         DC    C'47'                               MCKIM JE NUMBER              
         DC    C'1500'                             MCKIM GL ACCOUNT             
         DC    AL1(SJ24,0,0)                       CLIENT STATUS BYTES          
         DC    AL1(0)                              EXTRA STATUS                 
         DC    CL30'PST ALLOCATION'                DESCRIPTION                  
*                                                                               
*                                                                               
         DC    CL14'SJ'                            ACCOUNT                      
         DC    AL1(0)                              L'ACCT TO MATCH              
         DC    AL1(DR)                             STATUS                       
         DC    AL1(3)                              DDS BATCH TYPE               
         DC    CL14'SVTMINFINV'                    C/A                          
         DC    C'47'                               MCKIM JE NUMBER              
         DC    C'1500'                             MCKIM GL ACCOUNT             
         DC    AL1(SJ24,0,0)                       CLIENT STATUS BYTES          
         DC    AL1(0)                              EXTRA STATUS                 
         DC    CL30'PST ALLOCATION'                DESCRIPTION                  
*                                                                               
*                                                                               
         DC    CL14'SJ'                            ACCOUNT                      
         DC    AL1(0)                              L'ACCT TO MATCH              
         DC    AL1(DR)                             STATUS                       
         DC    AL1(3)                              DDS BATCH TYPE               
         DC    CL14'SVTMINFINT'                    C/A                          
         DC    C'47'                               MCKIM JE NUMBER              
         DC    C'1500'                             MCKIM GL ACCOUNT             
         DC    AL1(SJ24,0,0)                       CLIENT STATUS BYTES          
         DC    AL1(0)                              EXTRA STATUS                 
         DC    CL30'PST ALLOCATION'                DESCRIPTION                  
*                                                                               
*                                                                               
         DC    CL14'SJ'                            ACCOUNT                      
         DC    AL1(0)                              L'ACCT TO MATCH              
         DC    AL1(DR)                             STATUS                       
         DC    AL1(3)                              DDS BATCH TYPE               
         DC    CL14'SVTMINFINC'                    C/A                          
         DC    C'47'                               MCKIM JE NUMBER              
         DC    C'1500'                             MCKIM GL ACCOUNT             
         DC    AL1(SJ24,0,0)                       CLIENT STATUS BYTES          
         DC    AL1(0)                              EXTRA STATUS                 
         DC    CL30'PST ALLOCATION'                DESCRIPTION                  
*                                                                               
*                                                                               
         DC    CL14'SJ'                            ACCOUNT                      
         DC    AL1(0)                              L'ACCT TO MATCH              
         DC    AL1(DR)                             STATUS                       
         DC    AL1(3)                              DDS BATCH TYPE               
         DC    CL14'SVTMINFINW'                    C/A                          
         DC    C'47'                               MCKIM JE NUMBER              
         DC    C'1500'                             MCKIM GL ACCOUNT             
         DC    AL1(SJ24,0,0)                       CLIENT STATUS BYTES          
         DC    AL1(0)                              EXTRA STATUS                 
         DC    CL30'PST ALLOCATION'                DESCRIPTION                  
*                                                                               
*                                                                               
         DC    CL14'SJ'                            ACCOUNT                      
         DC    AL1(0)                              L'ACCT TO MATCH              
         DC    AL1(DR)                             STATUS                       
         DC    AL1(3)                              DDS BATCH TYPE               
         DC    CL14'SVUMINFINN'                    C/A                          
         DC    C'47'                               MCKIM JE NUMBER              
         DC    C'1500'                             MCKIM GL ACCOUNT             
         DC    AL1(SJ24,0,0)                       CLIENT STATUS BYTES          
         DC    AL1(0)                              EXTRA STATUS                 
         DC    CL30'PST ALLOCATION'                DESCRIPTION                  
*                                                                               
*                                                                               
         DC    CL14'SJ'                            ACCOUNT                      
         DC    AL1(0)                              L'ACCT TO MATCH              
         DC    AL1(DR)                             STATUS                       
         DC    AL1(3)                              DDS BATCH TYPE               
         DC    CL14'SVUMINFING'                    C/A                          
         DC    C'47'                               MCKIM JE NUMBER              
         DC    C'1500'                             MCKIM GL ACCOUNT             
         DC    AL1(SJ24,0,0)                       CLIENT STATUS BYTES          
         DC    AL1(0)                              EXTRA STATUS                 
         DC    CL30'PST ALLOCATION'                DESCRIPTION                  
*                                                                               
*                                                                               
         DC    CL14'SJ'                            ACCOUNT                      
         DC    AL1(0)                              L'ACCT TO MATCH              
         DC    AL1(DR)                             STATUS                       
         DC    AL1(3)                              DDS BATCH TYPE               
         DC    CL14'SV'                            C/A                          
         DC    C'46'                               MCKIM JE NUMBER              
         DC    C'1500'                             MCKIM GL ACCOUNT             
         DC    AL1(SJ24,0,0)                       CLIENT STATUS BYTES          
         DC    AL1(0)                              EXTRA STATUS                 
         DC    CL30'DOCKET CLEARING'               DESCRIPTION                  
*                                                                               
*                                                                               
         DC    CL14'SJ'                            ACCOUNT                      
         DC    AL1(0)                              L'ACCT TO MATCH              
         DC    AL1(DR)                             STATUS                       
         DC    AL1(3)                              DDS BATCH TYPE               
         DC    CL14'SW'                            C/A                          
         DC    C'46'                               MCKIM JE NUMBER              
         DC    C'1500'                             MCKIM GL ACCOUNT             
         DC    AL1(SJ24,0,0)                       CLIENT STATUS BYTES          
         DC    AL1(0)                              EXTRA STATUS                 
         DC    CL30'DOCKET CLEARING'               DESCRIPTION                  
*                                                                               
*                                                                               
         DC    CL14'SJ'                            ACCOUNT                      
         DC    AL1(0)                              L'ACCT TO MATCH              
         DC    AL1(CR)                             STATUS                       
         DC    AL1(6)                              DDS BATCH TYPE               
         DC    CL14'SR'                            C/A                          
         DC    C'24'                               MCKIM JE NUMBER              
         DC    C'1500'                             MCKIM GL ACCOUNT             
         DC    AL1(CNTR,2,6)                       CLIENT STATUS BYTES          
         DC    AL1(0)                              EXTRA STATUS                 
         DC    CL30'FINAL BILLING (ACCTS RCVBLE)'  DESCRIPTION                  
*                                                                               
*                                                                               
         DC    CL14'SJ'                            ACCOUNT                      
         DC    AL1(0)                              L'ACCT TO MATCH              
         DC    AL1(CR)                             STATUS                       
         DC    AL1(7)                              DDS BATCH TYPE               
         DC    CL14'SR'                            C/A                          
         DC    C'24'                               MCKIM JE NUMBER              
         DC    C'1500'                             MCKIM GL ACCOUNT             
         DC    AL1(CNTR,2,6)                       CLIENT STATUS BYTES          
         DC    AL1(0)                              EXTRA STATUS                 
         DC    CL30'FINAL BILLING (ACCTS RCVBLE)'  DESCRIPTION                  
*                                                                               
*                                                                               
         DC    CL14'SJ'                            ACCOUNT                      
         DC    AL1(0)                              L'ACCT TO MATCH              
         DC    AL1(DR)                             STATUS                       
         DC    AL1(8)                              DDS BATCH TYPE               
         DC    CL14'SK'                            C/A                          
         DC    C'44'                               MCKIM JE NUMBER              
         DC    C'1500'                             MCKIM GL ACCOUNT             
         DC    AL1(SJ24,0,0)                       CLIENT STATUS BYTES          
         DC    AL1(0)                              EXTRA STATUS                 
         DC    CL30'INTERNAL INCOME'               DESCRIPTION                  
*                                                                               
*                                                                               
         DC    CL14'SJ'                            ACCOUNT                      
         DC    AL1(0)                              L'ACCT TO MATCH              
         DC    AL1(DR)                             STATUS                       
         DC    AL1(14)                             DDS BATCH TYPE               
         DC    CL14'SE'                            C/A                          
         DC    C'49'                               MCKIM JE NUMBER              
         DC    C'1500'                             MCKIM GL ACCOUNT             
         DC    AL1(SJ24,0,0)                       CLIENT STATUS BYTES          
         DC    AL1(CANAME)                         EXTRA STATUS                 
         DC    CL30'               - DOCKET TO EXP'  DESCRIPTION                
*                                                                               
*                                                                               
         DC    CL14'SJ'                            ACCOUNT                      
         DC    AL1(0)                              L'ACCT TO MATCH              
         DC    AL1(DR)                             STATUS                       
         DC    AL1(15)                             DDS BATCH TYPE               
         DC    CL14'SE'                            C/A                          
         DC    C'48'                               MCKIM JE NUMBER              
         DC    C'1500'                             MCKIM GL ACCOUNT             
         DC    AL1(SJ24,0,0)                       CLIENT STATUS BYTES          
         DC    AL1(CANAME)                         EXTRA STATUS                 
         DC    CL30'               - EXP TO DOCKET'  DESCRIPTION                
*                                                                               
*                                                                               
         DC    CL14'SJ'                            ACCOUNT                      
         DC    AL1(0)                              L'ACCT TO MATCH              
         DC    AL1(DR)                             STATUS                       
         DC    AL1(33)                             DDS BATCH TYPE               
         DC    CL14'SV'                            C/A                          
         DC    C'45'                               MCKIM JE NUMBER              
         DC    C'1500'                             MCKIM GL ACCOUNT             
         DC    AL1(SJ24,0,0)                       CLIENT STATUS BYTES          
         DC    AL1(0)                              EXTRA STATUS                 
         DC    CL30'PRODUCTION ALLOCATION'         DESCRIPTION                  
*                                                                               
*                                                                               
         DC    CL14'SJ'                            ACCOUNT                      
         DC    AL1(0)                              L'ACCT TO MATCH              
         DC    AL1(DR)                             STATUS                       
         DC    AL1(34)                             DDS BATCH TYPE               
         DC    CL14' '                             C/A                          
         DC    C'45'                               MCKIM JE NUMBER              
         DC    C'1500'                             MCKIM GL ACCOUNT             
         DC    AL1(SJ24,0,0)                       CLIENT STATUS BYTES          
         DC    AL1(0)                              EXTRA STATUS                 
         DC    CL30'PRODUCTION ALLOCATION'         DESCRIPTION                  
         EJECT ,                                                                
******************************************************************              
*              U/L=SK                                            *              
******************************************************************              
*                                                                               
         DC    CL14'SK'                            ACCOUNT                      
         DC    AL1(0)                              L'ACCT TO MATCH              
         DC    AL1(DR)                             STATUS                       
         DC    AL1(6)                              DDS BATCH TYPE               
         DC    CL14'SJ'                            C/A                          
         DC    C'24'                               MCKIM JE NUMBER              
         DC    C'    '                             MCKIM GL ACCOUNT             
         DC    AL1(SJ24,0,0)                       CLIENT STATUS BYTES          
         DC    AL1(0)                              EXTRA STATUS                 
         DC    CL30'FINAL BILLING (ACCTS RCVBLE)'  DESCRIPTION                  
*                                                                               
*                                                                               
         DC    CL14'SK'                            ACCOUNT                      
         DC    AL1(0)                              L'ACCT TO MATCH              
         DC    AL1(DR)                             STATUS                       
         DC    AL1(7)                              DDS BATCH TYPE               
         DC    CL14'SJ'                            C/A                          
         DC    C'24'                               MCKIM JE NUMBER              
         DC    C'    '                             MCKIM GL ACCOUNT             
         DC    AL1(SJ24,0,0)                       CLIENT STATUS BYTES          
         DC    AL1(0)                              EXTRA STATUS                 
         DC    CL30'FINAL BILLING (ACCTS RCVBLE)'  DESCRIPTION                  
*                                                                               
*                                                                               
         DC    CL14'SK'                            ACCOUNT                      
         DC    AL1(0)                              L'ACCT TO MATCH              
         DC    AL1(CR)                             STATUS                       
         DC    AL1(8)                              DDS BATCH TYPE               
         DC    CL14'SJ'                            C/A                          
         DC    C'44'                               MCKIM JE NUMBER              
         DC    C'    '                             MCKIM GL ACCOUNT             
         DC    AL1(SJ24,0,0)                       CLIENT STATUS BYTES          
         DC    AL1(0)                              EXTRA STATUS                 
         DC    CL30'INTERNAL INCOME'               DESCRIPTION                  
*                                                                               
*                                                                               
         DC    CL14'SK'                            ACCOUNT                      
         DC    AL1(0)                              L'ACCT TO MATCH              
         DC    AL1(CR)                             STATUS                       
         DC    AL1(34)                             DDS BATCH TYPE               
         DC    CL14'SJ'                            C/A                          
         DC    C'44'                               MCKIM JE NUMBER              
         DC    C'    '                             MCKIM GL ACCOUNT             
         DC    AL1(SJ24,0,0)                       CLIENT STATUS BYTES          
         DC    AL1(0)                              EXTRA STATUS                 
         DC    CL30'INTERNAL INCOME'               DESCRIPTION                  
         EJECT ,                                                                
******************************************************************              
*              U/L=SP                                            *              
******************************************************************              
*                                                                               
         DC    CL14'SP'                            ACCOUNT                      
         DC    AL1(0)                              L'ACCT TO MATCH              
         DC    AL1(DR+CR)                          STATUS                       
         DC    AL1(37)                             DDS BATCH TYPE               
         DC    CL14' '                             C/A                          
         DC    C'30'                               MCKIM JE NUMBER              
         DC    C'2013'                             MCKIM GL ACCOUNT             
         DC    AL1(0,0,0)                          CLIENT STATUS BYTES          
         DC    AL1(0)                              EXTRA STATUS                 
         DC    CL30'CANCELLED CHEQUES'             DESCRIPTION                  
*                                                                               
*                                                                               
         DC    CL14'SP'                            ACCOUNT                      
         DC    AL1(0)                              L'ACCT TO MATCH              
         DC    AL1(CR)                             STATUS                       
         DC    AL1(45)                             DDS BATCH TYPE               
         DC    CL14'SE094006830'                   C/A                          
         DC    C'28'                               MCKIM JE NUMBER              
         DC    C'2013'                             MCKIM GL ACCOUNT             
         DC    AL1(0,0,0)                          CLIENT STATUS BYTES          
         DC    AL1(0)                              EXTRA STATUS                 
         DC    CL30'FOREIGN ADJUSTMENT'            DESCRIPTION                  
*                                                                               
*                                                                               
         DC    CL14'SP'                            ACCOUNT                      
         DC    AL1(0)                              L'ACCT TO MATCH              
         DC    AL1(CR)                             STATUS                       
         DC    AL1(0)                              DDS BATCH TYPE               
         DC    CL14' '                             C/A                          
         DC    C'27'                               MCKIM JE NUMBER              
         DC    C'2013'                             MCKIM GL ACCOUNT             
         DC    AL1(0,0,0)                          CLIENT STATUS BYTES          
         DC    AL1(0)                              EXTRA STATUS                 
         DC    CL30'CLEARANCES'                    DESCRIPTION                  
*                                                                               
*                                                                               
         DC    CL14'SP'                            ACCOUNT                      
         DC    AL1(0)                              L'ACCT TO MATCH              
         DC    AL1(DR)                             STATUS                       
         DC    AL1(0)                              DDS BATCH TYPE               
         DC    CL14' '                             C/A                          
         DC    C'22'                               MCKIM JE NUMBER              
         DC    C'2013'                             MCKIM GL ACCOUNT             
         DC    AL1(0,0,0)                          CLIENT STATUS BYTES          
         DC    AL1(0)                              EXTRA STATUS                 
         DC    CL30'CHEQUE PAYMENTS'               DESCRIPTION                  
*                                                                               
*                                                                               
         DC    CL14'SP'                            ACCOUNT                      
         DC    AL1(0)                              L'ACCT TO MATCH              
         DC    AL1(CR)                             STATUS                       
         DC    AL1(45)                             DDS BATCH TYPE               
         DC    CL14'SB031002026'                   C/A                          
         DC    C'75'                               MCKIM JE NUMBER              
         DC    C'2013'                             MCKIM GL ACCOUNT             
         DC    AL1(0,0,0)                          CLIENT STATUS BYTES          
         DC    AL1(0)                              EXTRA STATUS                 
         DC    CL30'PAYABLES OFFSETS'              DESCRIPTION                  
         EJECT ,                                                                
******************************************************************              
*              U/L=SQ                                            *              
******************************************************************              
*                                                                               
         DC    CL14'SQ'                            ACCOUNT                      
         DC    AL1(0)                              L'ACCT TO MATCH              
         DC    AL1(DR)                             STATUS                       
         DC    AL1(36)                             DDS BATCH TYPE               
         DC    CL14' '                             C/A                          
         DC    C'31'                               MCKIM JE NUMBER              
         DC    C'2012'                             MCKIM GL ACCOUNT             
         DC    AL1(0,0,0)                          CLIENT STATUS BYTES          
         DC    AL1(0)                              EXTRA STATUS                 
         DC    CL30'MANUAL CHEQUE'                 DESCRIPTION                  
*                                                                               
*                                                                               
         DC    CL14'SQ'                            ACCOUNT                      
         DC    AL1(0)                              L'ACCT TO MATCH              
         DC    AL1(DR+CR)                          STATUS                       
         DC    AL1(37)                             DDS BATCH TYPE               
         DC    CL14' '                             C/A                          
         DC    C'30'                               MCKIM JE NUMBER              
         DC    C'2012'                             MCKIM GL ACCOUNT             
         DC    AL1(0,0,0)                          CLIENT STATUS BYTES          
         DC    AL1(0)                              EXTRA STATUS                 
         DC    CL30'CANCELLED CHEQUES'             DESCRIPTION                  
*                                                                               
*                                                                               
         DC    CL14'SQ'                            ACCOUNT                      
         DC    AL1(0)                              L'ACCT TO MATCH              
         DC    AL1(CR)                             STATUS                       
         DC    AL1(45)                             DDS BATCH TYPE               
         DC    CL14'SE094006830'                   C/A                          
         DC    C'28'                               MCKIM JE NUMBER              
         DC    C'2013'                             MCKIM GL ACCOUNT             
         DC    AL1(0,0,0)                          CLIENT STATUS BYTES          
         DC    AL1(0)                              EXTRA STATUS                 
         DC    CL30'FOREIGN ADJUSTMENT'            DESCRIPTION                  
*                                                                               
*                                                                               
         DC    CL14'SQ'                            ACCOUNT                      
         DC    AL1(0)                              L'ACCT TO MATCH              
         DC    AL1(CR)                             STATUS                       
         DC    AL1(0)                              DDS BATCH TYPE               
         DC    CL14' '                             C/A                          
         DC    C'27'                               MCKIM JE NUMBER              
         DC    C'2012'                             MCKIM GL ACCOUNT             
         DC    AL1(0,0,0)                          CLIENT STATUS BYTES          
         DC    AL1(0)                              EXTRA STATUS                 
         DC    CL30'CLEARANCES'                    DESCRIPTION                  
*                                                                               
*                                                                               
         DC    CL14'SQ'                            ACCOUNT                      
         DC    AL1(0)                              L'ACCT TO MATCH              
         DC    AL1(DR)                             STATUS                       
         DC    AL1(0)                              DDS BATCH TYPE               
         DC    CL14' '                             C/A                          
         DC    C'22'                               MCKIM JE NUMBER              
         DC    C'2012'                             MCKIM GL ACCOUNT             
         DC    AL1(0,0,0)                          CLIENT STATUS BYTES          
         DC    AL1(0)                              EXTRA STATUS                 
         DC    CL30'CHEQUE PAYMENTS'               DESCRIPTION                  
*                                                                               
*                                                                               
         DC    CL14'SQ'                            ACCOUNT                      
         DC    AL1(0)                              L'ACCT TO MATCH              
         DC    AL1(CR)                             STATUS                       
         DC    AL1(45)                             DDS BATCH TYPE               
         DC    CL14'SB031002025'                   C/A                          
         DC    C'75'                               MCKIM JE NUMBER              
         DC    C'2012'                             MCKIM GL ACCOUNT             
         DC    AL1(0,0,0)                          CLIENT STATUS BYTES          
         DC    AL1(0)                              EXTRA STATUS                 
         DC    CL30'PAYABLES OFFSETS'              DESCRIPTION                  
*                                                                               
*                                                                               
         DC    CL14'SQ'                            ACCOUNT                      
         DC    AL1(0)                              L'ACCT TO MATCH              
         DC    AL1(CR)                             STATUS                       
         DC    AL1(45)                             DDS BATCH TYPE               
         DC    CL14'SVP'                           C/A                          
         DC    C'33'                               MCKIM JE NUMBER              
         DC    C'2012'                             MCKIM GL ACCOUNT             
         DC    AL1(0,0,0)                          CLIENT STATUS BYTES          
         DC    AL1(0)                              EXTRA STATUS                 
         DC    CL30'A/P TRANSFER'                  DESCRIPTION                  
         EJECT ,                                                                
******************************************************************              
*              U/L=SR                                            *              
******************************************************************              
*                                                                               
         DC    CL14'SR'                            ACCOUNT                      
         DC    AL1(0)                              L'ACCT TO MATCH              
         DC    AL1(DR)                             STATUS                       
         DC    AL1(20)                             DDS BATCH TYPE               
         DC    CL14'  PRIOR O/S'                   C/A                          
         DC    C'53'                               MCKIM JE NUMBER              
         DC    C'1202'                             MCKIM GL ACCOUNT             
         DC    AL1(ACCT,2,6)                       CLIENT STATUS BYTES          
         DC    AL1(0)                              EXTRA STATUS                 
         DC    CL30'FOCUS REC''B BAL. FWD.'        DESCRIPTION                  
*                                                                               
*                                                                               
         DC    CL14'SR'                            ACCOUNT                      
         DC    AL1(0)                              L'ACCT TO MATCH              
         DC    AL1(DR)                             STATUS                       
         DC    AL1(26)                             DDS BATCH TYPE               
         DC    CL14'  PREBILL'                     C/A                          
         DC    C'6 '                               MCKIM JE NUMBER              
         DC    C'1202'                             MCKIM GL ACCOUNT             
         DC    AL1(ACCT,2,6)                       CLIENT STATUS BYTES          
         DC    AL1(0)                              EXTRA STATUS                 
         DC    CL30'ACCTS RCVBLE W/O ADJ.'         DESCRIPTION                  
*                                                                               
*                                                                               
         DC    CL14'SR'                            ACCOUNT                      
         DC    AL1(0)                              L'ACCT TO MATCH              
         DC    AL1(DR)                             STATUS                       
         DC    AL1(26)                             DDS BATCH TYPE               
         DC    CL14'  BAD DEBT W/O'                C/A                          
         DC    C'35'                               MCKIM JE NUMBER              
         DC    C'1202'                             MCKIM GL ACCOUNT             
         DC    AL1(ACCT,2,6)                       CLIENT STATUS BYTES          
         DC    AL1(0)                              EXTRA STATUS                 
         DC    CL30'BAD DEBT W/O'                  DESCRIPTION                  
*                                                                               
*                                                                               
         DC    CL14'SR'                            ACCOUNT                      
         DC    AL1(0)                              L'ACCT TO MATCH              
         DC    AL1(CR)                             STATUS                       
         DC    AL1(30)                             DDS BATCH TYPE               
         DC    CL14'SE094006789'                   C/A                          
         DC    C'53'                               MCKIM JE NUMBER              
         DC    C'1202'                             MCKIM GL ACCOUNT             
         DC    AL1(ACCT,2,6)                       CLIENT STATUS BYTES          
         DC    AL1(WRITEOFF)                       EXTRA STATUS                 
         DC    CL30'RCVBLE TRANSFER-FROM ACCT.'    DESCRIPTION                  
*                                                                               
*                                                                               
         DC    CL14'SR'                            ACCOUNT                      
         DC    AL1(0)                              L'ACCT TO MATCH              
         DC    AL1(CR)                             STATUS                       
         DC    AL1(30)                             DDS BATCH TYPE               
         DC    CL14'SE1220'                        C/A                          
         DC    C'53'                               MCKIM JE NUMBER              
         DC    C'1202'                             MCKIM GL ACCOUNT             
         DC    AL1(ACCT,2,6)                       CLIENT STATUS BYTES          
         DC    AL1(WRITEOFF)                       EXTRA STATUS                 
         DC    CL30'AR ADJUSTMENTS'                DESCRIPTION                  
*                                                                               
*                                                                               
         DC    CL14'SR'                            ACCOUNT                      
         DC    AL1(0)                              L'ACCT TO MATCH              
         DC    AL1(CR)                             STATUS                       
         DC    AL1(30)                             DDS BATCH TYPE               
         DC    CL14'SE094006818'                   C/A                          
         DC    C'26'                               MCKIM JE NUMBER              
         DC    C'1202'                             MCKIM GL ACCOUNT             
         DC    AL1(ACCT,2,6)                       CLIENT STATUS BYTES          
         DC    AL1(WRITEOFF)                       EXTRA STATUS                 
         DC    CL30'AR SMALL BALANCE'              DESCRIPTION                  
*                                                                               
*                                                                               
         DC    CL14'SR'                            ACCOUNT                      
         DC    AL1(0)                              L'ACCT TO MATCH              
         DC    AL1(CR)                             STATUS                       
         DC    AL1(30)                             DDS BATCH TYPE               
         DC    CL14'SE094006812'                   C/A                          
         DC    C'26'                               MCKIM JE NUMBER              
         DC    C'1202'                             MCKIM GL ACCOUNT             
         DC    AL1(ACCT,2,6)                       CLIENT STATUS BYTES          
         DC    AL1(WRITEOFF)                       EXTRA STATUS                 
         DC    CL30'WRITE-OFF I/CO BILLING - WCD'  DESCRIPTION                  
*                                                                               
*                                                                               
         DC    CL14'SR'                            ACCOUNT                      
         DC    AL1(0)                              L'ACCT TO MATCH              
         DC    AL1(CR)                             STATUS                       
         DC    AL1(30)                             DDS BATCH TYPE               
         DC    CL14'SE094006830'                   C/A                          
         DC    C'28'                               MCKIM JE NUMBER              
         DC    C'1202'                             MCKIM GL ACCOUNT             
         DC    AL1(ACCT,2,6)                       CLIENT STATUS BYTES          
         DC    AL1(WRITEOFF)                       EXTRA STATUS                 
         DC    CL30'FOREIGN EXCH.'                 DESCRIPTION                  
*                                                                               
*                                                                               
         DC    CL14'SR'                            ACCOUNT                      
         DC    AL1(0)                              L'ACCT TO MATCH              
         DC    AL1(CR)                             STATUS                       
         DC    AL1(30)                             DDS BATCH TYPE               
         DC    CL14'SE094006840'                   C/A                          
         DC    C'28'                               MCKIM JE NUMBER              
         DC    C'1202'                             MCKIM GL ACCOUNT             
         DC    AL1(ACCT,2,6)                       CLIENT STATUS BYTES          
         DC    AL1(WRITEOFF)                       EXTRA STATUS                 
         DC    CL30'FOREIGN EXCH.'                 DESCRIPTION                  
*                                                                               
*                                                                               
         DC    CL14'SR'                            ACCOUNT                      
         DC    AL1(0)                              L'ACCT TO MATCH              
         DC    AL1(CR)                             STATUS                       
         DC    AL1(30)                             DDS BATCH TYPE               
         DC    CL14'SE083956880'                   C/A                          
         DC    C'35'                               MCKIM JE NUMBER              
         DC    C'1202'                             MCKIM GL ACCOUNT             
         DC    AL1(ACCT,2,6)                       CLIENT STATUS BYTES          
         DC    AL1(WRITEOFF)                       EXTRA STATUS                 
         DC    CL30'BAD DEBTS'                     DESCRIPTION                  
*                                                                               
*                                                                               
         DC    CL14'SR'                            ACCOUNT                      
         DC    AL1(0)                              L'ACCT TO MATCH              
         DC    AL1(DR)                             STATUS                       
         DC    AL1(45)                             DDS BATCH TYPE               
         DC    CL14'  00CD ALLOWED'                C/A                          
         DC    C'29'                               MCKIM JE NUMBER              
         DC    C'1202'                             MCKIM GL ACCOUNT             
         DC    AL1(ACCT,2,6)                       CLIENT STATUS BYTES          
         DC    AL1(0)                              EXTRA STATUS                 
         DC    CL30'CD ALLOWED (ACCTS RCVBLE)'     DESCRIPTION                  
*                                                                               
*                                                                               
         DC    CL14'SR'                            ACCOUNT                      
         DC    AL1(0)                              L'ACCT TO MATCH              
         DC    AL1(DR)                             STATUS                       
         DC    AL1(45)                             DDS BATCH TYPE               
         DC    CL14'SE094006855'                   C/A                          
         DC    C'29'                               MCKIM JE NUMBER              
         DC    C'1202'                             MCKIM GL ACCOUNT             
         DC    AL1(ACCT,2,6)                       CLIENT STATUS BYTES          
         DC    AL1(0)                              EXTRA STATUS                 
         DC    CL30'CD ALLOWED'                    DESCRIPTION                  
*                                                                               
*                                                                               
         DC    CL14'SR'                            ACCOUNT                      
         DC    AL1(0)                              L'ACCT TO MATCH              
         DC    AL1(DR)                             STATUS                       
         DC    AL1(45)                             DDS BATCH TYPE               
         DC    CL14'SE6853'                        C/A                          
         DC    C'29'                               MCKIM JE NUMBER              
         DC    C'1202'                             MCKIM GL ACCOUNT             
         DC    AL1(ACCT,2,6)                       CLIENT STATUS BYTES          
         DC    AL1(0)                              EXTRA STATUS                 
         DC    CL30'CD LOST'                       DESCRIPTION                  
*                                                                               
*                                                                               
         DC    CL14'SR'                            ACCOUNT                      
         DC    AL1(0)                              L'ACCT TO MATCH              
         DC    AL1(DR)                             STATUS                       
         DC    AL1(45)                             DDS BATCH TYPE               
         DC    CL14'SE094006850'                   C/A                          
         DC    C'29'                               MCKIM JE NUMBER              
         DC    C'1202'                             MCKIM GL ACCOUNT             
         DC    AL1(ACCT,2,6)                       CLIENT STATUS BYTES          
         DC    AL1(0)                              EXTRA STATUS                 
         DC    CL30'CD LOST'                       DESCRIPTION                  
*                                                                               
*                                                                               
         DC    CL14'SR'                            ACCOUNT                      
         DC    AL1(0)                              L'ACCT TO MATCH              
         DC    AL1(DR)                             STATUS                       
         DC    AL1(45)                             DDS BATCH TYPE               
         DC    CL14'SV'                            C/A                          
         DC    C'36'                               MCKIM JE NUMBER              
         DC    C'1202'                             MCKIM GL ACCOUNT             
         DC    AL1(ACCT,2,6)                       CLIENT STATUS BYTES          
         DC    AL1(0)                              EXTRA STATUS                 
         DC    CL30'REFUNDS TO CLIENTS'            DESCRIPTION                  
*                                                                               
*                                                                               
         DC    CL14'SR'                            ACCOUNT                      
         DC    AL1(0)                              L'ACCT TO MATCH              
         DC    AL1(DR+CR)                          STATUS                       
         DC    AL1(45)                             DDS BATCH TYPE               
         DC    CL14' '                             C/A                          
         DC    C'6 '                               MCKIM JE NUMBER              
         DC    C'1202'                             MCKIM GL ACCOUNT             
         DC    AL1(ACCT,2,6)                       CLIENT STATUS BYTES          
         DC    AL1(0)                              EXTRA STATUS                 
         DC    CL30'ACCTS RCVBLE WRITE-OFF ADJ.'   DESCRIPTION                  
*                                                                               
*                                                                               
         DC    CL14'SR'                            ACCOUNT                      
         DC    AL1(0)                              L'ACCT TO MATCH              
         DC    AL1(DR)                             STATUS                       
         DC    AL1(0)                              DDS BATCH TYPE               
         DC    CL14' '                             C/A                          
         DC    C'24'                               MCKIM JE NUMBER              
         DC    C'1202'                             MCKIM GL ACCOUNT             
         DC    AL1(ACCT,2,6)                       CLIENT STATUS BYTES          
         DC    AL1(0)                              EXTRA STATUS                 
         DC    CL30'FINAL BILLING (ACCTS RCVBLE)'  DESCRIPTION                  
*                                                                               
*                                                                               
         DC    CL14'SR'                            ACCOUNT                      
         DC    AL1(0)                              L'ACCT TO MATCH              
         DC    AL1(CR)                             STATUS                       
         DC    AL1(0)                              DDS BATCH TYPE               
         DC    CL14' '                             C/A                          
         DC    C'21'                               MCKIM JE NUMBER              
         DC    C'1202'                             MCKIM GL ACCOUNT             
         DC    AL1(ACCT,2,6)                       CLIENT STATUS BYTES          
         DC    AL1(0)                              EXTRA STATUS                 
         DC    CL30'CASH RECEIPTS'                 DESCRIPTION                  
         EJECT ,                                                                
******************************************************************              
*              U/L=SS                                            *              
******************************************************************              
*                                                                               
         DC    CL14'SS'                            ACCOUNT                      
         DC    AL1(0)                              L'ACCT TO MATCH              
         DC    AL1(DR+CR)                          STATUS                       
         DC    AL1(5)                              DDS BATCH TYPE               
         DC    CL14' '                             C/A                          
         DC    C'22'                               MCKIM JE NUMBER              
         DC    C'2156'                             MCKIM GL ACCOUNT             
         DC    AL1(0,0,0)                          CLIENT STATUS BYTES          
         DC    AL1(0)                              EXTRA STATUS                 
         DC    CL30'CHEQUE PAYMENTS'               DESCRIPTION                  
*                                                                               
*                                                                               
         DC    CL14'SS'                            ACCOUNT                      
         DC    AL1(0)                              L'ACCT TO MATCH              
         DC    AL1(DR+CR)                          STATUS                       
         DC    AL1(37)                             DDS BATCH TYPE               
         DC    CL14' '                             C/A                          
         DC    C'30'                               MCKIM JE NUMBER              
         DC    C'2156'                             MCKIM GL ACCOUNT             
         DC    AL1(0,0,0)                          CLIENT STATUS BYTES          
         DC    AL1(0)                              EXTRA STATUS                 
         DC    CL30'CANCELLED CHEQUES'             DESCRIPTION                  
*                                                                               
*                                                                               
         DC    CL14'SS'                            ACCOUNT                      
         DC    AL1(0)                              L'ACCT TO MATCH              
         DC    AL1(CR)                             STATUS                       
         DC    AL1(45)                             DDS BATCH TYPE               
         DC    CL14'SB031002026'                   C/A                          
         DC    C'75'                               MCKIM JE NUMBER              
         DC    C'2156'                             MCKIM GL ACCOUNT             
         DC    AL1(0,0,0)                          CLIENT STATUS BYTES          
         DC    AL1(0)                              EXTRA STATUS                 
         DC    CL30'PAYABLES OFFSETS'              DESCRIPTION                  
*                                                                               
*                                                                               
         DC    CL14'SS'                            ACCOUNT                      
         DC    AL1(0)                              L'ACCT TO MATCH              
         DC    AL1(CR)                             STATUS                       
         DC    AL1(45)                             DDS BATCH TYPE               
         DC    CL14'SE094006830'                   C/A                          
         DC    C'28'                               MCKIM JE NUMBER              
         DC    C'2156'                             MCKIM GL ACCOUNT             
         DC    AL1(0,0,0)                          CLIENT STATUS BYTES          
         DC    AL1(0)                              EXTRA STATUS                 
         DC    CL30'FOREIGN ADJUSTMENT'            DESCRIPTION                  
*                                                                               
*                                                                               
         DC    CL14'SS'                            ACCOUNT                      
         DC    AL1(0)                              L'ACCT TO MATCH              
         DC    AL1(CR)                             STATUS                       
         DC    AL1(0)                              DDS BATCH TYPE               
         DC    CL14' '                             C/A                          
         DC    C'23'                               MCKIM JE NUMBER              
         DC    C'2156'                             MCKIM GL ACCOUNT             
         DC    AL1(0,0,0)                          CLIENT STATUS BYTES          
         DC    AL1(0)                              EXTRA STATUS                 
         DC    CL30'CLEARANCES'                    DESCRIPTION                  
*                                                                               
*                                                                               
         DC    CL14'SS'                            ACCOUNT                      
         DC    AL1(0)                              L'ACCT TO MATCH              
         DC    AL1(DR)                             STATUS                       
         DC    AL1(0)                              DDS BATCH TYPE               
         DC    CL14' '                             C/A                          
         DC    C'22'                               MCKIM JE NUMBER              
         DC    C'2156'                             MCKIM GL ACCOUNT             
         DC    AL1(0,0,0)                          CLIENT STATUS BYTES          
         DC    AL1(0)                              EXTRA STATUS                 
         DC    CL30'CHEQUE PAYMENTS'               DESCRIPTION                  
         EJECT ,                                                                
******************************************************************              
*              U/L=ST                                            *              
******************************************************************              
*                                                                               
         DC    CL14'ST'                            ACCOUNT                      
         DC    AL1(0)                              L'ACCT TO MATCH              
         DC    AL1(DR+CR)                          STATUS                       
         DC    AL1(5)                              DDS BATCH TYPE               
         DC    CL14' '                             C/A                          
         DC    C'22'                               MCKIM JE NUMBER              
         DC    C'2155'                             MCKIM GL ACCOUNT             
         DC    AL1(0,0,0)                          CLIENT STATUS BYTES          
         DC    AL1(0)                              EXTRA STATUS                 
         DC    CL30'CHEQUE PAYMENTS'               DESCRIPTION                  
*                                                                               
*                                                                               
         DC    CL14'ST'                            ACCOUNT                      
         DC    AL1(0)                              L'ACCT TO MATCH              
         DC    AL1(DR)                             STATUS                       
         DC    AL1(36)                             DDS BATCH TYPE               
         DC    CL14' '                             C/A                          
         DC    C'31'                               MCKIM JE NUMBER              
         DC    C'2155'                             MCKIM GL ACCOUNT             
         DC    AL1(0,0,0)                          CLIENT STATUS BYTES          
         DC    AL1(0)                              EXTRA STATUS                 
         DC    CL30'MANUAL CHEQUE'                 DESCRIPTION                  
*                                                                               
*                                                                               
         DC    CL14'ST'                            ACCOUNT                      
         DC    AL1(0)                              L'ACCT TO MATCH              
         DC    AL1(DR+CR)                          STATUS                       
         DC    AL1(37)                             DDS BATCH TYPE               
         DC    CL14' '                             C/A                          
         DC    C'30'                               MCKIM JE NUMBER              
         DC    C'2155'                             MCKIM GL ACCOUNT             
         DC    AL1(0,0,0)                          CLIENT STATUS BYTES          
         DC    AL1(0)                              EXTRA STATUS                 
         DC    CL30'CANCELLED CHEQUES'             DESCRIPTION                  
*                                                                               
*                                                                               
         DC    CL14'ST'                            ACCOUNT                      
         DC    AL1(0)                              L'ACCT TO MATCH              
         DC    AL1(CR)                             STATUS                       
         DC    AL1(45)                             DDS BATCH TYPE               
         DC    CL14'SB031002025'                   C/A                          
         DC    C'75'                               MCKIM JE NUMBER              
         DC    C'2155'                             MCKIM GL ACCOUNT             
         DC    AL1(0,0,0)                          CLIENT STATUS BYTES          
         DC    AL1(0)                              EXTRA STATUS                 
         DC    CL30'PAYABLES OFFSETS'              DESCRIPTION                  
*                                                                               
*                                                                               
         DC    CL14'ST'                            ACCOUNT                      
         DC    AL1(0)                              L'ACCT TO MATCH              
         DC    AL1(CR)                             STATUS                       
         DC    AL1(45)                             DDS BATCH TYPE               
         DC    CL14'SE094006830'                   C/A                          
         DC    C'28'                               MCKIM JE NUMBER              
         DC    C'2155'                             MCKIM GL ACCOUNT             
         DC    AL1(0,0,0)                          CLIENT STATUS BYTES          
         DC    AL1(0)                              EXTRA STATUS                 
         DC    CL30'FOREIGN ADJUSTMENT'            DESCRIPTION                  
*                                                                               
*                                                                               
         DC    CL14'ST'                            ACCOUNT                      
         DC    AL1(0)                              L'ACCT TO MATCH              
         DC    AL1(CR)                             STATUS                       
         DC    AL1(0)                              DDS BATCH TYPE               
         DC    CL14' '                             C/A                          
         DC    C'23'                               MCKIM JE NUMBER              
         DC    C'2155'                             MCKIM GL ACCOUNT             
         DC    AL1(0,0,0)                          CLIENT STATUS BYTES          
         DC    AL1(0)                              EXTRA STATUS                 
         DC    CL30'CLEARANCES'                    DESCRIPTION                  
*                                                                               
*                                                                               
         DC    CL14'ST'                            ACCOUNT                      
         DC    AL1(0)                              L'ACCT TO MATCH              
         DC    AL1(DR)                             STATUS                       
         DC    AL1(0)                              DDS BATCH TYPE               
         DC    CL14' '                             C/A                          
         DC    C'22'                               MCKIM JE NUMBER              
         DC    C'2155'                             MCKIM GL ACCOUNT             
         DC    AL1(0,0,0)                          CLIENT STATUS BYTES          
         DC    AL1(0)                              EXTRA STATUS                 
         DC    CL30'CHEQUE PAYMENTS'               DESCRIPTION                  
         EJECT ,                                                                
******************************************************************              
*              U/L=SV                                            *              
******************************************************************              
*                                                                               
         DC    CL14'SV'                            ACCOUNT                      
         DC    AL1(0)                              L'ACCT TO MATCH              
         DC    AL1(CR)                             STATUS                       
         DC    AL1(1)                              DDS BATCH TYPE               
         DC    CL14'SJ'                            C/A                          
         DC    C'45'                               MCKIM JE NUMBER              
         DC    C'2290'                             MCKIM GL ACCOUNT             
         DC    AL1(0,0,0)                          CLIENT STATUS BYTES          
         DC    AL1(0)                              EXTRA STATUS                 
         DC    CL30'PRODUCTION ALLOCATION'         DESCRIPTION                  
*                                                                               
*                                                                               
         DC    CL14'SV'                            ACCOUNT                      
         DC    AL1(0)                              L'ACCT TO MATCH              
         DC    AL1(DR+CR)                          STATUS                       
         DC    AL1(9)                              DDS BATCH TYPE               
         DC    CL14' '                             C/A                          
         DC    C'24'                               MCKIM JE NUMBER              
         DC    C'2570'                             MCKIM GL ACCOUNT             
         DC    AL1(0,0,0)                          CLIENT STATUS BYTES          
         DC    AL1(0)                              EXTRA STATUS                 
         DC    CL30'FINAL BILLING (ACCTS RCVBLE)'  DESCRIPTION                  
*                                                                               
*                                                                               
         DC    CL14'SV'                            ACCOUNT                      
         DC    AL1(0)                              L'ACCT TO MATCH              
         DC    AL1(CR)                             STATUS                       
         DC    AL1(33)                             DDS BATCH TYPE               
         DC    CL14'SJ'                            C/A                          
         DC    C'45'                               MCKIM JE NUMBER              
         DC    C'2290'                             MCKIM GL ACCOUNT             
         DC    AL1(0,0,0)                          CLIENT STATUS BYTES          
         DC    AL1(0)                              EXTRA STATUS                 
         DC    CL30'PRODUCTION ALLOCATION'         DESCRIPTION                  
*                                                                               
*                                                                               
         DC    CL14'SVA'                           ACCOUNT                      
         DC    AL1(1)                              L'ACCT TO MATCH              
         DC    AL1(DR)                             STATUS                       
         DC    AL1(36)                             DDS BATCH TYPE               
         DC    CL14' '                             C/A                          
         DC    C'31'                               MCKIM JE NUMBER              
         DC    C'2570'                             MCKIM GL ACCOUNT             
         DC    AL1(0,0,0)                          CLIENT STATUS BYTES          
         DC    AL1(0)                              EXTRA STATUS                 
         DC    CL30'MANUAL CHEQUE'                 DESCRIPTION                  
*                                                                               
*                                                                               
         DC    CL14'SVP'                           ACCOUNT                      
         DC    AL1(1)                              L'ACCT TO MATCH              
         DC    AL1(DR)                             STATUS                       
         DC    AL1(36)                             DDS BATCH TYPE               
         DC    CL14' '                             C/A                          
         DC    C'31'                               MCKIM JE NUMBER              
         DC    C'2290'                             MCKIM GL ACCOUNT             
         DC    AL1(0,0,0)                          CLIENT STATUS BYTES          
         DC    AL1(0)                              EXTRA STATUS                 
         DC    CL30'MANUAL CHEQUE'                 DESCRIPTION                  
*                                                                               
*                                                                               
         DC    CL14'SV'                            ACCOUNT                      
         DC    AL1(0)                              L'ACCT TO MATCH              
         DC    AL1(DR+CR)                          STATUS                       
         DC    AL1(37)                             DDS BATCH TYPE               
         DC    CL14' '                             C/A                          
         DC    C'30'                               MCKIM JE NUMBER              
         DC    C'2290'                             MCKIM GL ACCOUNT             
         DC    AL1(0,0,0)                          CLIENT STATUS BYTES          
         DC    AL1(0)                              EXTRA STATUS                 
         DC    CL30'CANCELLED CHEQUES'             DESCRIPTION                  
*                                                                               
*                                                                               
         DC    CL14'SV'                            ACCOUNT                      
         DC    AL1(0)                              L'ACCT TO MATCH              
         DC    AL1(CR)                             STATUS                       
         DC    AL1(45)                             DDS BATCH TYPE               
         DC    CL14'SC'                            C/A                          
         DC    C'29'                               MCKIM JE NUMBER              
         DC    C'    '                             MCKIM GL ACCOUNT             
         DC    AL1(0,0,0)                          CLIENT STATUS BYTES          
         DC    AL1(0)                              EXTRA STATUS                 
         DC    CL30'CD LOST/REFUNDS'               DESCRIPTION                  
*                                                                               
*                                                                               
         DC    CL14'SVP'                           ACCOUNT                      
         DC    AL1(1)                              L'ACCT TO MATCH              
         DC    AL1(DR+CR)                          STATUS                       
         DC    AL1(45)                             DDS BATCH TYPE               
         DC    CL14'SC'                            C/A                          
         DC    C'22'                               MCKIM JE NUMBER              
         DC    C'2290'                             MCKIM GL ACCOUNT             
         DC    AL1(0,0,0)                          CLIENT STATUS BYTES          
         DC    AL1(0)                              EXTRA STATUS                 
         DC    CL30'CHEQUE PAYMENTS'               DESCRIPTION                  
*                                                                               
*                                                                               
         DC    CL14'SVP'                           ACCOUNT                      
         DC    AL1(1)                              L'ACCT TO MATCH              
         DC    AL1(CR)                             STATUS                       
         DC    AL1(45)                             DDS BATCH TYPE               
         DC    CL14'SQ'                            C/A                          
         DC    C'33'                               MCKIM JE NUMBER              
         DC    C'2290'                             MCKIM GL ACCOUNT             
         DC    AL1(0,0,0)                          CLIENT STATUS BYTES          
         DC    AL1(0)                              EXTRA STATUS                 
         DC    CL30'A/P TRANSFER'                  DESCRIPTION                  
*                                                                               
*                                                                               
         DC    CL14'SV'                            ACCOUNT                      
         DC    AL1(0)                              L'ACCT TO MATCH              
         DC    AL1(CR)                             STATUS                       
         DC    AL1(51)                             DDS BATCH TYPE               
         DC    CL14'SJ'                            C/A                          
         DC    C'24'                               MCKIM JE NUMBER              
         DC    C'2570'                             MCKIM GL ACCOUNT             
         DC    AL1(0,0,0)                          CLIENT STATUS BYTES          
         DC    AL1(0)                              EXTRA STATUS                 
         DC    CL30'FINAL BILLING (ACCTS RCVBLE)'  DESCRIPTION                  
*                                                                               
*                                                                               
         DC    CL14'SV'                            ACCOUNT                      
         DC    AL1(0)                              L'ACCT TO MATCH              
         DC    AL1(CR)                             STATUS                       
         DC    AL1(45)                             DDS BATCH TYPE               
         DC    CL14'SR'                            C/A                          
         DC    C'36'                               MCKIM JE NUMBER              
         DC    C'    '                             MCKIM GL ACCOUNT             
         DC    AL1(0,0,0)                          CLIENT STATUS BYTES          
         DC    AL1(0)                              EXTRA STATUS                 
         DC    CL30'REFUNDS TO CLIENTS'            DESCRIPTION                  
*                                                                               
*                                                                               
         DC    CL14'SVA'                           ACCOUNT                      
         DC    AL1(1)                              L'ACCT TO MATCH              
         DC    AL1(DR)                             STATUS                       
         DC    AL1(0)                              DDS BATCH TYPE               
         DC    CL14' '                             C/A                          
         DC    C'22'                               MCKIM JE NUMBER              
         DC    C'2570'                             MCKIM GL ACCOUNT             
         DC    AL1(0,0,0)                          CLIENT STATUS BYTES          
         DC    AL1(0)                              EXTRA STATUS                 
         DC    CL30'CHEQUE PAYMENTS'               DESCRIPTION                  
*                                                                               
*                                                                               
         DC    CL14'SVP'                           ACCOUNT                      
         DC    AL1(1)                              L'ACCT TO MATCH              
         DC    AL1(DR+CR)                          STATUS                       
         DC    AL1(0)                              DDS BATCH TYPE               
         DC    CL14' '                             C/A                          
         DC    C'22'                               MCKIM JE NUMBER              
         DC    C'2290'                             MCKIM GL ACCOUNT             
         DC    AL1(0,0,0)                          CLIENT STATUS BYTES          
         DC    AL1(0)                              EXTRA STATUS                 
         DC    CL30'CHEQUE PAYMENTS'               DESCRIPTION                  
         EJECT ,                                                                
******************************************************************              
*              U/L=SW                                            *              
******************************************************************              
*                                                                               
         DC    CL14'SW'                            ACCOUNT                      
         DC    AL1(0)                              L'ACCT TO MATCH              
         DC    AL1(CR)                             STATUS                       
         DC    AL1(1)                              DDS BATCH TYPE               
         DC    CL14'SJ'                            C/A                          
         DC    C'45'                               MCKIM JE NUMBER              
         DC    C'2295'                             MCKIM GL ACCOUNT             
         DC    AL1(0,0,0)                          CLIENT STATUS BYTES          
         DC    AL1(0)                              EXTRA STATUS                 
         DC    CL30'PRODUCTION ALLOCATION'         DESCRIPTION                  
*                                                                               
*                                                                               
         DC    CL14'SW'                            ACCOUNT                      
         DC    AL1(0)                              L'ACCT TO MATCH              
         DC    AL1(DR+CR)                          STATUS                       
         DC    AL1(37)                             DDS BATCH TYPE               
         DC    CL14' '                             C/A                          
         DC    C'30'                               MCKIM JE NUMBER              
         DC    C'2295'                             MCKIM GL ACCOUNT             
         DC    AL1(0,0,0)                          CLIENT STATUS BYTES          
         DC    AL1(0)                              EXTRA STATUS                 
         DC    CL30'CANCELLED CHEQUES'             DESCRIPTION                  
*                                                                               
*                                                                               
         DC    CL14'SW'                            ACCOUNT                      
         DC    AL1(0)                              L'ACCT TO MATCH              
         DC    AL1(DR)                             STATUS                       
         DC    AL1(0)                              DDS BATCH TYPE               
         DC    CL14' '                             C/A                          
         DC    C'22'                               MCKIM JE NUMBER              
         DC    C'2295'                             MCKIM GL ACCOUNT             
         DC    AL1(0,0,0)                          CLIENT STATUS BYTES          
         DC    AL1(0)                              EXTRA STATUS                 
         DC    CL30'CHEQUE PAYMENTS'               DESCRIPTION                  
         EJECT ,                                                                
******************************************************************              
*              U/L=SZ                                            *              
******************************************************************              
*                                                                               
         DC    CL14'SZP'                           ACCOUNT                      
         DC    AL1(1)                              L'ACCT TO MATCH              
         DC    AL1(DR)                             STATUS                       
         DC    AL1(0)                              DDS BATCH TYPE               
         DC    CL14' '                             C/A                          
         DC    C'27'                               MCKIM JE NUMBER              
         DC    C'2165'                             MCKIM GL ACCOUNT             
         DC    AL1(0,0,0)                          CLIENT STATUS BYTES          
         DC    AL1(0)                              EXTRA STATUS                 
         DC    CL30'CLEARANCES'                    DESCRIPTION                  
*                                                                               
*                                                                               
         DC    CL14'SZS'                           ACCOUNT                      
         DC    AL1(1)                              L'ACCT TO MATCH              
         DC    AL1(DR)                             STATUS                       
         DC    AL1(0)                              DDS BATCH TYPE               
         DC    CL14' '                             C/A                          
         DC    C'23'                               MCKIM JE NUMBER              
         DC    C'2165'                             MCKIM GL ACCOUNT             
         DC    AL1(0,0,0)                          CLIENT STATUS BYTES          
         DC    AL1(0)                              EXTRA STATUS                 
         DC    CL30'CLEARANCES'                    DESCRIPTION                  
*                                                                               
*                                                                               
         DC    CL14'SZ'                            ACCOUNT                      
         DC    AL1(0)                              L'ACCT TO MATCH              
         DC    AL1(CR)                             STATUS                       
         DC    AL1(0)                              DDS BATCH TYPE               
         DC    CL14' '                             C/A                          
         DC    C'24'                               MCKIM JE NUMBER              
         DC    C'2165'                             MCKIM GL ACCOUNT             
         DC    AL1(0,0,0)                          CLIENT STATUS BYTES          
         DC    AL1(0)                              EXTRA STATUS                 
         DC    CL30'FINAL BILLING (ACCTS RCVBLE)'  DESCRIPTION                  
*                                                                               
*                                                                               
         DC    X'FF'                               END OF TABLE                 
         EJECT ,                                                                
******************************************************************              
*              U/L=SC                                            *              
******************************************************************              
*                                                                               
LDGDEF   DS    0F                                                               
         DC    CL14'SCM2610AC'                     ACCOUNT                      
         DC    AL1(12)                             L'ACCT TO MATCH              
         DC    AL1(CR)                             STATUS                       
         DC    AL1(3)                              DDS BATCH TYPE               
         DC    CL14'SJ'                            C/A                          
         DC    C'47'                               MCKIM JE NUMBER              
         DC    C'    '                             MCKIM GL ACCOUNT             
         DC    AL1(0,0,0)                          CLIENT STATUS BYTES          
         DC    AL1(0)                              EXTRA STATUS                 
         DC    CL30'PST ALLOCATION'                DESCRIPTION                  
*                                                                               
*                                                                               
         DC    CL14'SCM2610AT'                     ACCOUNT                      
         DC    AL1(12)                             L'ACCT TO MATCH              
         DC    AL1(CR)                             STATUS                       
         DC    AL1(3)                              DDS BATCH TYPE               
         DC    CL14'SJ'                            C/A                          
         DC    C'47'                               MCKIM JE NUMBER              
         DC    C'    '                             MCKIM GL ACCOUNT             
         DC    AL1(0,0,0)                          CLIENT STATUS BYTES          
         DC    AL1(0)                              EXTRA STATUS                 
         DC    CL30'PST ALLOCATION'                DESCRIPTION                  
*                                                                               
*                                                                               
         DC    CL14'SCM2610AV'                     ACCOUNT                      
         DC    AL1(12)                             L'ACCT TO MATCH              
         DC    AL1(CR)                             STATUS                       
         DC    AL1(3)                              DDS BATCH TYPE               
         DC    CL14'SJ'                            C/A                          
         DC    C'47'                               MCKIM JE NUMBER              
         DC    C'    '                             MCKIM GL ACCOUNT             
         DC    AL1(0,0,0)                          CLIENT STATUS BYTES          
         DC    AL1(0)                              EXTRA STATUS                 
         DC    CL30'PST ALLOCATION'                DESCRIPTION                  
*                                                                               
*                                                                               
         DC    CL14'SCM2610AW'                     ACCOUNT                      
         DC    AL1(12)                             L'ACCT TO MATCH              
         DC    AL1(CR)                             STATUS                       
         DC    AL1(3)                              DDS BATCH TYPE               
         DC    CL14'SJ'                            C/A                          
         DC    C'47'                               MCKIM JE NUMBER              
         DC    C'    '                             MCKIM GL ACCOUNT             
         DC    AL1(0,0,0)                          CLIENT STATUS BYTES          
         DC    AL1(0)                              EXTRA STATUS                 
         DC    CL30'PST ALLOCATION'                DESCRIPTION                  
*                                                                               
*                                                                               
         DC    CL14'SCM2610BC'                     ACCOUNT                      
         DC    AL1(12)                             L'ACCT TO MATCH              
         DC    AL1(CR)                             STATUS                       
         DC    AL1(3)                              DDS BATCH TYPE               
         DC    CL14'SJ'                            C/A                          
         DC    C'47'                               MCKIM JE NUMBER              
         DC    C'    '                             MCKIM GL ACCOUNT             
         DC    AL1(0,0,0)                          CLIENT STATUS BYTES          
         DC    AL1(0)                              EXTRA STATUS                 
         DC    CL30'PST ALLOCATION'                DESCRIPTION                  
*                                                                               
*                                                                               
         DC    CL14'SCM2610BP'                     ACCOUNT                      
         DC    AL1(12)                             L'ACCT TO MATCH              
         DC    AL1(CR)                             STATUS                       
         DC    AL1(3)                              DDS BATCH TYPE               
         DC    CL14'SJ'                            C/A                          
         DC    C'47'                               MCKIM JE NUMBER              
         DC    C'    '                             MCKIM GL ACCOUNT             
         DC    AL1(0,0,0)                          CLIENT STATUS BYTES          
         DC    AL1(0)                              EXTRA STATUS                 
         DC    CL30'PST ALLOCATION'                DESCRIPTION                  
*                                                                               
*                                                                               
         DC    CL14'SCM2610BT'                     ACCOUNT                      
         DC    AL1(12)                             L'ACCT TO MATCH              
         DC    AL1(CR)                             STATUS                       
         DC    AL1(3)                              DDS BATCH TYPE               
         DC    CL14'SJ'                            C/A                          
         DC    C'47'                               MCKIM JE NUMBER              
         DC    C'    '                             MCKIM GL ACCOUNT             
         DC    AL1(0,0,0)                          CLIENT STATUS BYTES          
         DC    AL1(0)                              EXTRA STATUS                 
         DC    CL30'PST ALLOCATION'                DESCRIPTION                  
*                                                                               
*                                                                               
         DC    CL14'SCM2610BV'                     ACCOUNT                      
         DC    AL1(12)                             L'ACCT TO MATCH              
         DC    AL1(CR)                             STATUS                       
         DC    AL1(3)                              DDS BATCH TYPE               
         DC    CL14'SJ'                            C/A                          
         DC    C'47'                               MCKIM JE NUMBER              
         DC    C'    '                             MCKIM GL ACCOUNT             
         DC    AL1(0,0,0)                          CLIENT STATUS BYTES          
         DC    AL1(0)                              EXTRA STATUS                 
         DC    CL30'PST ALLOCATION'                DESCRIPTION                  
*                                                                               
*                                                                               
         DC    CL14'SCM2610BW'                     ACCOUNT                      
         DC    AL1(12)                             L'ACCT TO MATCH              
         DC    AL1(CR)                             STATUS                       
         DC    AL1(3)                              DDS BATCH TYPE               
         DC    CL14'SJ'                            C/A                          
         DC    C'47'                               MCKIM JE NUMBER              
         DC    C'    '                             MCKIM GL ACCOUNT             
         DC    AL1(0,0,0)                          CLIENT STATUS BYTES          
         DC    AL1(0)                              EXTRA STATUS                 
         DC    CL30'PST ALLOCATION'                DESCRIPTION                  
*                                                                               
*                                                                               
         DC    CL14'SCM2610MC'                     ACCOUNT                      
         DC    AL1(12)                             L'ACCT TO MATCH              
         DC    AL1(CR)                             STATUS                       
         DC    AL1(3)                              DDS BATCH TYPE               
         DC    CL14'SJ'                            C/A                          
         DC    C'47'                               MCKIM JE NUMBER              
         DC    C'    '                             MCKIM GL ACCOUNT             
         DC    AL1(0,0,0)                          CLIENT STATUS BYTES          
         DC    AL1(0)                              EXTRA STATUS                 
         DC    CL30'PST ALLOCATION'                DESCRIPTION                  
*                                                                               
*                                                                               
         DC    CL14'SCM2610MP'                     ACCOUNT                      
         DC    AL1(12)                             L'ACCT TO MATCH              
         DC    AL1(CR)                             STATUS                       
         DC    AL1(3)                              DDS BATCH TYPE               
         DC    CL14'SJ'                            C/A                          
         DC    C'47'                               MCKIM JE NUMBER              
         DC    C'    '                             MCKIM GL ACCOUNT             
         DC    AL1(0,0,0)                          CLIENT STATUS BYTES          
         DC    AL1(0)                              EXTRA STATUS                 
         DC    CL30'PST ALLOCATION'                DESCRIPTION                  
*                                                                               
*                                                                               
         DC    CL14'SCM2610MT'                     ACCOUNT                      
         DC    AL1(12)                             L'ACCT TO MATCH              
         DC    AL1(CR)                             STATUS                       
         DC    AL1(3)                              DDS BATCH TYPE               
         DC    CL14'SJ'                            C/A                          
         DC    C'47'                               MCKIM JE NUMBER              
         DC    C'    '                             MCKIM GL ACCOUNT             
         DC    AL1(0,0,0)                          CLIENT STATUS BYTES          
         DC    AL1(0)                              EXTRA STATUS                 
         DC    CL30'PST ALLOCATION'                DESCRIPTION                  
*                                                                               
*                                                                               
         DC    CL14'SCM2610MV'                     ACCOUNT                      
         DC    AL1(12)                             L'ACCT TO MATCH              
         DC    AL1(CR)                             STATUS                       
         DC    AL1(3)                              DDS BATCH TYPE               
         DC    CL14'SJ'                            C/A                          
         DC    C'47'                               MCKIM JE NUMBER              
         DC    C'    '                             MCKIM GL ACCOUNT             
         DC    AL1(0,0,0)                          CLIENT STATUS BYTES          
         DC    AL1(0)                              EXTRA STATUS                 
         DC    CL30'PST ALLOCATION'                DESCRIPTION                  
*                                                                               
*                                                                               
         DC    CL14'SCM2610MW'                     ACCOUNT                      
         DC    AL1(12)                             L'ACCT TO MATCH              
         DC    AL1(CR)                             STATUS                       
         DC    AL1(3)                              DDS BATCH TYPE               
         DC    CL14'SJ'                            C/A                          
         DC    C'47'                               MCKIM JE NUMBER              
         DC    C'    '                             MCKIM GL ACCOUNT             
         DC    AL1(0,0,0)                          CLIENT STATUS BYTES          
         DC    AL1(0)                              EXTRA STATUS                 
         DC    CL30'PST ALLOCATION'                DESCRIPTION                  
*                                                                               
*                                                                               
         DC    CL14'SCM2610OC'                     ACCOUNT                      
         DC    AL1(12)                             L'ACCT TO MATCH              
         DC    AL1(CR)                             STATUS                       
         DC    AL1(3)                              DDS BATCH TYPE               
         DC    CL14'SJ'                            C/A                          
         DC    C'47'                               MCKIM JE NUMBER              
         DC    C'    '                             MCKIM GL ACCOUNT             
         DC    AL1(0,0,0)                          CLIENT STATUS BYTES          
         DC    AL1(0)                              EXTRA STATUS                 
         DC    CL30'PST ALLOCATION'                DESCRIPTION                  
*                                                                               
*                                                                               
         DC    CL14'SCM2610OP'                     ACCOUNT                      
         DC    AL1(12)                             L'ACCT TO MATCH              
         DC    AL1(CR)                             STATUS                       
         DC    AL1(3)                              DDS BATCH TYPE               
         DC    CL14'SJ'                            C/A                          
         DC    C'47'                               MCKIM JE NUMBER              
         DC    C'    '                             MCKIM GL ACCOUNT             
         DC    AL1(0,0,0)                          CLIENT STATUS BYTES          
         DC    AL1(0)                              EXTRA STATUS                 
         DC    CL30'PST ALLOCATION'                DESCRIPTION                  
*                                                                               
*                                                                               
         DC    CL14'SCM2610OT'                     ACCOUNT                      
         DC    AL1(12)                             L'ACCT TO MATCH              
         DC    AL1(CR)                             STATUS                       
         DC    AL1(3)                              DDS BATCH TYPE               
         DC    CL14'SJ'                            C/A                          
         DC    C'47'                               MCKIM JE NUMBER              
         DC    C'    '                             MCKIM GL ACCOUNT             
         DC    AL1(0,0,0)                          CLIENT STATUS BYTES          
         DC    AL1(0)                              EXTRA STATUS                 
         DC    CL30'PST ALLOCATION'                DESCRIPTION                  
*                                                                               
*                                                                               
         DC    CL14'SCM2610OV'                     ACCOUNT                      
         DC    AL1(12)                             L'ACCT TO MATCH              
         DC    AL1(CR)                             STATUS                       
         DC    AL1(3)                              DDS BATCH TYPE               
         DC    CL14'SJ'                            C/A                          
         DC    C'47'                               MCKIM JE NUMBER              
         DC    C'    '                             MCKIM GL ACCOUNT             
         DC    AL1(0,0,0)                          CLIENT STATUS BYTES          
         DC    AL1(0)                              EXTRA STATUS                 
         DC    CL30'PST ALLOCATION'                DESCRIPTION                  
*                                                                               
*                                                                               
         DC    CL14'SCM2610OW'                     ACCOUNT                      
         DC    AL1(12)                             L'ACCT TO MATCH              
         DC    AL1(CR)                             STATUS                       
         DC    AL1(3)                              DDS BATCH TYPE               
         DC    CL14'SJ'                            C/A                          
         DC    C'47'                               MCKIM JE NUMBER              
         DC    C'    '                             MCKIM GL ACCOUNT             
         DC    AL1(0,0,0)                          CLIENT STATUS BYTES          
         DC    AL1(0)                              EXTRA STATUS                 
         DC    CL30'PST ALLOCATION'                DESCRIPTION                  
*                                                                               
*                                                                               
         DC    CL14'SCM2610QC'                     ACCOUNT                      
         DC    AL1(12)                             L'ACCT TO MATCH              
         DC    AL1(CR)                             STATUS                       
         DC    AL1(3)                              DDS BATCH TYPE               
         DC    CL14'SJ'                            C/A                          
         DC    C'47'                               MCKIM JE NUMBER              
         DC    C'    '                             MCKIM GL ACCOUNT             
         DC    AL1(0,0,0)                          CLIENT STATUS BYTES          
         DC    AL1(0)                              EXTRA STATUS                 
         DC    CL30'PST ALLOCATION'                DESCRIPTION                  
*                                                                               
*                                                                               
         DC    CL14'SCM2610QT'                     ACCOUNT                      
         DC    AL1(12)                             L'ACCT TO MATCH              
         DC    AL1(CR)                             STATUS                       
         DC    AL1(3)                              DDS BATCH TYPE               
         DC    CL14'SJ'                            C/A                          
         DC    C'47'                               MCKIM JE NUMBER              
         DC    C'    '                             MCKIM GL ACCOUNT             
         DC    AL1(0,0,0)                          CLIENT STATUS BYTES          
         DC    AL1(0)                              EXTRA STATUS                 
         DC    CL30'PST ALLOCATION'                DESCRIPTION                  
*                                                                               
*                                                                               
         DC    CL14'SCM2610QV'                     ACCOUNT                      
         DC    AL1(12)                             L'ACCT TO MATCH              
         DC    AL1(CR)                             STATUS                       
         DC    AL1(3)                              DDS BATCH TYPE               
         DC    CL14'SJ'                            C/A                          
         DC    C'47'                               MCKIM JE NUMBER              
         DC    C'    '                             MCKIM GL ACCOUNT             
         DC    AL1(0,0,0)                          CLIENT STATUS BYTES          
         DC    AL1(0)                              EXTRA STATUS                 
         DC    CL30'PST ALLOCATION'                DESCRIPTION                  
*                                                                               
*                                                                               
         DC    CL14'SCM2610QW'                     ACCOUNT                      
         DC    AL1(12)                             L'ACCT TO MATCH              
         DC    AL1(CR)                             STATUS                       
         DC    AL1(3)                              DDS BATCH TYPE               
         DC    CL14'SJ'                            C/A                          
         DC    C'47'                               MCKIM JE NUMBER              
         DC    C'    '                             MCKIM GL ACCOUNT             
         DC    AL1(0,0,0)                          CLIENT STATUS BYTES          
         DC    AL1(0)                              EXTRA STATUS                 
         DC    CL30'PST ALLOCATION'                DESCRIPTION                  
*                                                                               
*                                                                               
         DC    CL14'SCL2610A'                      ACCOUNT                      
         DC    AL1(12)                             L'ACCT TO MATCH              
         DC    AL1(CR)                             STATUS                       
         DC    AL1(3)                              DDS BATCH TYPE               
         DC    CL14'SJ'                            C/A                          
         DC    C'47'                               MCKIM JE NUMBER              
         DC    C'    '                             MCKIM GL ACCOUNT             
         DC    AL1(0,0,0)                          CLIENT STATUS BYTES          
         DC    AL1(0)                              EXTRA STATUS                 
         DC    CL30'PST ALLOCATION'                DESCRIPTION                  
*                                                                               
*                                                                               
         DC    CL14'SCL2610B'                      ACCOUNT                      
         DC    AL1(12)                             L'ACCT TO MATCH              
         DC    AL1(CR)                             STATUS                       
         DC    AL1(3)                              DDS BATCH TYPE               
         DC    CL14'SJ'                            C/A                          
         DC    C'47'                               MCKIM JE NUMBER              
         DC    C'    '                             MCKIM GL ACCOUNT             
         DC    AL1(0,0,0)                          CLIENT STATUS BYTES          
         DC    AL1(0)                              EXTRA STATUS                 
         DC    CL30'PST ALLOCATION'                DESCRIPTION                  
*                                                                               
*                                                                               
         DC    CL14'SCL2610M'                      ACCOUNT                      
         DC    AL1(12)                             L'ACCT TO MATCH              
         DC    AL1(CR)                             STATUS                       
         DC    AL1(3)                              DDS BATCH TYPE               
         DC    CL14'SJ'                            C/A                          
         DC    C'47'                               MCKIM JE NUMBER              
         DC    C'    '                             MCKIM GL ACCOUNT             
         DC    AL1(0,0,0)                          CLIENT STATUS BYTES          
         DC    AL1(0)                              EXTRA STATUS                 
         DC    CL30'PST ALLOCATION'                DESCRIPTION                  
*                                                                               
*                                                                               
         DC    CL14'SCL2610O'                      ACCOUNT                      
         DC    AL1(12)                             L'ACCT TO MATCH              
         DC    AL1(CR)                             STATUS                       
         DC    AL1(3)                              DDS BATCH TYPE               
         DC    CL14'SJ'                            C/A                          
         DC    C'47'                               MCKIM JE NUMBER              
         DC    C'    '                             MCKIM GL ACCOUNT             
         DC    AL1(0,0,0)                          CLIENT STATUS BYTES          
         DC    AL1(0)                              EXTRA STATUS                 
         DC    CL30'PST ALLOCATION'                DESCRIPTION                  
*                                                                               
*                                                                               
         DC    CL14'SCL2610Q'                      ACCOUNT                      
         DC    AL1(12)                             L'ACCT TO MATCH              
         DC    AL1(CR)                             STATUS                       
         DC    AL1(3)                              DDS BATCH TYPE               
         DC    CL14'SJ'                            C/A                          
         DC    C'47'                               MCKIM JE NUMBER              
         DC    C'    '                             MCKIM GL ACCOUNT             
         DC    AL1(0,0,0)                          CLIENT STATUS BYTES          
         DC    AL1(0)                              EXTRA STATUS                 
         DC    CL30'PST ALLOCATION'                DESCRIPTION                  
*                                                                               
*                                                                               
         DC    CL14'SCS2610AE'                     ACCOUNT                      
         DC    AL1(12)                             L'ACCT TO MATCH              
         DC    AL1(CR)                             STATUS                       
         DC    AL1(3)                              DDS BATCH TYPE               
         DC    CL14'SJ'                            C/A                          
         DC    C'47'                               MCKIM JE NUMBER              
         DC    C'    '                             MCKIM GL ACCOUNT             
         DC    AL1(0,0,0)                          CLIENT STATUS BYTES          
         DC    AL1(0)                              EXTRA STATUS                 
         DC    CL30'PST ALLOCATION'                DESCRIPTION                  
*                                                                               
*                                                                               
         DC    CL14'SCS2610AF'                     ACCOUNT                      
         DC    AL1(12)                             L'ACCT TO MATCH              
         DC    AL1(CR)                             STATUS                       
         DC    AL1(3)                              DDS BATCH TYPE               
         DC    CL14'SJ'                            C/A                          
         DC    C'47'                               MCKIM JE NUMBER              
         DC    C'    '                             MCKIM GL ACCOUNT             
         DC    AL1(0,0,0)                          CLIENT STATUS BYTES          
         DC    AL1(0)                              EXTRA STATUS                 
         DC    CL30'PST ALLOCATION'                DESCRIPTION                  
*                                                                               
*                                                                               
         DC    CL14'SCS2610BE'                     ACCOUNT                      
         DC    AL1(12)                             L'ACCT TO MATCH              
         DC    AL1(CR)                             STATUS                       
         DC    AL1(3)                              DDS BATCH TYPE               
         DC    CL14'SJ'                            C/A                          
         DC    C'47'                               MCKIM JE NUMBER              
         DC    C'    '                             MCKIM GL ACCOUNT             
         DC    AL1(0,0,0)                          CLIENT STATUS BYTES          
         DC    AL1(0)                              EXTRA STATUS                 
         DC    CL30'PST ALLOCATION'                DESCRIPTION                  
*                                                                               
*                                                                               
         DC    CL14'SCS2610BF'                     ACCOUNT                      
         DC    AL1(12)                             L'ACCT TO MATCH              
         DC    AL1(CR)                             STATUS                       
         DC    AL1(3)                              DDS BATCH TYPE               
         DC    CL14'SJ'                            C/A                          
         DC    C'47'                               MCKIM JE NUMBER              
         DC    C'    '                             MCKIM GL ACCOUNT             
         DC    AL1(0,0,0)                          CLIENT STATUS BYTES          
         DC    AL1(0)                              EXTRA STATUS                 
         DC    CL30'PST ALLOCATION'                DESCRIPTION                  
*                                                                               
*                                                                               
         DC    CL14'SCS2610ME'                     ACCOUNT                      
         DC    AL1(12)                             L'ACCT TO MATCH              
         DC    AL1(CR)                             STATUS                       
         DC    AL1(3)                              DDS BATCH TYPE               
         DC    CL14'SJ'                            C/A                          
         DC    C'47'                               MCKIM JE NUMBER              
         DC    C'    '                             MCKIM GL ACCOUNT             
         DC    AL1(0,0,0)                          CLIENT STATUS BYTES          
         DC    AL1(0)                              EXTRA STATUS                 
         DC    CL30'PST ALLOCATION'                DESCRIPTION                  
*                                                                               
*                                                                               
         DC    CL14'SCS2610MF'                     ACCOUNT                      
         DC    AL1(12)                             L'ACCT TO MATCH              
         DC    AL1(CR)                             STATUS                       
         DC    AL1(3)                              DDS BATCH TYPE               
         DC    CL14'SJ'                            C/A                          
         DC    C'47'                               MCKIM JE NUMBER              
         DC    C'    '                             MCKIM GL ACCOUNT             
         DC    AL1(0,0,0)                          CLIENT STATUS BYTES          
         DC    AL1(0)                              EXTRA STATUS                 
         DC    CL30'PST ALLOCATION'                DESCRIPTION                  
*                                                                               
*                                                                               
         DC    CL14'SCS2610OE'                     ACCOUNT                      
         DC    AL1(12)                             L'ACCT TO MATCH              
         DC    AL1(CR)                             STATUS                       
         DC    AL1(3)                              DDS BATCH TYPE               
         DC    CL14'SJ'                            C/A                          
         DC    C'47'                               MCKIM JE NUMBER              
         DC    C'    '                             MCKIM GL ACCOUNT             
         DC    AL1(0,0,0)                          CLIENT STATUS BYTES          
         DC    AL1(0)                              EXTRA STATUS                 
         DC    CL30'PST ALLOCATION'                DESCRIPTION                  
*                                                                               
*                                                                               
         DC    CL14'SCS2610OF'                     ACCOUNT                      
         DC    AL1(12)                             L'ACCT TO MATCH              
         DC    AL1(CR)                             STATUS                       
         DC    AL1(3)                              DDS BATCH TYPE               
         DC    CL14'SJ'                            C/A                          
         DC    C'47'                               MCKIM JE NUMBER              
         DC    C'    '                             MCKIM GL ACCOUNT             
         DC    AL1(0,0,0)                          CLIENT STATUS BYTES          
         DC    AL1(0)                              EXTRA STATUS                 
         DC    CL30'PST ALLOCATION'                DESCRIPTION                  
*                                                                               
*                                                                               
         DC    CL14'SCS2610QE'                     ACCOUNT                      
         DC    AL1(12)                             L'ACCT TO MATCH              
         DC    AL1(CR)                             STATUS                       
         DC    AL1(3)                              DDS BATCH TYPE               
         DC    CL14'SJ'                            C/A                          
         DC    C'47'                               MCKIM JE NUMBER              
         DC    C'    '                             MCKIM GL ACCOUNT             
         DC    AL1(0,0,0)                          CLIENT STATUS BYTES          
         DC    AL1(0)                              EXTRA STATUS                 
         DC    CL30'PST ALLOCATION'                DESCRIPTION                  
*                                                                               
*                                                                               
         DC    CL14'SCS2610QF'                     ACCOUNT                      
         DC    AL1(12)                             L'ACCT TO MATCH              
         DC    AL1(CR)                             STATUS                       
         DC    AL1(3)                              DDS BATCH TYPE               
         DC    CL14'SJ'                            C/A                          
         DC    C'47'                               MCKIM JE NUMBER              
         DC    C'    '                             MCKIM GL ACCOUNT             
         DC    AL1(0,0,0)                          CLIENT STATUS BYTES          
         DC    AL1(0)                              EXTRA STATUS                 
         DC    CL30'PST ALLOCATION'                DESCRIPTION                  
*                                                                               
*                                                                               
         DC    CL14'SCU2610AN'                     ACCOUNT                      
         DC    AL1(12)                             L'ACCT TO MATCH              
         DC    AL1(CR)                             STATUS                       
         DC    AL1(3)                              DDS BATCH TYPE               
         DC    CL14'SJ'                            C/A                          
         DC    C'47'                               MCKIM JE NUMBER              
         DC    C'    '                             MCKIM GL ACCOUNT             
         DC    AL1(0,0,0)                          CLIENT STATUS BYTES          
         DC    AL1(0)                              EXTRA STATUS                 
         DC    CL30'PST ALLOCATION'                DESCRIPTION                  
*                                                                               
*                                                                               
         DC    CL14'SCU2610AG'                     ACCOUNT                      
         DC    AL1(12)                             L'ACCT TO MATCH              
         DC    AL1(CR)                             STATUS                       
         DC    AL1(3)                              DDS BATCH TYPE               
         DC    CL14'SJ'                            C/A                          
         DC    C'47'                               MCKIM JE NUMBER              
         DC    C'    '                             MCKIM GL ACCOUNT             
         DC    AL1(0,0,0)                          CLIENT STATUS BYTES          
         DC    AL1(0)                              EXTRA STATUS                 
         DC    CL30'PST ALLOCATION'                DESCRIPTION                  
*                                                                               
*                                                                               
         DC    CL14'SCU2610BN'                     ACCOUNT                      
         DC    AL1(12)                             L'ACCT TO MATCH              
         DC    AL1(CR)                             STATUS                       
         DC    AL1(3)                              DDS BATCH TYPE               
         DC    CL14'SJ'                            C/A                          
         DC    C'47'                               MCKIM JE NUMBER              
         DC    C'    '                             MCKIM GL ACCOUNT             
         DC    AL1(0,0,0)                          CLIENT STATUS BYTES          
         DC    AL1(0)                              EXTRA STATUS                 
         DC    CL30'PST ALLOCATION'                DESCRIPTION                  
*                                                                               
*                                                                               
         DC    CL14'SCU2610BG'                     ACCOUNT                      
         DC    AL1(12)                             L'ACCT TO MATCH              
         DC    AL1(CR)                             STATUS                       
         DC    AL1(3)                              DDS BATCH TYPE               
         DC    CL14'SJ'                            C/A                          
         DC    C'47'                               MCKIM JE NUMBER              
         DC    C'    '                             MCKIM GL ACCOUNT             
         DC    AL1(0,0,0)                          CLIENT STATUS BYTES          
         DC    AL1(0)                              EXTRA STATUS                 
         DC    CL30'PST ALLOCATION'                DESCRIPTION                  
*                                                                               
*                                                                               
         DC    CL14'SCU2610MN'                     ACCOUNT                      
         DC    AL1(12)                             L'ACCT TO MATCH              
         DC    AL1(CR)                             STATUS                       
         DC    AL1(3)                              DDS BATCH TYPE               
         DC    CL14'SJ'                            C/A                          
         DC    C'47'                               MCKIM JE NUMBER              
         DC    C'    '                             MCKIM GL ACCOUNT             
         DC    AL1(0,0,0)                          CLIENT STATUS BYTES          
         DC    AL1(0)                              EXTRA STATUS                 
         DC    CL30'PST ALLOCATION'                DESCRIPTION                  
*                                                                               
*                                                                               
         DC    CL14'SCU2610MG'                     ACCOUNT                      
         DC    AL1(12)                             L'ACCT TO MATCH              
         DC    AL1(CR)                             STATUS                       
         DC    AL1(3)                              DDS BATCH TYPE               
         DC    CL14'SJ'                            C/A                          
         DC    C'47'                               MCKIM JE NUMBER              
         DC    C'    '                             MCKIM GL ACCOUNT             
         DC    AL1(0,0,0)                          CLIENT STATUS BYTES          
         DC    AL1(0)                              EXTRA STATUS                 
         DC    CL30'PST ALLOCATION'                DESCRIPTION                  
*                                                                               
*                                                                               
         DC    CL14'SCU2610ON'                     ACCOUNT                      
         DC    AL1(12)                             L'ACCT TO MATCH              
         DC    AL1(CR)                             STATUS                       
         DC    AL1(3)                              DDS BATCH TYPE               
         DC    CL14'SJ'                            C/A                          
         DC    C'47'                               MCKIM JE NUMBER              
         DC    C'    '                             MCKIM GL ACCOUNT             
         DC    AL1(0,0,0)                          CLIENT STATUS BYTES          
         DC    AL1(0)                              EXTRA STATUS                 
         DC    CL30'PST ALLOCATION'                DESCRIPTION                  
*                                                                               
*                                                                               
         DC    CL14'SCU2610OG'                     ACCOUNT                      
         DC    AL1(12)                             L'ACCT TO MATCH              
         DC    AL1(CR)                             STATUS                       
         DC    AL1(3)                              DDS BATCH TYPE               
         DC    CL14'SJ'                            C/A                          
         DC    C'47'                               MCKIM JE NUMBER              
         DC    C'    '                             MCKIM GL ACCOUNT             
         DC    AL1(0,0,0)                          CLIENT STATUS BYTES          
         DC    AL1(0)                              EXTRA STATUS                 
         DC    CL30'PST ALLOCATION'                DESCRIPTION                  
*                                                                               
*                                                                               
         DC    CL14'SCU2610QN'                     ACCOUNT                      
         DC    AL1(12)                             L'ACCT TO MATCH              
         DC    AL1(CR)                             STATUS                       
         DC    AL1(3)                              DDS BATCH TYPE               
         DC    CL14'SJ'                            C/A                          
         DC    C'47'                               MCKIM JE NUMBER              
         DC    C'    '                             MCKIM GL ACCOUNT             
         DC    AL1(0,0,0)                          CLIENT STATUS BYTES          
         DC    AL1(0)                              EXTRA STATUS                 
         DC    CL30'PST ALLOCATION'                DESCRIPTION                  
*                                                                               
*                                                                               
         DC    CL14'SCU2610QG'                     ACCOUNT                      
         DC    AL1(12)                             L'ACCT TO MATCH              
         DC    AL1(CR)                             STATUS                       
         DC    AL1(3)                              DDS BATCH TYPE               
         DC    CL14'SJ'                            C/A                          
         DC    C'47'                               MCKIM JE NUMBER              
         DC    C'    '                             MCKIM GL ACCOUNT             
         DC    AL1(0,0,0)                          CLIENT STATUS BYTES          
         DC    AL1(0)                              EXTRA STATUS                 
         DC    CL30'PST ALLOCATION'                DESCRIPTION                  
*                                                                               
*                                                                               
         DC    CL14'SCV2610AR'                     ACCOUNT                      
         DC    AL1(12)                             L'ACCT TO MATCH              
         DC    AL1(CR)                             STATUS                       
         DC    AL1(3)                              DDS BATCH TYPE               
         DC    CL14'SJ'                            C/A                          
         DC    C'47'                               MCKIM JE NUMBER              
         DC    C'    '                             MCKIM GL ACCOUNT             
         DC    AL1(0,0,0)                          CLIENT STATUS BYTES          
         DC    AL1(0)                              EXTRA STATUS                 
         DC    CL30'PST ALLOCATION'                DESCRIPTION                  
*                                                                               
*                                                                               
         DC    CL14'SCV2610BR'                     ACCOUNT                      
         DC    AL1(12)                             L'ACCT TO MATCH              
         DC    AL1(CR)                             STATUS                       
         DC    AL1(3)                              DDS BATCH TYPE               
         DC    CL14'SJ'                            C/A                          
         DC    C'47'                               MCKIM JE NUMBER              
         DC    C'    '                             MCKIM GL ACCOUNT             
         DC    AL1(0,0,0)                          CLIENT STATUS BYTES          
         DC    AL1(0)                              EXTRA STATUS                 
         DC    CL30'PST ALLOCATION'                DESCRIPTION                  
*                                                                               
*                                                                               
         DC    CL14'SCV2610MR'                     ACCOUNT                      
         DC    AL1(12)                             L'ACCT TO MATCH              
         DC    AL1(CR)                             STATUS                       
         DC    AL1(3)                              DDS BATCH TYPE               
         DC    CL14'SJ'                            C/A                          
         DC    C'47'                               MCKIM JE NUMBER              
         DC    C'    '                             MCKIM GL ACCOUNT             
         DC    AL1(0,0,0)                          CLIENT STATUS BYTES          
         DC    AL1(0)                              EXTRA STATUS                 
         DC    CL30'PST ALLOCATION'                DESCRIPTION                  
*                                                                               
*                                                                               
         DC    CL14'SCV2610OR'                     ACCOUNT                      
         DC    AL1(12)                             L'ACCT TO MATCH              
         DC    AL1(CR)                             STATUS                       
         DC    AL1(3)                              DDS BATCH TYPE               
         DC    CL14'SJ'                            C/A                          
         DC    C'47'                               MCKIM JE NUMBER              
         DC    C'    '                             MCKIM GL ACCOUNT             
         DC    AL1(0,0,0)                          CLIENT STATUS BYTES          
         DC    AL1(0)                              EXTRA STATUS                 
         DC    CL30'PST ALLOCATION'                DESCRIPTION                  
*                                                                               
*                                                                               
         DC    CL14'SCV2610QR'                     ACCOUNT                      
         DC    AL1(12)                             L'ACCT TO MATCH              
         DC    AL1(CR)                             STATUS                       
         DC    AL1(3)                              DDS BATCH TYPE               
         DC    CL14'SJ'                            C/A                          
         DC    C'47'                               MCKIM JE NUMBER              
         DC    C'    '                             MCKIM GL ACCOUNT             
         DC    AL1(0,0,0)                          CLIENT STATUS BYTES          
         DC    AL1(0)                              EXTRA STATUS                 
         DC    CL30'PST ALLOCATION'                DESCRIPTION                  
*                                                                               
*                                                                               
         DC    CL14'SC'                            ACCOUNT                      
         DC    AL1(0)                              L'ACCT TO MATCH              
         DC    AL1(CR)                             STATUS                       
         DC    AL1(3)                              DDS BATCH TYPE               
         DC    CL14'SJ'                            C/A                          
         DC    C'46'                               MCKIM JE NUMBER              
         DC    C'    '                             MCKIM GL ACCOUNT             
         DC    AL1(0,0,0)                          CLIENT STATUS BYTES          
         DC    AL1(0)                              EXTRA STATUS                 
         DC    CL30'DOCKET CLEARING'               DESCRIPTION                  
*                                                                               
*                                                                               
         DC    CL14'SC'                            ACCOUNT                      
         DC    AL1(0)                              L'ACCT TO MATCH              
         DC    AL1(CR)                             STATUS                       
         DC    AL1(33)                             DDS BATCH TYPE               
         DC    CL14' '                             C/A                          
         DC    C'23'                               MCKIM JE NUMBER              
         DC    C'    '                             MCKIM GL ACCOUNT             
         DC    AL1(0,0,0)                          CLIENT STATUS BYTES          
         DC    AL1(0)                              EXTRA STATUS                 
         DC    CL30'CLEARANCES'                    DESCRIPTION                  
*                                                                               
*                                                                               
         DC    CL14'SC'                            ACCOUNT                      
         DC    AL1(0)                              L'ACCT TO MATCH              
         DC    AL1(CR)                             STATUS                       
         DC    AL1(34)                             DDS BATCH TYPE               
         DC    CL14' '                             C/A                          
         DC    C'23'                               MCKIM JE NUMBER              
         DC    C'    '                             MCKIM GL ACCOUNT             
         DC    AL1(0,0,0)                          CLIENT STATUS BYTES          
         DC    AL1(0)                              EXTRA STATUS                 
         DC    CL30'CLEARANCES'                    DESCRIPTION                  
*                                                                               
*                                                                               
         DC    CL14'SC'                            ACCOUNT                      
         DC    AL1(0)                              L'ACCT TO MATCH              
         DC    AL1(CR)                             STATUS                       
         DC    AL1(36)                             DDS BATCH TYPE               
         DC    CL14' '                             C/A                          
         DC    C'31'                               MCKIM JE NUMBER              
         DC    C'    '                             MCKIM GL ACCOUNT             
         DC    AL1(0,0,0)                          CLIENT STATUS BYTES          
         DC    AL1(0)                              EXTRA STATUS                 
         DC    CL30'MANUAL CHEQUE'                 DESCRIPTION                  
*                                                                               
*                                                                               
         DC    CL14'SC'                            ACCOUNT                      
         DC    AL1(0)                              L'ACCT TO MATCH              
         DC    AL1(DR+CR)                          STATUS                       
         DC    AL1(37)                             DDS BATCH TYPE               
         DC    CL14' '                             C/A                          
         DC    C'30'                               MCKIM JE NUMBER              
         DC    C'    '                             MCKIM GL ACCOUNT             
         DC    AL1(0,0,0)                          CLIENT STATUS BYTES          
         DC    AL1(SCCODE)                         EXTRA STATUS                 
         DC    CL30'CANCELLED CHEQUES'             DESCRIPTION                  
*                                                                               
*                                                                               
         DC    CL14'SCM2340'                       ACCOUNT                      
         DC    AL1(12)                             L'ACCT TO MATCH              
         DC    AL1(CR)                             STATUS                       
         DC    AL1(45)                             DDS BATCH TYPE               
         DC    CL14' '                             C/A                          
         DC    C'29'                               MCKIM JE NUMBER              
         DC    C'    '                             MCKIM GL ACCOUNT             
         DC    AL1(0,0,0)                          CLIENT STATUS BYTES          
         DC    AL1(0)                              EXTRA STATUS                 
         DC    CL30'CD LOST/REFUNDS'               DESCRIPTION                  
*                                                                               
*                                                                               
         DC    CL14'SCCC06M'                       ACCOUNT                      
         DC    AL1(12)                             L'ACCT TO MATCH              
         DC    AL1(CR)                             STATUS                       
         DC    AL1(45)                             DDS BATCH TYPE               
         DC    CL14'SCM2340'                       C/A                          
         DC    C'29'                               MCKIM JE NUMBER              
         DC    C'    '                             MCKIM GL ACCOUNT             
         DC    AL1(0,0,0)                          CLIENT STATUS BYTES          
         DC    AL1(0)                              EXTRA STATUS                 
         DC    CL30'CD LOST/REFUNDS'               DESCRIPTION                  
*                                                                               
*                                                                               
         DC    CL14'SCCC06N'                       ACCOUNT                      
         DC    AL1(12)                             L'ACCT TO MATCH              
         DC    AL1(CR)                             STATUS                       
         DC    AL1(45)                             DDS BATCH TYPE               
         DC    CL14'SCM2340'                       C/A                          
         DC    C'29'                               MCKIM JE NUMBER              
         DC    C'    '                             MCKIM GL ACCOUNT             
         DC    AL1(0,0,0)                          CLIENT STATUS BYTES          
         DC    AL1(0)                              EXTRA STATUS                 
         DC    CL30'CD LOST/REFUNDS'               DESCRIPTION                  
*                                                                               
*                                                                               
         DC    CL14'SCCC06O'                       ACCOUNT                      
         DC    AL1(12)                             L'ACCT TO MATCH              
         DC    AL1(CR)                             STATUS                       
         DC    AL1(45)                             DDS BATCH TYPE               
         DC    CL14'SCM2340'                       C/A                          
         DC    C'29'                               MCKIM JE NUMBER              
         DC    C'    '                             MCKIM GL ACCOUNT             
         DC    AL1(0,0,0)                          CLIENT STATUS BYTES          
         DC    AL1(0)                              EXTRA STATUS                 
         DC    CL30'CD LOST/REFUNDS'               DESCRIPTION                  
*                                                                               
*                                                                               
         DC    CL14'SC'                            ACCOUNT                      
         DC    AL1(0)                              L'ACCT TO MATCH              
         DC    AL1(CR)                             STATUS                       
         DC    AL1(45)                             DDS BATCH TYPE               
         DC    CL14'SQ'                            C/A                          
         DC    C'28'                               MCKIM JE NUMBER              
         DC    C'6830'                             MCKIM GL ACCOUNT             
         DC    AL1(0,0,0)                          CLIENT STATUS BYTES          
         DC    AL1(0)                              EXTRA STATUS                 
         DC    CL30'FOREIGN ADJUSTMENT'            DESCRIPTION                  
*                                                                               
*                                                                               
         DC    CL14'SC'                            ACCOUNT                      
         DC    AL1(0)                              L'ACCT TO MATCH              
         DC    AL1(CR)                             STATUS                       
         DC    AL1(49)                             DDS BATCH TYPE               
         DC    CL14' '                             C/A                          
         DC    C'27'                               MCKIM JE NUMBER              
         DC    C'    '                             MCKIM GL ACCOUNT             
         DC    AL1(0,0,0)                          CLIENT STATUS BYTES          
         DC    AL1(0)                              EXTRA STATUS                 
         DC    CL30'CD LOST/REFUNDS'               DESCRIPTION                  
*                                                                               
*                                                                               
         DC    CL14'SC'                            ACCOUNT                      
         DC    AL1(0)                              L'ACCT TO MATCH              
         DC    AL1(CR)                             STATUS                       
         DC    AL1(50)                             DDS BATCH TYPE               
         DC    CL14' '                             C/A                          
         DC    C'27'                               MCKIM JE NUMBER              
         DC    C'    '                             MCKIM GL ACCOUNT             
         DC    AL1(0,0,0)                          CLIENT STATUS BYTES          
         DC    AL1(0)                              EXTRA STATUS                 
         DC    CL30'CD LOST/REFUNDS'               DESCRIPTION                  
*                                                                               
*                                                                               
         DC    CL14'SC'                            ACCOUNT                      
         DC    AL1(0)                              L'ACCT TO MATCH              
         DC    AL1(DR)                             STATUS                       
         DC    AL1(0)                              DDS BATCH TYPE               
         DC    CL14' '                             C/A                          
         DC    C'21'                               MCKIM JE NUMBER              
         DC    C'    '                             MCKIM GL ACCOUNT             
         DC    AL1(0,0,0)                          CLIENT STATUS BYTES          
         DC    AL1(SCCODE)                         EXTRA STATUS                 
         DC    CL30'CASH RECEIPTS'                 DESCRIPTION                  
*                                                                               
*                                                                               
         DC    CL14'SC'                            ACCOUNT                      
         DC    AL1(0)                              L'ACCT TO MATCH              
         DC    AL1(CR)                             STATUS                       
         DC    AL1(0)                              DDS BATCH TYPE               
         DC    CL14' '                             C/A                          
         DC    C'22'                               MCKIM JE NUMBER              
         DC    C'    '                             MCKIM GL ACCOUNT             
         DC    AL1(0,0,0)                          CLIENT STATUS BYTES          
         DC    AL1(SCCODE)                         EXTRA STATUS                 
         DC    CL30'CHEQUE PAYMENTS'               DESCRIPTION                  
         EJECT ,                                                                
******************************************************************              
*              U/L=SE                                            *              
******************************************************************              
*                                                                               
         DC    CL14'SE'                            ACCOUNT                      
         DC    AL1(0)                              L'ACCT TO MATCH              
         DC    AL1(DR)                             STATUS                       
         DC    AL1(14)                             DDS BATCH TYPE               
         DC    CL14'SJ'                            C/A                          
         DC    C'49'                               MCKIM JE NUMBER              
         DC    C'    '                             MCKIM GL ACCOUNT             
         DC    AL1(SJ24,0,0)                       CLIENT STATUS BYTES          
         DC    AL1(ACCNAME)                        EXTRA STATUS                 
         DC    CL30'               - DOCKET TO EXP'    DESCRIPTION              
*                                                                               
*                                                                               
         DC    CL14'SE'                            ACCOUNT                      
         DC    AL1(0)                              L'ACCT TO MATCH              
         DC    AL1(DR)                             STATUS                       
         DC    AL1(15)                             DDS BATCH TYPE               
         DC    CL14'SJ'                            C/A                          
         DC    C'48'                               MCKIM JE NUMBER              
         DC    C'    '                             MCKIM GL ACCOUNT             
         DC    AL1(SJ24,0,0)                       CLIENT STATUS BYTES          
         DC    AL1(ACCNAME)                        EXTRA STATUS                 
         DC    CL30'               - EXP TO DOCKET'    DESCRIPTION              
*                                                                               
*                                                                               
         DC    CL14'SE2025'                        ACCOUNT                      
         DC    AL1(12)                             L'ACCT TO MATCH              
         DC    AL1(CR)                             STATUS                       
         DC    AL1(45)                             DDS BATCH TYPE               
         DC    CL14'SQ'                            C/A                          
         DC    C'75'                               MCKIM JE NUMBER              
         DC    C'2025'                             MCKIM GL ACCOUNT             
         DC    AL1(0,0,0)                          CLIENT STATUS BYTES          
         DC    AL1(0)                              EXTRA STATUS                 
         DC    CL30'PAYABLES OFFSETS'              DESCRIPTION                  
*                                                                               
*                                                                               
         DC    CL14'SE2025'                        ACCOUNT                      
         DC    AL1(12)                             L'ACCT TO MATCH              
         DC    AL1(DR+CR)                          STATUS                       
         DC    AL1(45)                             DDS BATCH TYPE               
         DC    CL14'ST'                            C/A                          
         DC    C'75'                               MCKIM JE NUMBER              
         DC    C'2025'                             MCKIM GL ACCOUNT             
         DC    AL1(0,0,0)                          CLIENT STATUS BYTES          
         DC    AL1(0)                              EXTRA STATUS                 
         DC    CL30'PAYABLES OFFSETS'              DESCRIPTION                  
*                                                                               
*                                                                               
         DC    CL14'SE2026'                        ACCOUNT                      
         DC    AL1(12)                             L'ACCT TO MATCH              
         DC    AL1(CR)                             STATUS                       
         DC    AL1(45)                             DDS BATCH TYPE               
         DC    CL14'SP'                            C/A                          
         DC    C'75'                               MCKIM JE NUMBER              
         DC    C'2026'                             MCKIM GL ACCOUNT             
         DC    AL1(0,0,0)                          CLIENT STATUS BYTES          
         DC    AL1(0)                              EXTRA STATUS                 
         DC    CL30'PAYABLES OFFSETS'              DESCRIPTION                  
*                                                                               
*                                                                               
         DC    CL14'SE2026'                        ACCOUNT                      
         DC    AL1(12)                             L'ACCT TO MATCH              
         DC    AL1(DR+CR)                          STATUS                       
         DC    AL1(45)                             DDS BATCH TYPE               
         DC    CL14'SS'                            C/A                          
         DC    C'75'                               MCKIM JE NUMBER              
         DC    C'2026'                             MCKIM GL ACCOUNT             
         DC    AL1(0,0,0)                          CLIENT STATUS BYTES          
         DC    AL1(0)                              EXTRA STATUS                 
         DC    CL30'PAYABLES OFFSETS'              DESCRIPTION                  
*                                                                               
*                                                                               
         DC    CL14'SE6789'                        ACCOUNT                      
         DC    AL1(12)                             L'ACCT TO MATCH              
         DC    AL1(CR)                             STATUS                       
         DC    AL1(20)                             DDS BATCH TYPE               
         DC    CL14'SR'                            C/A                          
         DC    C'53'                               MCKIM JE NUMBER              
         DC    C'6789'                             MCKIM GL ACCOUNT             
         DC    AL1(CNTR,2,6)                       CLIENT STATUS BYTES          
         DC    AL1(0)                              EXTRA STATUS                 
         DC    CL30'FOCUS REC''B BAL. FWD.'        DESCRIPTION                  
*                                                                               
*                                                                               
         DC    CL14'SE5740'                        ACCOUNT                      
         DC    AL1(12)                             L'ACCT TO MATCH              
         DC    AL1(CR)                             STATUS                       
         DC    AL1(26)                             DDS BATCH TYPE               
         DC    CL14' '                             C/A                          
         DC    C'6 '                               MCKIM JE NUMBER              
         DC    C'5740'                             MCKIM GL ACCOUNT             
         DC    AL1(SJ24,0,0)                       CLIENT STATUS BYTES          
         DC    AL1(0)                              EXTRA STATUS                 
         DC    CL30'ACCTS RCVBLE W/O ADJ.'         DESCRIPTION                  
*                                                                               
*                                                                               
         DC    CL14'SE6880'                        ACCOUNT                      
         DC    AL1(12)                             L'ACCT TO MATCH              
         DC    AL1(CR)                             STATUS                       
         DC    AL1(26)                             DDS BATCH TYPE               
         DC    CL14' '                             C/A                          
         DC    C'35'                               MCKIM JE NUMBER              
         DC    C'6880'                             MCKIM GL ACCOUNT             
         DC    AL1(SJ24,0,0)                       CLIENT STATUS BYTES          
         DC    AL1(0)                              EXTRA STATUS                 
         DC    CL30'BAD DEBT W/O'                  DESCRIPTION                  
*                                                                               
*                                                                               
         DC    CL14'SE1220'                        ACCOUNT                      
         DC    AL1(12)                             L'ACCT TO MATCH              
         DC    AL1(DR)                             STATUS                       
         DC    AL1(30)                             DDS BATCH TYPE               
         DC    CL14'SR'                            C/A                          
         DC    C'53'                               MCKIM JE NUMBER              
         DC    C'1220'                             MCKIM GL ACCOUNT             
         DC    AL1(CNTR,2,6)                       CLIENT STATUS BYTES          
         DC    AL1(0)                              EXTRA STATUS                 
         DC    CL30'AR ADJUSTMENTS'                DESCRIPTION                  
*                                                                               
*                                                                               
         DC    CL14'SE6812'                        ACCOUNT                      
         DC    AL1(12)                             L'ACCT TO MATCH              
         DC    AL1(DR)                             STATUS                       
         DC    AL1(30)                             DDS BATCH TYPE               
         DC    CL14'SR'                            C/A                          
         DC    C'26'                               MCKIM JE NUMBER              
         DC    C'1245'                             MCKIM GL ACCOUNT             
         DC    AL1(CNTR,2,6)                       CLIENT STATUS BYTES          
         DC    AL1(0)                              EXTRA STATUS                 
         DC    CL30'WRITE-OFF I/CO BILLING - WCD'  DESCRIPTION                  
*                                                                               
*                                                                               
         DC    CL14'SE6818'                        ACCOUNT                      
         DC    AL1(12)                             L'ACCT TO MATCH              
         DC    AL1(DR+CR)                          STATUS                       
         DC    AL1(30)                             DDS BATCH TYPE               
         DC    CL14'SR'                            C/A                          
         DC    C'26'                               MCKIM JE NUMBER              
         DC    C'6818'                             MCKIM GL ACCOUNT             
         DC    AL1(CNTR,2,6)                       CLIENT STATUS BYTES          
         DC    AL1(0)                              EXTRA STATUS                 
         DC    CL30'AR SMALL BAL WRITE-OFFS'       DESCRIPTION                  
*                                                                               
         DC    CL14'SE6818'                        ACCOUNT                      
         DC    AL1(12)                             L'ACCT TO MATCH              
         DC    AL1(DR+CR)                          STATUS                       
         DC    AL1(45)                             DDS BATCH TYPE               
         DC    CL14'SR'                            C/A                          
         DC    C'6 '                               MCKIM JE NUMBER              
         DC    C'6818'                             MCKIM GL ACCOUNT             
         DC    AL1(CNTR,2,6)                       CLIENT STATUS BYTES          
         DC    AL1(0)                              EXTRA STATUS                 
         DC    CL30'ACCT RCVBLE W/O ADJ'           DESCRIPTION                  
*                                                                               
*                                                                               
         DC    CL14'SE6830'                        ACCOUNT                      
         DC    AL1(12)                             L'ACCT TO MATCH              
         DC    AL1(DR+CR)                          STATUS                       
         DC    AL1(30)                             DDS BATCH TYPE               
         DC    CL14'SR'                            C/A                          
         DC    C'28'                               MCKIM JE NUMBER              
         DC    C'6830'                             MCKIM GL ACCOUNT             
         DC    AL1(CNTR,2,6)                       CLIENT STATUS BYTES          
         DC    AL1(0)                              EXTRA STATUS                 
         DC    CL30'FOREIGN EXCH. ADJUSTMENT'      DESCRIPTION                  
*                                                                               
*                                                                               
         DC    CL14'SE6830'                        ACCOUNT                      
         DC    AL1(12)                             L'ACCT TO MATCH              
         DC    AL1(DR+CR)                          STATUS                       
         DC    AL1(45)                             DDS BATCH TYPE               
         DC    CL14'ST'                            C/A                          
         DC    C'28'                               MCKIM JE NUMBER              
         DC    C'6830'                             MCKIM GL ACCOUNT             
         DC    AL1(0,0,0)                          CLIENT STATUS BYTES          
         DC    AL1(0)                              EXTRA STATUS                 
         DC    CL30'FOREIGN ADJUSTMENT'            DESCRIPTION                  
*                                                                               
*                                                                               
         DC    CL14'SE6840'                        ACCOUNT                      
         DC    AL1(12)                             L'ACCT TO MATCH              
         DC    AL1(DR+CR)                          STATUS                       
         DC    AL1(30)                             DDS BATCH TYPE               
         DC    CL14'SR'                            C/A                          
         DC    C'28'                               MCKIM JE NUMBER              
         DC    C'6830'                             MCKIM GL ACCOUNT             
         DC    AL1(CNTR,2,6)                       CLIENT STATUS BYTES          
         DC    AL1(0)                              EXTRA STATUS                 
         DC    CL30'FOREIGN EXCH. ADJUSTMENT'      DESCRIPTION                  
*                                                                               
*                                                                               
         DC    CL14'SE6880'                        ACCOUNT                      
         DC    AL1(12)                             L'ACCT TO MATCH              
         DC    AL1(CR)                             STATUS                       
         DC    AL1(26)                             DDS BATCH TYPE               
         DC    CL14' '                             C/A                          
         DC    C'35'                               MCKIM JE NUMBER              
         DC    C'6880'                             MCKIM GL ACCOUNT             
         DC    AL1(SJ24,0,0)                       CLIENT STATUS BYTES          
         DC    AL1(0)                              EXTRA STATUS                 
         DC    CL30'BAD DEBT W/O'                  DESCRIPTION                  
*                                                                               
*                                                                               
         DC    CL14'SE6789'                        ACCOUNT                      
         DC    AL1(12)                             L'ACCT TO MATCH              
         DC    AL1(DR+CR)                          STATUS                       
         DC    AL1(30)                             DDS BATCH TYPE               
         DC    CL14'SR'                            C/A                          
         DC    C'53'                               MCKIM JE NUMBER              
         DC    C'6789'                             MCKIM GL ACCOUNT             
         DC    AL1(CNTR,2,6)                       CLIENT STATUS BYTES          
         DC    AL1(0)                              EXTRA STATUS                 
         DC    CL30'RCVBLE TRANSFER-FROM ACCT.'    DESCRIPTION                  
*                                                                               
*                                                                               
         DC    CL14'SE6789'                        ACCOUNT                      
         DC    AL1(12)                             L'ACCT TO MATCH              
         DC    AL1(CR)                             STATUS                       
         DC    AL1(26)                             DDS BATCH TYPE               
         DC    CL14'SJ'                            C/A                          
         DC    C'53'                               MCKIM JE NUMBER              
         DC    C'6789'                             MCKIM GL ACCOUNT             
         DC    AL1(0,0,0)                          CLIENT STATUS BYTES          
         DC    AL1(0)                              EXTRA STATUS                 
         DC    CL30'RCVBLE TRANSFER-TO ACCT.'      DESCRIPTION                  
*                                                                               
*                                                                               
         DC    CL14'SE6789'                        ACCOUNT                      
         DC    AL1(12)                             L'ACCT TO MATCH              
         DC    AL1(CR)                             STATUS                       
         DC    AL1(45)                             DDS BATCH TYPE               
         DC    CL14'SR'                            C/A                          
         DC    C'53'                               MCKIM JE NUMBER              
         DC    C'6789'                             MCKIM GL ACCOUNT             
         DC    AL1(CNTR,2,6)                       CLIENT STATUS BYTES          
         DC    AL1(0)                              EXTRA STATUS                 
         DC    CL30'FINANSYS RCVBLE B/F'           DESCRIPTION                  
*                                                                               
*                                                                               
         DC    CL14'SE6880'                        ACCOUNT                      
         DC    AL1(12)                             L'ACCT TO MATCH              
         DC    AL1(DR+CR)                          STATUS                       
         DC    AL1(30)                             DDS BATCH TYPE               
         DC    CL14'SR'                            C/A                          
         DC    C'35'                               MCKIM JE NUMBER              
         DC    C'6880'                             MCKIM GL ACCOUNT             
         DC    AL1(CNTR,2,6)                       CLIENT STATUS BYTES          
         DC    AL1(0)                              EXTRA STATUS                 
         DC    CL30'BAD DEBTS'                     DESCRIPTION                  
*                                                                               
*                                                                               
         DC    CL14'SE6830'                        ACCOUNT                      
         DC    AL1(12)                             L'ACCT TO MATCH              
         DC    AL1(DR+CR)                          STATUS                       
         DC    AL1(45)                             DDS BATCH TYPE               
         DC    CL14'SS'                            C/A                          
         DC    C'28'                               MCKIM JE NUMBER              
         DC    C'6830'                             MCKIM GL ACCOUNT             
         DC    AL1(0,0,0)                          CLIENT STATUS BYTES          
         DC    AL1(0)                              EXTRA STATUS                 
         DC    CL30'FOREIGN ADJUSTMENT'            DESCRIPTION                  
*                                                                               
*                                                                               
         DC    CL14'SE6830'                        ACCOUNT                      
         DC    AL1(12)                             L'ACCT TO MATCH              
         DC    AL1(DR+CR)                          STATUS                       
         DC    AL1(45)                             DDS BATCH TYPE               
         DC    CL14'SP'                            C/A                          
         DC    C'28'                               MCKIM JE NUMBER              
         DC    C'6830'                             MCKIM GL ACCOUNT             
         DC    AL1(0,0,0)                          CLIENT STATUS BYTES          
         DC    AL1(0)                              EXTRA STATUS                 
         DC    CL30'FOREIGN ADJUSTMENT'            DESCRIPTION                  
*                                                                               
*                                                                               
         DC    CL14'SE6830'                        ACCOUNT                      
         DC    AL1(12)                             L'ACCT TO MATCH              
         DC    AL1(DR+CR)                          STATUS                       
         DC    AL1(45)                             DDS BATCH TYPE               
         DC    CL14'SQ'                            C/A                          
         DC    C'28'                               MCKIM JE NUMBER              
         DC    C'6830'                             MCKIM GL ACCOUNT             
         DC    AL1(0,0,0)                          CLIENT STATUS BYTES          
         DC    AL1(0)                              EXTRA STATUS                 
         DC    CL30'FOREIGN ADJUSTMENT'            DESCRIPTION                  
*                                                                               
*                                                                               
         DC    CL14'SE6850'                        ACCOUNT                      
         DC    AL1(12)                             L'ACCT TO MATCH              
         DC    AL1(CR)                             STATUS                       
         DC    AL1(45)                             DDS BATCH TYPE               
         DC    CL14'SR'                            C/A                          
         DC    C'29'                               MCKIM JE NUMBER              
         DC    C'6850'                             MCKIM GL ACCOUNT             
         DC    AL1(CNTR,2,6)                       CLIENT STATUS BYTES          
         DC    AL1(0)                              EXTRA STATUS                 
         DC    CL30'CD EARNED/LOST'                DESCRIPTION                  
*                                                                               
*                                                                               
         DC    CL14'SE6855'                        ACCOUNT                      
         DC    AL1(12)                             L'ACCT TO MATCH              
         DC    AL1(CR)                             STATUS                       
         DC    AL1(45)                             DDS BATCH TYPE               
         DC    CL14'SR'                            C/A                          
         DC    C'29'                               MCKIM JE NUMBER              
         DC    C'6855'                             MCKIM GL ACCOUNT             
         DC    AL1(CNTR,2,6)                       CLIENT STATUS BYTES          
         DC    AL1(0)                              EXTRA STATUS                 
         DC    CL30'CD ALLOWED'                    DESCRIPTION                  
*                                                                               
*                                                                               
         DC    CL14'SE6853'                        ACCOUNT                      
         DC    AL1(12)                             L'ACCT TO MATCH              
         DC    AL1(CR)                             STATUS                       
         DC    AL1(45)                             DDS BATCH TYPE               
         DC    CL14'SR'                            C/A                          
         DC    C'29'                               MCKIM JE NUMBER              
         DC    C'6850'                             MCKIM GL ACCOUNT             
         DC    AL1(CNTR,2,6)                       CLIENT STATUS BYTES          
         DC    AL1(0)                              EXTRA STATUS                 
         DC    CL30'CD LOST'                       DESCRIPTION                  
*                                                                               
*                                                                               
         DC    CL14'SE6812'                        ACCOUNT                      
         DC    AL1(12)                             L'ACCT TO MATCH              
         DC    AL1(CR)                             STATUS                       
         DC    AL1(45)                             DDS BATCH TYPE               
         DC    CL14'SR'                            C/A                          
         DC    C'6 '                               MCKIM JE NUMBER              
         DC    C'1245'                             MCKIM GL ACCOUNT             
         DC    AL1(CNTR,2,6)                       CLIENT STATUS BYTES          
         DC    AL1(0)                              EXTRA STATUS                 
         DC    CL30'ACCTS RCVBLE W/O ADJ.'         DESCRIPTION                  
         EJECT ,                                                                
******************************************************************              
*              U/L=SF                                            *              
******************************************************************              
*                                                                               
         DC    CL14'SF'                            ACCOUNT                      
         DC    AL1(0)                              L'ACCT TO MATCH              
         DC    AL1(DR+CR)                          STATUS                       
         DC    AL1(45)                             DDS BATCH TYPE               
         DC    CL14' '                             C/A                          
         DC    C'6 '                               MCKIM JE NUMBER              
         DC    C'    '                             MCKIM GL ACCOUNT             
         DC    AL1(0,0,0)                          CLIENT STATUS BYTES          
         DC    AL1(0)                              EXTRA STATUS                 
         DC    CL30'ACCTS RCVBLE WRITE-OFF ADJ.'   DESCRIPTION                  
*                                                                               
*                                                                               
         DC    CL14'SF'                            ACCOUNT                      
         DC    AL1(0)                              L'ACCT TO MATCH              
         DC    AL1(DR+CR)                          STATUS                       
         DC    AL1(0)                              DDS BATCH TYPE               
         DC    CL14' '                             C/A                          
         DC    C'6 '                               MCKIM JE NUMBER              
         DC    C'1299'                             MCKIM GL ACCOUNT             
         DC    AL1(0,0,0)                          CLIENT STATUS BYTES          
         DC    AL1(0)                              EXTRA STATUS                 
         DC    CL30'ACCTS RCVBLE WRITE-OFF ADJ.'   DESCRIPTION                  
         EJECT ,                                                                
******************************************************************              
*              U/L=SG                                            *              
******************************************************************              
*                                                                               
         DC    CL14'SG'                            ACCOUNT                      
         DC    AL1(0)                              L'ACCT TO MATCH              
         DC    AL1(DR)                             STATUS                       
         DC    AL1(1)                              DDS BATCH TYPE               
         DC    CL14' '                             C/A                          
         DC    C'45'                               MCKIM JE NUMBER              
         DC    C'    '                             MCKIM GL ACCOUNT             
         DC    AL1(0,0,0)                          CLIENT STATUS BYTES          
         DC    AL1(0)                              EXTRA STATUS                 
         DC    CL30'PRODUCTION ALLOCATION'         DESCRIPTION                  
*                                                                               
*                                                                               
         DC    CL14'SG'                            ACCOUNT                      
         DC    AL1(0)                              L'ACCT TO MATCH              
         DC    AL1(DR)                             STATUS                       
         DC    AL1(3)                              DDS BATCH TYPE               
         DC    CL14' '                             C/A                          
         DC    C'46'                               MCKIM JE NUMBER              
         DC    C'    '                             MCKIM GL ACCOUNT             
         DC    AL1(0,0,0)                          CLIENT STATUS BYTES          
         DC    AL1(0)                              EXTRA STATUS                 
         DC    CL30'DOCKET CLEARING'               DESCRIPTION                  
*                                                                               
*                                                                               
         DC    CL14'SG'                            ACCOUNT                      
         DC    AL1(0)                              L'ACCT TO MATCH              
         DC    AL1(DR)                             STATUS                       
         DC    AL1(9)                              DDS BATCH TYPE               
         DC    CL14' '                             C/A                          
         DC    C'24'                               MCKIM JE NUMBER              
         DC    C'    '                             MCKIM GL ACCOUNT             
         DC    AL1(0,0,0)                          CLIENT STATUS BYTES          
         DC    AL1(0)                              EXTRA STATUS                 
         DC    CL30'FINAL BILLING (ACCTS RCVBLE)'  DESCRIPTION                  
*                                                                               
*                                                                               
         DC    CL14'SG'                            ACCOUNT                      
         DC    AL1(0)                              L'ACCT TO MATCH              
         DC    AL1(CR)                             STATUS                       
         DC    AL1(26)                             DDS BATCH TYPE               
         DC    CL14'SJHOECOR'                      C/A                          
         DC    C'6 '                               MCKIM JE NUMBER              
         DC    C'    '                             MCKIM GL ACCOUNT             
         DC    AL1(0,0,0)                          CLIENT STATUS BYTES          
         DC    AL1(0)                              EXTRA STATUS                 
         DC    CL30'ACCTS RCVBLE W/O ADJ.'         DESCRIPTION                  
*                                                                               
*                                                                               
         DC    CL14'SG'                            ACCOUNT                      
         DC    AL1(0)                              L'ACCT TO MATCH              
         DC    AL1(DR)                             STATUS                       
         DC    AL1(33)                             DDS BATCH TYPE               
         DC    CL14'SV'                            C/A                          
         DC    C'45'                               MCKIM JE NUMBER              
         DC    C'    '                             MCKIM GL ACCOUNT             
         DC    AL1(0,0,0)                          CLIENT STATUS BYTES          
         DC    AL1(0)                              EXTRA STATUS                 
         DC    CL30'PRODUCTION ALLOCATION'         DESCRIPTION                  
*                                                                               
*                                                                               
         DC    CL14'SG'                            ACCOUNT                      
         DC    AL1(0)                              L'ACCT TO MATCH              
         DC    AL1(CR)                             STATUS                       
         DC    AL1(26)                             DDS BATCH TYPE               
         DC    CL14' '                             C/A                          
         DC    C'35'                               MCKIM JE NUMBER              
         DC    C'    '                             MCKIM GL ACCOUNT             
         DC    AL1(0,0,0)                          CLIENT STATUS BYTES          
         DC    AL1(0)                              EXTRA STATUS                 
         DC    CL30'BAD DEBT W/O'                  DESCRIPTION                  
*                                                                               
*                                                                               
         DC    CL14'SG'                            ACCOUNT                      
         DC    AL1(0)                              L'ACCT TO MATCH              
         DC    AL1(DR)                             STATUS                       
         DC    AL1(49)                             DDS BATCH TYPE               
         DC    CL14' '                             C/A                          
         DC    C'27'                               MCKIM JE NUMBER              
         DC    C'    '                             MCKIM GL ACCOUNT             
         DC    AL1(0,0,0)                          CLIENT STATUS BYTES          
         DC    AL1(0)                              EXTRA STATUS                 
         DC    CL30'GST INPUT TAX CREDIT - RCVBLE' DESCRIPTION                  
*                                                                               
*                                                                               
         DC    CL14'SG'                            ACCOUNT                      
         DC    AL1(0)                              L'ACCT TO MATCH              
         DC    AL1(DR)                             STATUS                       
         DC    AL1(50)                             DDS BATCH TYPE               
         DC    CL14' '                             C/A                          
         DC    C'27'                               MCKIM JE NUMBER              
         DC    C'    '                             MCKIM GL ACCOUNT             
         DC    AL1(0,0,0)                          CLIENT STATUS BYTES          
         DC    AL1(0)                              EXTRA STATUS                 
         DC    CL30'GST INPUT TAX CREDIT - RCVBLE' DESCRIPTION                  
*                                                                               
*                                                                               
         DC    CL14'SG'                            ACCOUNT                      
         DC    AL1(0)                              L'ACCT TO MATCH              
         DC    AL1(DR)                             STATUS                       
         DC    AL1(0)                              DDS BATCH TYPE               
         DC    CL14' '                             C/A                          
         DC    C'23'                               MCKIM JE NUMBER              
         DC    C'    '                             MCKIM GL ACCOUNT             
         DC    AL1(0,0,0)                          CLIENT STATUS BYTES          
         DC    AL1(0)                              EXTRA STATUS                 
         DC    CL30'GST INPUT TAX CREDIT - RCVBLE' DESCRIPTION                  
*                                                                               
*                                                                               
         DC    CL14'SG'                            ACCOUNT                      
         DC    AL1(0)                              L'ACCT TO MATCH              
         DC    AL1(CR)                             STATUS                       
         DC    AL1(0)                              DDS BATCH TYPE               
         DC    CL14' '                             C/A                          
         DC    C'24'                               MCKIM JE NUMBER              
         DC    C'    '                             MCKIM GL ACCOUNT             
         DC    AL1(0,0,0)                          CLIENT STATUS BYTES          
         DC    AL1(0)                              EXTRA STATUS                 
         DC    CL30'FINAL BILLING (ACCTS RCVBLE)'  DESCRIPTION                  
         EJECT ,                                                                
******************************************************************              
*              U/L=SI                                            *              
******************************************************************              
*                                                                               
         DC    CL14'SI'                            ACCOUNT                      
         DC    AL1(0)                              L'ACCT TO MATCH              
         DC    AL1(CR)                             STATUS                       
         DC    AL1(1)                              DDS BATCH TYPE               
         DC    CL14'SJ'                            C/A                          
         DC    C'45'                               MCKIM JE NUMBER              
         DC    C'    '                             MCKIM GL ACCOUNT             
         DC    AL1(0,0,0)                          CLIENT STATUS BYTES          
         DC    AL1(0)                              EXTRA STATUS                 
         DC    CL30'PRODUCTION ALLOCATION'         DESCRIPTION                  
*                                                                               
*                                                                               
         DC    CL14'SI'                            ACCOUNT                      
         DC    AL1(0)                              L'ACCT TO MATCH              
         DC    AL1(DR+CR)                          STATUS                       
         DC    AL1(5)                              DDS BATCH TYPE               
         DC    CL14' '                             C/A                          
         DC    C'22'                               MCKIM JE NUMBER              
         DC    C'    '                             MCKIM GL ACCOUNT             
         DC    AL1(0,0,0)                          CLIENT STATUS BYTES          
         DC    AL1(0)                              EXTRA STATUS                 
         DC    CL30'CHEQUE PAYMENTS'               DESCRIPTION                  
*                                                                               
*                                                                               
         DC    CL14'SI'                            ACCOUNT                      
         DC    AL1(0)                              L'ACCT TO MATCH              
         DC    AL1(CR)                             STATUS                       
         DC    AL1(0)                              DDS BATCH TYPE               
         DC    CL14' '                             C/A                          
         DC    C'24'                               MCKIM JE NUMBER              
         DC    C'    '                             MCKIM GL ACCOUNT             
         DC    AL1(CNTR,0,6)                       CLIENT STATUS BYTES          
         DC    AL1(0)                              EXTRA STATUS                 
         DC    CL30'FINAL BILLING (ACCTS RCVBLE)'  DESCRIPTION                  
*                                                                               
*                                                                               
         DC    CL14'SI'                            ACCOUNT                      
         DC    AL1(0)                              L'ACCT TO MATCH              
         DC    AL1(CR)                             STATUS                       
         DC    AL1(33)                             DDS BATCH TYPE               
         DC    CL14'SJ'                            C/A                          
         DC    C'45'                               MCKIM JE NUMBER              
         DC    C'    '                             MCKIM GL ACCOUNT             
         DC    AL1(0,0,0)                          CLIENT STATUS BYTES          
         DC    AL1(0)                              EXTRA STATUS                 
         DC    CL30'PRODUCTION ALLOCATION'         DESCRIPTION                  
         EJECT ,                                                                
******************************************************************              
*              U/L=SJ                                            *              
******************************************************************              
*                                                                               
         DC    CL14'SJ'                            ACCOUNT                      
         DC    AL1(0)                              L'ACCT TO MATCH              
         DC    AL1(DR)                             STATUS                       
         DC    AL1(1)                              DDS BATCH TYPE               
         DC    CL14'SV'                            C/A                          
         DC    C'45'                               MCKIM JE NUMBER              
         DC    C'1500'                             MCKIM GL ACCOUNT             
         DC    AL1(SJ24,0,0)                       CLIENT STATUS BYTES          
         DC    AL1(0)                              EXTRA STATUS                 
         DC    CL30'PRODUCTION ALLOCATION'         DESCRIPTION                  
*                                                                               
*                                                                               
         DC    CL14'SJ'                            ACCOUNT                      
         DC    AL1(0)                              L'ACCT TO MATCH              
         DC    AL1(DR)                             STATUS                       
         DC    AL1(1)                              DDS BATCH TYPE               
         DC    CL14'SW'                            C/A                          
         DC    C'45'                               MCKIM JE NUMBER              
         DC    C'1500'                             MCKIM GL ACCOUNT             
         DC    AL1(SJ24,0,0)                       CLIENT STATUS BYTES          
         DC    AL1(0)                              EXTRA STATUS                 
         DC    CL30'PRODUCTION ALLOCATION'         DESCRIPTION                  
*                                                                               
*                                                                               
         DC    CL14'SJ'                            ACCOUNT                      
         DC    AL1(0)                              L'ACCT TO MATCH              
         DC    AL1(DR)                             STATUS                       
         DC    AL1(3)                              DDS BATCH TYPE               
         DC    CL14'SVSMINFINE'                    C/A                          
         DC    C'47'                               MCKIM JE NUMBER              
         DC    C'1500'                             MCKIM GL ACCOUNT             
         DC    AL1(SJ24,0,0)                       CLIENT STATUS BYTES          
         DC    AL1(0)                              EXTRA STATUS                 
         DC    CL30'PST ALLOCATION'                DESCRIPTION                  
*                                                                               
*                                                                               
         DC    CL14'SJ'                            ACCOUNT                      
         DC    AL1(0)                              L'ACCT TO MATCH              
         DC    AL1(DR)                             STATUS                       
         DC    AL1(3)                              DDS BATCH TYPE               
         DC    CL14'SVSMINFINF'                    C/A                          
         DC    C'47'                               MCKIM JE NUMBER              
         DC    C'1500'                             MCKIM GL ACCOUNT             
         DC    AL1(SJ24,0,0)                       CLIENT STATUS BYTES          
         DC    AL1(0)                              EXTRA STATUS                 
         DC    CL30'PST ALLOCATION'                DESCRIPTION                  
*                                                                               
*                                                                               
         DC    CL14'SJ'                            ACCOUNT                      
         DC    AL1(0)                              L'ACCT TO MATCH              
         DC    AL1(DR)                             STATUS                       
         DC    AL1(3)                              DDS BATCH TYPE               
         DC    CL14'SVTMINFINV'                    C/A                          
         DC    C'47'                               MCKIM JE NUMBER              
         DC    C'1500'                             MCKIM GL ACCOUNT             
         DC    AL1(SJ24,0,0)                       CLIENT STATUS BYTES          
         DC    AL1(0)                              EXTRA STATUS                 
         DC    CL30'PST ALLOCATION'                DESCRIPTION                  
*                                                                               
*                                                                               
         DC    CL14'SJ'                            ACCOUNT                      
         DC    AL1(0)                              L'ACCT TO MATCH              
         DC    AL1(DR)                             STATUS                       
         DC    AL1(3)                              DDS BATCH TYPE               
         DC    CL14'SVTMINFINT'                    C/A                          
         DC    C'47'                               MCKIM JE NUMBER              
         DC    C'1500'                             MCKIM GL ACCOUNT             
         DC    AL1(SJ24,0,0)                       CLIENT STATUS BYTES          
         DC    AL1(0)                              EXTRA STATUS                 
         DC    CL30'PST ALLOCATION'                DESCRIPTION                  
*                                                                               
*                                                                               
         DC    CL14'SJ'                            ACCOUNT                      
         DC    AL1(0)                              L'ACCT TO MATCH              
         DC    AL1(DR)                             STATUS                       
         DC    AL1(3)                              DDS BATCH TYPE               
         DC    CL14'SVTMINFINC'                    C/A                          
         DC    C'47'                               MCKIM JE NUMBER              
         DC    C'1500'                             MCKIM GL ACCOUNT             
         DC    AL1(SJ24,0,0)                       CLIENT STATUS BYTES          
         DC    AL1(0)                              EXTRA STATUS                 
         DC    CL30'PST ALLOCATION'                DESCRIPTION                  
*                                                                               
*                                                                               
         DC    CL14'SJ'                            ACCOUNT                      
         DC    AL1(0)                              L'ACCT TO MATCH              
         DC    AL1(DR)                             STATUS                       
         DC    AL1(3)                              DDS BATCH TYPE               
         DC    CL14'SVTMINFINW'                    C/A                          
         DC    C'47'                               MCKIM JE NUMBER              
         DC    C'1500'                             MCKIM GL ACCOUNT             
         DC    AL1(SJ24,0,0)                       CLIENT STATUS BYTES          
         DC    AL1(0)                              EXTRA STATUS                 
         DC    CL30'PST ALLOCATION'                DESCRIPTION                  
*                                                                               
*                                                                               
         DC    CL14'SJ'                            ACCOUNT                      
         DC    AL1(0)                              L'ACCT TO MATCH              
         DC    AL1(DR)                             STATUS                       
         DC    AL1(3)                              DDS BATCH TYPE               
         DC    CL14'SVUMINFINN'                    C/A                          
         DC    C'47'                               MCKIM JE NUMBER              
         DC    C'1500'                             MCKIM GL ACCOUNT             
         DC    AL1(SJ24,0,0)                       CLIENT STATUS BYTES          
         DC    AL1(0)                              EXTRA STATUS                 
         DC    CL30'PST ALLOCATION'                DESCRIPTION                  
*                                                                               
*                                                                               
         DC    CL14'SJ'                            ACCOUNT                      
         DC    AL1(0)                              L'ACCT TO MATCH              
         DC    AL1(DR)                             STATUS                       
         DC    AL1(3)                              DDS BATCH TYPE               
         DC    CL14'SVUMINFING'                    C/A                          
         DC    C'47'                               MCKIM JE NUMBER              
         DC    C'1500'                             MCKIM GL ACCOUNT             
         DC    AL1(SJ24,0,0)                       CLIENT STATUS BYTES          
         DC    AL1(0)                              EXTRA STATUS                 
         DC    CL30'PST ALLOCATION'                DESCRIPTION                  
*                                                                               
*                                                                               
         DC    CL14'SJ'                            ACCOUNT                      
         DC    AL1(0)                              L'ACCT TO MATCH              
         DC    AL1(DR)                             STATUS                       
         DC    AL1(3)                              DDS BATCH TYPE               
         DC    CL14'SV'                            C/A                          
         DC    C'46'                               MCKIM JE NUMBER              
         DC    C'1500'                             MCKIM GL ACCOUNT             
         DC    AL1(SJ24,0,0)                       CLIENT STATUS BYTES          
         DC    AL1(0)                              EXTRA STATUS                 
         DC    CL30'DOCKET CLEARING'               DESCRIPTION                  
*                                                                               
*                                                                               
         DC    CL14'SJ'                            ACCOUNT                      
         DC    AL1(0)                              L'ACCT TO MATCH              
         DC    AL1(DR)                             STATUS                       
         DC    AL1(3)                              DDS BATCH TYPE               
         DC    CL14'SW'                            C/A                          
         DC    C'46'                               MCKIM JE NUMBER              
         DC    C'1500'                             MCKIM GL ACCOUNT             
         DC    AL1(SJ24,0,0)                       CLIENT STATUS BYTES          
         DC    AL1(0)                              EXTRA STATUS                 
         DC    CL30'DOCKET CLEARING'               DESCRIPTION                  
*                                                                               
*                                                                               
         DC    CL14'SJ'                            ACCOUNT                      
         DC    AL1(0)                              L'ACCT TO MATCH              
         DC    AL1(CR)                             STATUS                       
         DC    AL1(6)                              DDS BATCH TYPE               
         DC    CL14'SR'                            C/A                          
         DC    C'24'                               MCKIM JE NUMBER              
         DC    C'1500'                             MCKIM GL ACCOUNT             
         DC    AL1(CNTR,2,6)                       CLIENT STATUS BYTES          
         DC    AL1(0)                              EXTRA STATUS                 
         DC    CL30'FINAL BILLING (ACCTS RCVBLE)'  DESCRIPTION                  
*                                                                               
*                                                                               
         DC    CL14'SJ'                            ACCOUNT                      
         DC    AL1(0)                              L'ACCT TO MATCH              
         DC    AL1(CR)                             STATUS                       
         DC    AL1(7)                              DDS BATCH TYPE               
         DC    CL14'SR'                            C/A                          
         DC    C'24'                               MCKIM JE NUMBER              
         DC    C'1500'                             MCKIM GL ACCOUNT             
         DC    AL1(CNTR,2,6)                       CLIENT STATUS BYTES          
         DC    AL1(0)                              EXTRA STATUS                 
         DC    CL30'FINAL BILLING (ACCTS RCVBLE)'  DESCRIPTION                  
*                                                                               
*                                                                               
         DC    CL14'SJ'                            ACCOUNT                      
         DC    AL1(0)                              L'ACCT TO MATCH              
         DC    AL1(DR)                             STATUS                       
         DC    AL1(8)                              DDS BATCH TYPE               
         DC    CL14'SK'                            C/A                          
         DC    C'44'                               MCKIM JE NUMBER              
         DC    C'1500'                             MCKIM GL ACCOUNT             
         DC    AL1(SJ24,0,0)                       CLIENT STATUS BYTES          
         DC    AL1(0)                              EXTRA STATUS                 
         DC    CL30'INTERNAL INCOME'               DESCRIPTION                  
*                                                                               
*                                                                               
         DC    CL14'SJ'                            ACCOUNT                      
         DC    AL1(0)                              L'ACCT TO MATCH              
         DC    AL1(DR)                             STATUS                       
         DC    AL1(14)                             DDS BATCH TYPE               
         DC    CL14'SE'                            C/A                          
         DC    C'49'                               MCKIM JE NUMBER              
         DC    C'1500'                             MCKIM GL ACCOUNT             
         DC    AL1(SJ24,0,0)                       CLIENT STATUS BYTES          
         DC    AL1(CANAME)                         EXTRA STATUS                 
         DC    CL30'               - DOCKET TO EXP'  DESCRIPTION                
*                                                                               
*                                                                               
         DC    CL14'SJ'                            ACCOUNT                      
         DC    AL1(0)                              L'ACCT TO MATCH              
         DC    AL1(DR)                             STATUS                       
         DC    AL1(15)                             DDS BATCH TYPE               
         DC    CL14'SE'                            C/A                          
         DC    C'48'                               MCKIM JE NUMBER              
         DC    C'1500'                             MCKIM GL ACCOUNT             
         DC    AL1(SJ24,0,0)                       CLIENT STATUS BYTES          
         DC    AL1(CANAME)                         EXTRA STATUS                 
         DC    CL30'               - EXP TO DOCKET'  DESCRIPTION                
*                                                                               
*                                                                               
         DC    CL14'SJ'                            ACCOUNT                      
         DC    AL1(0)                              L'ACCT TO MATCH              
         DC    AL1(DR)                             STATUS                       
         DC    AL1(33)                             DDS BATCH TYPE               
         DC    CL14'SV'                            C/A                          
         DC    C'45'                               MCKIM JE NUMBER              
         DC    C'1500'                             MCKIM GL ACCOUNT             
         DC    AL1(SJ24,0,0)                       CLIENT STATUS BYTES          
         DC    AL1(0)                              EXTRA STATUS                 
         DC    CL30'PRODUCTION ALLOCATION'         DESCRIPTION                  
*                                                                               
*                                                                               
         DC    CL14'SJ'                            ACCOUNT                      
         DC    AL1(0)                              L'ACCT TO MATCH              
         DC    AL1(DR)                             STATUS                       
         DC    AL1(34)                             DDS BATCH TYPE               
         DC    CL14' '                             C/A                          
         DC    C'45'                               MCKIM JE NUMBER              
         DC    C'1500'                             MCKIM GL ACCOUNT             
         DC    AL1(SJ24,0,0)                       CLIENT STATUS BYTES          
         DC    AL1(0)                              EXTRA STATUS                 
         DC    CL30'PRODUCTION ALLOCATION'         DESCRIPTION                  
         EJECT ,                                                                
******************************************************************              
*              U/L=SK                                            *              
******************************************************************              
*                                                                               
         DC    CL14'SK'                            ACCOUNT                      
         DC    AL1(0)                              L'ACCT TO MATCH              
         DC    AL1(DR)                             STATUS                       
         DC    AL1(6)                              DDS BATCH TYPE               
         DC    CL14'SJ'                            C/A                          
         DC    C'24'                               MCKIM JE NUMBER              
         DC    C'    '                             MCKIM GL ACCOUNT             
         DC    AL1(SJ24,0,0)                       CLIENT STATUS BYTES          
         DC    AL1(0)                              EXTRA STATUS                 
         DC    CL30'FINAL BILLING (ACCTS RCVBLE)'  DESCRIPTION                  
*                                                                               
*                                                                               
         DC    CL14'SK'                            ACCOUNT                      
         DC    AL1(0)                              L'ACCT TO MATCH              
         DC    AL1(DR)                             STATUS                       
         DC    AL1(7)                              DDS BATCH TYPE               
         DC    CL14'SJ'                            C/A                          
         DC    C'24'                               MCKIM JE NUMBER              
         DC    C'    '                             MCKIM GL ACCOUNT             
         DC    AL1(SJ24,0,0)                       CLIENT STATUS BYTES          
         DC    AL1(0)                              EXTRA STATUS                 
         DC    CL30'FINAL BILLING (ACCTS RCVBLE)'  DESCRIPTION                  
*                                                                               
*                                                                               
         DC    CL14'SK'                            ACCOUNT                      
         DC    AL1(0)                              L'ACCT TO MATCH              
         DC    AL1(CR)                             STATUS                       
         DC    AL1(8)                              DDS BATCH TYPE               
         DC    CL14'SJ'                            C/A                          
         DC    C'44'                               MCKIM JE NUMBER              
         DC    C'    '                             MCKIM GL ACCOUNT             
         DC    AL1(SJ24,0,0)                       CLIENT STATUS BYTES          
         DC    AL1(0)                              EXTRA STATUS                 
         DC    CL30'INTERNAL INCOME'               DESCRIPTION                  
*                                                                               
*                                                                               
         DC    CL14'SK'                            ACCOUNT                      
         DC    AL1(0)                              L'ACCT TO MATCH              
         DC    AL1(CR)                             STATUS                       
         DC    AL1(34)                             DDS BATCH TYPE               
         DC    CL14'SJ'                            C/A                          
         DC    C'44'                               MCKIM JE NUMBER              
         DC    C'    '                             MCKIM GL ACCOUNT             
         DC    AL1(SJ24,0,0)                       CLIENT STATUS BYTES          
         DC    AL1(0)                              EXTRA STATUS                 
         DC    CL30'INTERNAL INCOME'               DESCRIPTION                  
         EJECT ,                                                                
******************************************************************              
*              U/L=SP                                            *              
******************************************************************              
*                                                                               
         DC    CL14'SP'                            ACCOUNT                      
         DC    AL1(0)                              L'ACCT TO MATCH              
         DC    AL1(DR+CR)                          STATUS                       
         DC    AL1(37)                             DDS BATCH TYPE               
         DC    CL14' '                             C/A                          
         DC    C'30'                               MCKIM JE NUMBER              
         DC    C'2013'                             MCKIM GL ACCOUNT             
         DC    AL1(0,0,0)                          CLIENT STATUS BYTES          
         DC    AL1(0)                              EXTRA STATUS                 
         DC    CL30'CANCELLED CHEQUES'             DESCRIPTION                  
*                                                                               
*                                                                               
         DC    CL14'SP'                            ACCOUNT                      
         DC    AL1(0)                              L'ACCT TO MATCH              
         DC    AL1(CR)                             STATUS                       
         DC    AL1(45)                             DDS BATCH TYPE               
         DC    CL14'SE6830'                        C/A                          
         DC    C'28'                               MCKIM JE NUMBER              
         DC    C'2013'                             MCKIM GL ACCOUNT             
         DC    AL1(0,0,0)                          CLIENT STATUS BYTES          
         DC    AL1(0)                              EXTRA STATUS                 
         DC    CL30'FOREIGN ADJUSTMENT'            DESCRIPTION                  
*                                                                               
*                                                                               
         DC    CL14'SP'                            ACCOUNT                      
         DC    AL1(0)                              L'ACCT TO MATCH              
         DC    AL1(CR)                             STATUS                       
         DC    AL1(0)                              DDS BATCH TYPE               
         DC    CL14' '                             C/A                          
         DC    C'27'                               MCKIM JE NUMBER              
         DC    C'2013'                             MCKIM GL ACCOUNT             
         DC    AL1(0,0,0)                          CLIENT STATUS BYTES          
         DC    AL1(0)                              EXTRA STATUS                 
         DC    CL30'CLEARANCES'                    DESCRIPTION                  
*                                                                               
*                                                                               
         DC    CL14'SP'                            ACCOUNT                      
         DC    AL1(0)                              L'ACCT TO MATCH              
         DC    AL1(DR)                             STATUS                       
         DC    AL1(0)                              DDS BATCH TYPE               
         DC    CL14' '                             C/A                          
         DC    C'22'                               MCKIM JE NUMBER              
         DC    C'2013'                             MCKIM GL ACCOUNT             
         DC    AL1(0,0,0)                          CLIENT STATUS BYTES          
         DC    AL1(0)                              EXTRA STATUS                 
         DC    CL30'CHEQUE PAYMENTS'               DESCRIPTION                  
*                                                                               
*                                                                               
         DC    CL14'SP'                            ACCOUNT                      
         DC    AL1(0)                              L'ACCT TO MATCH              
         DC    AL1(CR)                             STATUS                       
         DC    AL1(45)                             DDS BATCH TYPE               
         DC    CL14'SE2026'                        C/A                          
         DC    C'75'                               MCKIM JE NUMBER              
         DC    C'2013'                             MCKIM GL ACCOUNT             
         DC    AL1(0,0,0)                          CLIENT STATUS BYTES          
         DC    AL1(0)                              EXTRA STATUS                 
         DC    CL30'PAYABLES OFFSETS'              DESCRIPTION                  
         EJECT ,                                                                
******************************************************************              
*              U/L=SQ                                            *              
******************************************************************              
*                                                                               
         DC    CL14'SQ'                            ACCOUNT                      
         DC    AL1(0)                              L'ACCT TO MATCH              
         DC    AL1(DR)                             STATUS                       
         DC    AL1(36)                             DDS BATCH TYPE               
         DC    CL14' '                             C/A                          
         DC    C'31'                               MCKIM JE NUMBER              
         DC    C'2012'                             MCKIM GL ACCOUNT             
         DC    AL1(0,0,0)                          CLIENT STATUS BYTES          
         DC    AL1(0)                              EXTRA STATUS                 
         DC    CL30'MANUAL CHEQUE'                 DESCRIPTION                  
*                                                                               
*                                                                               
         DC    CL14'SQ'                            ACCOUNT                      
         DC    AL1(0)                              L'ACCT TO MATCH              
         DC    AL1(DR+CR)                          STATUS                       
         DC    AL1(37)                             DDS BATCH TYPE               
         DC    CL14' '                             C/A                          
         DC    C'30'                               MCKIM JE NUMBER              
         DC    C'2012'                             MCKIM GL ACCOUNT             
         DC    AL1(0,0,0)                          CLIENT STATUS BYTES          
         DC    AL1(0)                              EXTRA STATUS                 
         DC    CL30'CANCELLED CHEQUES'             DESCRIPTION                  
*                                                                               
*                                                                               
         DC    CL14'SQ'                            ACCOUNT                      
         DC    AL1(0)                              L'ACCT TO MATCH              
         DC    AL1(CR)                             STATUS                       
         DC    AL1(45)                             DDS BATCH TYPE               
         DC    CL14'SE6830'                        C/A                          
         DC    C'28'                               MCKIM JE NUMBER              
         DC    C'2013'                             MCKIM GL ACCOUNT             
         DC    AL1(0,0,0)                          CLIENT STATUS BYTES          
         DC    AL1(0)                              EXTRA STATUS                 
         DC    CL30'FOREIGN ADJUSTMENT'            DESCRIPTION                  
*                                                                               
*                                                                               
         DC    CL14'SQ'                            ACCOUNT                      
         DC    AL1(0)                              L'ACCT TO MATCH              
         DC    AL1(CR)                             STATUS                       
         DC    AL1(0)                              DDS BATCH TYPE               
         DC    CL14' '                             C/A                          
         DC    C'27'                               MCKIM JE NUMBER              
         DC    C'2012'                             MCKIM GL ACCOUNT             
         DC    AL1(0,0,0)                          CLIENT STATUS BYTES          
         DC    AL1(0)                              EXTRA STATUS                 
         DC    CL30'CLEARANCES'                    DESCRIPTION                  
*                                                                               
*                                                                               
         DC    CL14'SQ'                            ACCOUNT                      
         DC    AL1(0)                              L'ACCT TO MATCH              
         DC    AL1(DR)                             STATUS                       
         DC    AL1(0)                              DDS BATCH TYPE               
         DC    CL14' '                             C/A                          
         DC    C'22'                               MCKIM JE NUMBER              
         DC    C'2012'                             MCKIM GL ACCOUNT             
         DC    AL1(0,0,0)                          CLIENT STATUS BYTES          
         DC    AL1(0)                              EXTRA STATUS                 
         DC    CL30'CHEQUE PAYMENTS'               DESCRIPTION                  
*                                                                               
*                                                                               
         DC    CL14'SQ'                            ACCOUNT                      
         DC    AL1(0)                              L'ACCT TO MATCH              
         DC    AL1(CR)                             STATUS                       
         DC    AL1(45)                             DDS BATCH TYPE               
         DC    CL14'SE2025'                        C/A                          
         DC    C'75'                               MCKIM JE NUMBER              
         DC    C'2012'                             MCKIM GL ACCOUNT             
         DC    AL1(0,0,0)                          CLIENT STATUS BYTES          
         DC    AL1(0)                              EXTRA STATUS                 
         DC    CL30'PAYABLES OFFSETS'              DESCRIPTION                  
*                                                                               
*                                                                               
         DC    CL14'SQ'                            ACCOUNT                      
         DC    AL1(0)                              L'ACCT TO MATCH              
         DC    AL1(CR)                             STATUS                       
         DC    AL1(45)                             DDS BATCH TYPE               
         DC    CL14'SVP'                           C/A                          
         DC    C'33'                               MCKIM JE NUMBER              
         DC    C'2012'                             MCKIM GL ACCOUNT             
         DC    AL1(0,0,0)                          CLIENT STATUS BYTES          
         DC    AL1(0)                              EXTRA STATUS                 
         DC    CL30'A/P TRANSFER'                  DESCRIPTION                  
         EJECT ,                                                                
******************************************************************              
*              U/L=SR                                            *              
******************************************************************              
*                                                                               
         DC    CL14'SR'                            ACCOUNT                      
         DC    AL1(0)                              L'ACCT TO MATCH              
         DC    AL1(DR)                             STATUS                       
         DC    AL1(20)                             DDS BATCH TYPE               
         DC    CL14'  PRIOR O/S'                   C/A                          
         DC    C'53'                               MCKIM JE NUMBER              
         DC    C'1202'                             MCKIM GL ACCOUNT             
         DC    AL1(ACCT,2,6)                       CLIENT STATUS BYTES          
         DC    AL1(0)                              EXTRA STATUS                 
         DC    CL30'FOCUS REC''B BAL. FWD.'        DESCRIPTION                  
*                                                                               
*                                                                               
         DC    CL14'SR'                            ACCOUNT                      
         DC    AL1(0)                              L'ACCT TO MATCH              
         DC    AL1(DR)                             STATUS                       
         DC    AL1(26)                             DDS BATCH TYPE               
         DC    CL14'  PREBILL'                     C/A                          
         DC    C'6 '                               MCKIM JE NUMBER              
         DC    C'1202'                             MCKIM GL ACCOUNT             
         DC    AL1(ACCT,2,6)                       CLIENT STATUS BYTES          
         DC    AL1(0)                              EXTRA STATUS                 
         DC    CL30'ACCTS RCVBLE W/O ADJ.'         DESCRIPTION                  
*                                                                               
*                                                                               
         DC    CL14'SR'                            ACCOUNT                      
         DC    AL1(0)                              L'ACCT TO MATCH              
         DC    AL1(DR)                             STATUS                       
         DC    AL1(26)                             DDS BATCH TYPE               
         DC    CL14'  BAD DEBT W/O'                C/A                          
         DC    C'35'                               MCKIM JE NUMBER              
         DC    C'1202'                             MCKIM GL ACCOUNT             
         DC    AL1(ACCT,2,6)                       CLIENT STATUS BYTES          
         DC    AL1(0)                              EXTRA STATUS                 
         DC    CL30'BAD DEBT W/O'                  DESCRIPTION                  
*                                                                               
*                                                                               
         DC    CL14'SR'                            ACCOUNT                      
         DC    AL1(0)                              L'ACCT TO MATCH              
         DC    AL1(CR)                             STATUS                       
         DC    AL1(30)                             DDS BATCH TYPE               
         DC    CL14'SE6789'                        C/A                          
         DC    C'53'                               MCKIM JE NUMBER              
         DC    C'1202'                             MCKIM GL ACCOUNT             
         DC    AL1(ACCT,2,6)                       CLIENT STATUS BYTES          
         DC    AL1(WRITEOFF)                       EXTRA STATUS                 
         DC    CL30'RCVBLE TRANSFER-FROM ACCT.'    DESCRIPTION                  
*                                                                               
*                                                                               
         DC    CL14'SR'                            ACCOUNT                      
         DC    AL1(0)                              L'ACCT TO MATCH              
         DC    AL1(CR)                             STATUS                       
         DC    AL1(30)                             DDS BATCH TYPE               
         DC    CL14'SE1220'                        C/A                          
         DC    C'53'                               MCKIM JE NUMBER              
         DC    C'1202'                             MCKIM GL ACCOUNT             
         DC    AL1(ACCT,2,6)                       CLIENT STATUS BYTES          
         DC    AL1(WRITEOFF)                       EXTRA STATUS                 
         DC    CL30'AR ADJUSTMENTS'                DESCRIPTION                  
*                                                                               
*                                                                               
         DC    CL14'SR'                            ACCOUNT                      
         DC    AL1(0)                              L'ACCT TO MATCH              
         DC    AL1(CR)                             STATUS                       
         DC    AL1(30)                             DDS BATCH TYPE               
         DC    CL14'SE6818'                        C/A                          
         DC    C'26'                               MCKIM JE NUMBER              
         DC    C'1202'                             MCKIM GL ACCOUNT             
         DC    AL1(ACCT,2,6)                       CLIENT STATUS BYTES          
         DC    AL1(WRITEOFF)                       EXTRA STATUS                 
         DC    CL30'AR SMALL BALANCE'              DESCRIPTION                  
*                                                                               
*                                                                               
         DC    CL14'SR'                            ACCOUNT                      
         DC    AL1(0)                              L'ACCT TO MATCH              
         DC    AL1(CR)                             STATUS                       
         DC    AL1(30)                             DDS BATCH TYPE               
         DC    CL14'SE6812'                        C/A                          
         DC    C'26'                               MCKIM JE NUMBER              
         DC    C'1202'                             MCKIM GL ACCOUNT             
         DC    AL1(ACCT,2,6)                       CLIENT STATUS BYTES          
         DC    AL1(WRITEOFF)                       EXTRA STATUS                 
         DC    CL30'WRITE-OFF I/CO BILLING - WCD'  DESCRIPTION                  
*                                                                               
*                                                                               
         DC    CL14'SR'                            ACCOUNT                      
         DC    AL1(0)                              L'ACCT TO MATCH              
         DC    AL1(CR)                             STATUS                       
         DC    AL1(30)                             DDS BATCH TYPE               
         DC    CL14'SE6830'                        C/A                          
         DC    C'28'                               MCKIM JE NUMBER              
         DC    C'1202'                             MCKIM GL ACCOUNT             
         DC    AL1(ACCT,2,6)                       CLIENT STATUS BYTES          
         DC    AL1(WRITEOFF)                       EXTRA STATUS                 
         DC    CL30'FOREIGN EXCH.'                 DESCRIPTION                  
*                                                                               
*                                                                               
         DC    CL14'SR'                            ACCOUNT                      
         DC    AL1(0)                              L'ACCT TO MATCH              
         DC    AL1(CR)                             STATUS                       
         DC    AL1(30)                             DDS BATCH TYPE               
         DC    CL14'SE6840'                        C/A                          
         DC    C'28'                               MCKIM JE NUMBER              
         DC    C'1202'                             MCKIM GL ACCOUNT             
         DC    AL1(ACCT,2,6)                       CLIENT STATUS BYTES          
         DC    AL1(WRITEOFF)                       EXTRA STATUS                 
         DC    CL30'FOREIGN EXCH.'                 DESCRIPTION                  
*                                                                               
*                                                                               
         DC    CL14'SR'                            ACCOUNT                      
         DC    AL1(0)                              L'ACCT TO MATCH              
         DC    AL1(CR)                             STATUS                       
         DC    AL1(30)                             DDS BATCH TYPE               
         DC    CL14'SE6880'                        C/A                          
         DC    C'35'                               MCKIM JE NUMBER              
         DC    C'1202'                             MCKIM GL ACCOUNT             
         DC    AL1(ACCT,2,6)                       CLIENT STATUS BYTES          
         DC    AL1(WRITEOFF)                       EXTRA STATUS                 
         DC    CL30'BAD DEBTS'                     DESCRIPTION                  
*                                                                               
*                                                                               
         DC    CL14'SR'                            ACCOUNT                      
         DC    AL1(0)                              L'ACCT TO MATCH              
         DC    AL1(DR)                             STATUS                       
         DC    AL1(45)                             DDS BATCH TYPE               
         DC    CL14'  00CD ALLOWED'                C/A                          
         DC    C'29'                               MCKIM JE NUMBER              
         DC    C'1202'                             MCKIM GL ACCOUNT             
         DC    AL1(ACCT,2,6)                       CLIENT STATUS BYTES          
         DC    AL1(0)                              EXTRA STATUS                 
         DC    CL30'CD ALLOWED (ACCTS RCVBLE)'     DESCRIPTION                  
*                                                                               
*                                                                               
         DC    CL14'SR'                            ACCOUNT                      
         DC    AL1(0)                              L'ACCT TO MATCH              
         DC    AL1(DR)                             STATUS                       
         DC    AL1(45)                             DDS BATCH TYPE               
         DC    CL14'SE6855'                        C/A                          
         DC    C'29'                               MCKIM JE NUMBER              
         DC    C'1202'                             MCKIM GL ACCOUNT             
         DC    AL1(ACCT,2,6)                       CLIENT STATUS BYTES          
         DC    AL1(0)                              EXTRA STATUS                 
         DC    CL30'CD ALLOWED'                    DESCRIPTION                  
*                                                                               
*                                                                               
         DC    CL14'SR'                            ACCOUNT                      
         DC    AL1(0)                              L'ACCT TO MATCH              
         DC    AL1(DR)                             STATUS                       
         DC    AL1(45)                             DDS BATCH TYPE               
         DC    CL14'SE6853'                        C/A                          
         DC    C'29'                               MCKIM JE NUMBER              
         DC    C'1202'                             MCKIM GL ACCOUNT             
         DC    AL1(ACCT,2,6)                       CLIENT STATUS BYTES          
         DC    AL1(0)                              EXTRA STATUS                 
         DC    CL30'CD LOST'                       DESCRIPTION                  
*                                                                               
*                                                                               
         DC    CL14'SR'                            ACCOUNT                      
         DC    AL1(0)                              L'ACCT TO MATCH              
         DC    AL1(DR)                             STATUS                       
         DC    AL1(45)                             DDS BATCH TYPE               
         DC    CL14'SE6850'                        C/A                          
         DC    C'29'                               MCKIM JE NUMBER              
         DC    C'1202'                             MCKIM GL ACCOUNT             
         DC    AL1(ACCT,2,6)                       CLIENT STATUS BYTES          
         DC    AL1(0)                              EXTRA STATUS                 
         DC    CL30'CD LOST'                       DESCRIPTION                  
*                                                                               
*                                                                               
         DC    CL14'SR'                            ACCOUNT                      
         DC    AL1(0)                              L'ACCT TO MATCH              
         DC    AL1(DR)                             STATUS                       
         DC    AL1(45)                             DDS BATCH TYPE               
         DC    CL14'SV'                            C/A                          
         DC    C'36'                               MCKIM JE NUMBER              
         DC    C'1202'                             MCKIM GL ACCOUNT             
         DC    AL1(ACCT,2,6)                       CLIENT STATUS BYTES          
         DC    AL1(0)                              EXTRA STATUS                 
         DC    CL30'REFUNDS TO CLIENTS'            DESCRIPTION                  
*                                                                               
*                                                                               
         DC    CL14'SR'                            ACCOUNT                      
         DC    AL1(0)                              L'ACCT TO MATCH              
         DC    AL1(DR+CR)                          STATUS                       
         DC    AL1(45)                             DDS BATCH TYPE               
         DC    CL14' '                             C/A                          
         DC    C'6 '                               MCKIM JE NUMBER              
         DC    C'1202'                             MCKIM GL ACCOUNT             
         DC    AL1(ACCT,2,6)                       CLIENT STATUS BYTES          
         DC    AL1(0)                              EXTRA STATUS                 
         DC    CL30'ACCTS RCVBLE WRITE-OFF ADJ.'   DESCRIPTION                  
*                                                                               
*                                                                               
         DC    CL14'SR'                            ACCOUNT                      
         DC    AL1(0)                              L'ACCT TO MATCH              
         DC    AL1(DR)                             STATUS                       
         DC    AL1(0)                              DDS BATCH TYPE               
         DC    CL14' '                             C/A                          
         DC    C'24'                               MCKIM JE NUMBER              
         DC    C'1202'                             MCKIM GL ACCOUNT             
         DC    AL1(ACCT,2,6)                       CLIENT STATUS BYTES          
         DC    AL1(0)                              EXTRA STATUS                 
         DC    CL30'FINAL BILLING (ACCTS RCVBLE)'  DESCRIPTION                  
*                                                                               
*                                                                               
         DC    CL14'SR'                            ACCOUNT                      
         DC    AL1(0)                              L'ACCT TO MATCH              
         DC    AL1(CR)                             STATUS                       
         DC    AL1(0)                              DDS BATCH TYPE               
         DC    CL14' '                             C/A                          
         DC    C'21'                               MCKIM JE NUMBER              
         DC    C'1202'                             MCKIM GL ACCOUNT             
         DC    AL1(ACCT,2,6)                       CLIENT STATUS BYTES          
         DC    AL1(0)                              EXTRA STATUS                 
         DC    CL30'CASH RECEIPTS'                 DESCRIPTION                  
         EJECT ,                                                                
******************************************************************              
*              U/L=SS                                            *              
******************************************************************              
*                                                                               
         DC    CL14'SS'                            ACCOUNT                      
         DC    AL1(0)                              L'ACCT TO MATCH              
         DC    AL1(DR+CR)                          STATUS                       
         DC    AL1(5)                              DDS BATCH TYPE               
         DC    CL14' '                             C/A                          
         DC    C'22'                               MCKIM JE NUMBER              
         DC    C'2156'                             MCKIM GL ACCOUNT             
         DC    AL1(0,0,0)                          CLIENT STATUS BYTES          
         DC    AL1(0)                              EXTRA STATUS                 
         DC    CL30'CHEQUE PAYMENTS'               DESCRIPTION                  
*                                                                               
*                                                                               
         DC    CL14'SS'                            ACCOUNT                      
         DC    AL1(0)                              L'ACCT TO MATCH              
         DC    AL1(DR+CR)                          STATUS                       
         DC    AL1(37)                             DDS BATCH TYPE               
         DC    CL14' '                             C/A                          
         DC    C'30'                               MCKIM JE NUMBER              
         DC    C'2156'                             MCKIM GL ACCOUNT             
         DC    AL1(0,0,0)                          CLIENT STATUS BYTES          
         DC    AL1(0)                              EXTRA STATUS                 
         DC    CL30'CANCELLED CHEQUES'             DESCRIPTION                  
*                                                                               
*                                                                               
         DC    CL14'SS'                            ACCOUNT                      
         DC    AL1(0)                              L'ACCT TO MATCH              
         DC    AL1(CR)                             STATUS                       
         DC    AL1(45)                             DDS BATCH TYPE               
         DC    CL14'SE2026'                        C/A                          
         DC    C'75'                               MCKIM JE NUMBER              
         DC    C'2156'                             MCKIM GL ACCOUNT             
         DC    AL1(0,0,0)                          CLIENT STATUS BYTES          
         DC    AL1(0)                              EXTRA STATUS                 
         DC    CL30'PAYABLES OFFSETS'              DESCRIPTION                  
*                                                                               
*                                                                               
         DC    CL14'SS'                            ACCOUNT                      
         DC    AL1(0)                              L'ACCT TO MATCH              
         DC    AL1(CR)                             STATUS                       
         DC    AL1(45)                             DDS BATCH TYPE               
         DC    CL14'SE6830'                        C/A                          
         DC    C'28'                               MCKIM JE NUMBER              
         DC    C'2156'                             MCKIM GL ACCOUNT             
         DC    AL1(0,0,0)                          CLIENT STATUS BYTES          
         DC    AL1(0)                              EXTRA STATUS                 
         DC    CL30'FOREIGN ADJUSTMENT'            DESCRIPTION                  
*                                                                               
*                                                                               
         DC    CL14'SS'                            ACCOUNT                      
         DC    AL1(0)                              L'ACCT TO MATCH              
         DC    AL1(CR)                             STATUS                       
         DC    AL1(0)                              DDS BATCH TYPE               
         DC    CL14' '                             C/A                          
         DC    C'23'                               MCKIM JE NUMBER              
         DC    C'2156'                             MCKIM GL ACCOUNT             
         DC    AL1(0,0,0)                          CLIENT STATUS BYTES          
         DC    AL1(0)                              EXTRA STATUS                 
         DC    CL30'CLEARANCES'                    DESCRIPTION                  
*                                                                               
*                                                                               
         DC    CL14'SS'                            ACCOUNT                      
         DC    AL1(0)                              L'ACCT TO MATCH              
         DC    AL1(DR)                             STATUS                       
         DC    AL1(0)                              DDS BATCH TYPE               
         DC    CL14' '                             C/A                          
         DC    C'22'                               MCKIM JE NUMBER              
         DC    C'2156'                             MCKIM GL ACCOUNT             
         DC    AL1(0,0,0)                          CLIENT STATUS BYTES          
         DC    AL1(0)                              EXTRA STATUS                 
         DC    CL30'CHEQUE PAYMENTS'               DESCRIPTION                  
         EJECT ,                                                                
******************************************************************              
*              U/L=ST                                            *              
******************************************************************              
*                                                                               
         DC    CL14'ST'                            ACCOUNT                      
         DC    AL1(0)                              L'ACCT TO MATCH              
         DC    AL1(DR+CR)                          STATUS                       
         DC    AL1(5)                              DDS BATCH TYPE               
         DC    CL14' '                             C/A                          
         DC    C'22'                               MCKIM JE NUMBER              
         DC    C'2155'                             MCKIM GL ACCOUNT             
         DC    AL1(0,0,0)                          CLIENT STATUS BYTES          
         DC    AL1(0)                              EXTRA STATUS                 
         DC    CL30'CHEQUE PAYMENTS'               DESCRIPTION                  
*                                                                               
*                                                                               
         DC    CL14'ST'                            ACCOUNT                      
         DC    AL1(0)                              L'ACCT TO MATCH              
         DC    AL1(DR)                             STATUS                       
         DC    AL1(36)                             DDS BATCH TYPE               
         DC    CL14' '                             C/A                          
         DC    C'31'                               MCKIM JE NUMBER              
         DC    C'2155'                             MCKIM GL ACCOUNT             
         DC    AL1(0,0,0)                          CLIENT STATUS BYTES          
         DC    AL1(0)                              EXTRA STATUS                 
         DC    CL30'MANUAL CHEQUE'                 DESCRIPTION                  
*                                                                               
*                                                                               
         DC    CL14'ST'                            ACCOUNT                      
         DC    AL1(0)                              L'ACCT TO MATCH              
         DC    AL1(DR+CR)                          STATUS                       
         DC    AL1(37)                             DDS BATCH TYPE               
         DC    CL14' '                             C/A                          
         DC    C'30'                               MCKIM JE NUMBER              
         DC    C'2155'                             MCKIM GL ACCOUNT             
         DC    AL1(0,0,0)                          CLIENT STATUS BYTES          
         DC    AL1(0)                              EXTRA STATUS                 
         DC    CL30'CANCELLED CHEQUES'             DESCRIPTION                  
*                                                                               
*                                                                               
         DC    CL14'ST'                            ACCOUNT                      
         DC    AL1(0)                              L'ACCT TO MATCH              
         DC    AL1(CR)                             STATUS                       
         DC    AL1(45)                             DDS BATCH TYPE               
         DC    CL14'SE2025'                        C/A                          
         DC    C'75'                               MCKIM JE NUMBER              
         DC    C'2155'                             MCKIM GL ACCOUNT             
         DC    AL1(0,0,0)                          CLIENT STATUS BYTES          
         DC    AL1(0)                              EXTRA STATUS                 
         DC    CL30'PAYABLES OFFSETS'              DESCRIPTION                  
*                                                                               
*                                                                               
         DC    CL14'ST'                            ACCOUNT                      
         DC    AL1(0)                              L'ACCT TO MATCH              
         DC    AL1(CR)                             STATUS                       
         DC    AL1(45)                             DDS BATCH TYPE               
         DC    CL14'SE6830'                        C/A                          
         DC    C'28'                               MCKIM JE NUMBER              
         DC    C'2155'                             MCKIM GL ACCOUNT             
         DC    AL1(0,0,0)                          CLIENT STATUS BYTES          
         DC    AL1(0)                              EXTRA STATUS                 
         DC    CL30'FOREIGN ADJUSTMENT'            DESCRIPTION                  
*                                                                               
*                                                                               
         DC    CL14'ST'                            ACCOUNT                      
         DC    AL1(0)                              L'ACCT TO MATCH              
         DC    AL1(CR)                             STATUS                       
         DC    AL1(0)                              DDS BATCH TYPE               
         DC    CL14' '                             C/A                          
         DC    C'23'                               MCKIM JE NUMBER              
         DC    C'2155'                             MCKIM GL ACCOUNT             
         DC    AL1(0,0,0)                          CLIENT STATUS BYTES          
         DC    AL1(0)                              EXTRA STATUS                 
         DC    CL30'CLEARANCES'                    DESCRIPTION                  
*                                                                               
*                                                                               
         DC    CL14'ST'                            ACCOUNT                      
         DC    AL1(0)                              L'ACCT TO MATCH              
         DC    AL1(DR)                             STATUS                       
         DC    AL1(0)                              DDS BATCH TYPE               
         DC    CL14' '                             C/A                          
         DC    C'22'                               MCKIM JE NUMBER              
         DC    C'2155'                             MCKIM GL ACCOUNT             
         DC    AL1(0,0,0)                          CLIENT STATUS BYTES          
         DC    AL1(0)                              EXTRA STATUS                 
         DC    CL30'CHEQUE PAYMENTS'               DESCRIPTION                  
         EJECT ,                                                                
******************************************************************              
*              U/L=SV                                            *              
******************************************************************              
*                                                                               
         DC    CL14'SV'                            ACCOUNT                      
         DC    AL1(0)                              L'ACCT TO MATCH              
         DC    AL1(CR)                             STATUS                       
         DC    AL1(1)                              DDS BATCH TYPE               
         DC    CL14'SJ'                            C/A                          
         DC    C'45'                               MCKIM JE NUMBER              
         DC    C'2290'                             MCKIM GL ACCOUNT             
         DC    AL1(0,0,0)                          CLIENT STATUS BYTES          
         DC    AL1(0)                              EXTRA STATUS                 
         DC    CL30'PRODUCTION ALLOCATION'         DESCRIPTION                  
*                                                                               
*                                                                               
         DC    CL14'SV'                            ACCOUNT                      
         DC    AL1(0)                              L'ACCT TO MATCH              
         DC    AL1(DR+CR)                          STATUS                       
         DC    AL1(9)                              DDS BATCH TYPE               
         DC    CL14' '                             C/A                          
         DC    C'24'                               MCKIM JE NUMBER              
         DC    C'2570'                             MCKIM GL ACCOUNT             
         DC    AL1(0,0,0)                          CLIENT STATUS BYTES          
         DC    AL1(0)                              EXTRA STATUS                 
         DC    CL30'FINAL BILLING (ACCTS RCVBLE)'  DESCRIPTION                  
*                                                                               
*                                                                               
         DC    CL14'SV'                            ACCOUNT                      
         DC    AL1(0)                              L'ACCT TO MATCH              
         DC    AL1(CR)                             STATUS                       
         DC    AL1(33)                             DDS BATCH TYPE               
         DC    CL14'SJ'                            C/A                          
         DC    C'45'                               MCKIM JE NUMBER              
         DC    C'2290'                             MCKIM GL ACCOUNT             
         DC    AL1(0,0,0)                          CLIENT STATUS BYTES          
         DC    AL1(0)                              EXTRA STATUS                 
         DC    CL30'PRODUCTION ALLOCATION'         DESCRIPTION                  
*                                                                               
*                                                                               
         DC    CL14'SVA'                           ACCOUNT                      
         DC    AL1(1)                              L'ACCT TO MATCH              
         DC    AL1(DR)                             STATUS                       
         DC    AL1(36)                             DDS BATCH TYPE               
         DC    CL14' '                             C/A                          
         DC    C'31'                               MCKIM JE NUMBER              
         DC    C'2570'                             MCKIM GL ACCOUNT             
         DC    AL1(0,0,0)                          CLIENT STATUS BYTES          
         DC    AL1(0)                              EXTRA STATUS                 
         DC    CL30'MANUAL CHEQUE'                 DESCRIPTION                  
*                                                                               
*                                                                               
         DC    CL14'SVP'                           ACCOUNT                      
         DC    AL1(1)                              L'ACCT TO MATCH              
         DC    AL1(DR)                             STATUS                       
         DC    AL1(36)                             DDS BATCH TYPE               
         DC    CL14' '                             C/A                          
         DC    C'31'                               MCKIM JE NUMBER              
         DC    C'2290'                             MCKIM GL ACCOUNT             
         DC    AL1(0,0,0)                          CLIENT STATUS BYTES          
         DC    AL1(0)                              EXTRA STATUS                 
         DC    CL30'MANUAL CHEQUE'                 DESCRIPTION                  
*                                                                               
*                                                                               
         DC    CL14'SV'                            ACCOUNT                      
         DC    AL1(0)                              L'ACCT TO MATCH              
         DC    AL1(DR+CR)                          STATUS                       
         DC    AL1(37)                             DDS BATCH TYPE               
         DC    CL14' '                             C/A                          
         DC    C'30'                               MCKIM JE NUMBER              
         DC    C'2290'                             MCKIM GL ACCOUNT             
         DC    AL1(0,0,0)                          CLIENT STATUS BYTES          
         DC    AL1(0)                              EXTRA STATUS                 
         DC    CL30'CANCELLED CHEQUES'             DESCRIPTION                  
*                                                                               
*                                                                               
         DC    CL14'SV'                            ACCOUNT                      
         DC    AL1(0)                              L'ACCT TO MATCH              
         DC    AL1(CR)                             STATUS                       
         DC    AL1(45)                             DDS BATCH TYPE               
         DC    CL14'SC'                            C/A                          
         DC    C'29'                               MCKIM JE NUMBER              
         DC    C'    '                             MCKIM GL ACCOUNT             
         DC    AL1(0,0,0)                          CLIENT STATUS BYTES          
         DC    AL1(0)                              EXTRA STATUS                 
         DC    CL30'CD LOST/REFUNDS'               DESCRIPTION                  
*                                                                               
*                                                                               
         DC    CL14'SVP'                           ACCOUNT                      
         DC    AL1(1)                              L'ACCT TO MATCH              
         DC    AL1(DR+CR)                          STATUS                       
         DC    AL1(45)                             DDS BATCH TYPE               
         DC    CL14'SC'                            C/A                          
         DC    C'22'                               MCKIM JE NUMBER              
         DC    C'2290'                             MCKIM GL ACCOUNT             
         DC    AL1(0,0,0)                          CLIENT STATUS BYTES          
         DC    AL1(0)                              EXTRA STATUS                 
         DC    CL30'CHEQUE PAYMENTS'               DESCRIPTION                  
*                                                                               
*                                                                               
         DC    CL14'SVP'                           ACCOUNT                      
         DC    AL1(1)                              L'ACCT TO MATCH              
         DC    AL1(CR)                             STATUS                       
         DC    AL1(45)                             DDS BATCH TYPE               
         DC    CL14'SQ'                            C/A                          
         DC    C'33'                               MCKIM JE NUMBER              
         DC    C'2290'                             MCKIM GL ACCOUNT             
         DC    AL1(0,0,0)                          CLIENT STATUS BYTES          
         DC    AL1(0)                              EXTRA STATUS                 
         DC    CL30'A/P TRANSFER'                  DESCRIPTION                  
*                                                                               
*                                                                               
         DC    CL14'SV'                            ACCOUNT                      
         DC    AL1(0)                              L'ACCT TO MATCH              
         DC    AL1(CR)                             STATUS                       
         DC    AL1(51)                             DDS BATCH TYPE               
         DC    CL14'SJ'                            C/A                          
         DC    C'24'                               MCKIM JE NUMBER              
         DC    C'2570'                             MCKIM GL ACCOUNT             
         DC    AL1(0,0,0)                          CLIENT STATUS BYTES          
         DC    AL1(0)                              EXTRA STATUS                 
         DC    CL30'FINAL BILLING (ACCTS RCVBLE)'  DESCRIPTION                  
*                                                                               
*                                                                               
         DC    CL14'SV'                            ACCOUNT                      
         DC    AL1(0)                              L'ACCT TO MATCH              
         DC    AL1(CR)                             STATUS                       
         DC    AL1(45)                             DDS BATCH TYPE               
         DC    CL14'SR'                            C/A                          
         DC    C'36'                               MCKIM JE NUMBER              
         DC    C'    '                             MCKIM GL ACCOUNT             
         DC    AL1(0,0,0)                          CLIENT STATUS BYTES          
         DC    AL1(0)                              EXTRA STATUS                 
         DC    CL30'REFUNDS TO CLIENTS'            DESCRIPTION                  
*                                                                               
*                                                                               
         DC    CL14'SVA'                           ACCOUNT                      
         DC    AL1(1)                              L'ACCT TO MATCH              
         DC    AL1(DR)                             STATUS                       
         DC    AL1(0)                              DDS BATCH TYPE               
         DC    CL14' '                             C/A                          
         DC    C'22'                               MCKIM JE NUMBER              
         DC    C'2570'                             MCKIM GL ACCOUNT             
         DC    AL1(0,0,0)                          CLIENT STATUS BYTES          
         DC    AL1(0)                              EXTRA STATUS                 
         DC    CL30'CHEQUE PAYMENTS'               DESCRIPTION                  
*                                                                               
*                                                                               
         DC    CL14'SVP'                           ACCOUNT                      
         DC    AL1(1)                              L'ACCT TO MATCH              
         DC    AL1(DR+CR)                          STATUS                       
         DC    AL1(0)                              DDS BATCH TYPE               
         DC    CL14' '                             C/A                          
         DC    C'22'                               MCKIM JE NUMBER              
         DC    C'2290'                             MCKIM GL ACCOUNT             
         DC    AL1(0,0,0)                          CLIENT STATUS BYTES          
         DC    AL1(0)                              EXTRA STATUS                 
         DC    CL30'CHEQUE PAYMENTS'               DESCRIPTION                  
         EJECT ,                                                                
******************************************************************              
*              U/L=SW                                            *              
******************************************************************              
*                                                                               
         DC    CL14'SW'                            ACCOUNT                      
         DC    AL1(0)                              L'ACCT TO MATCH              
         DC    AL1(CR)                             STATUS                       
         DC    AL1(1)                              DDS BATCH TYPE               
         DC    CL14'SJ'                            C/A                          
         DC    C'45'                               MCKIM JE NUMBER              
         DC    C'2295'                             MCKIM GL ACCOUNT             
         DC    AL1(0,0,0)                          CLIENT STATUS BYTES          
         DC    AL1(0)                              EXTRA STATUS                 
         DC    CL30'PRODUCTION ALLOCATION'         DESCRIPTION                  
*                                                                               
*                                                                               
         DC    CL14'SW'                            ACCOUNT                      
         DC    AL1(0)                              L'ACCT TO MATCH              
         DC    AL1(DR+CR)                          STATUS                       
         DC    AL1(37)                             DDS BATCH TYPE               
         DC    CL14' '                             C/A                          
         DC    C'30'                               MCKIM JE NUMBER              
         DC    C'2295'                             MCKIM GL ACCOUNT             
         DC    AL1(0,0,0)                          CLIENT STATUS BYTES          
         DC    AL1(0)                              EXTRA STATUS                 
         DC    CL30'CANCELLED CHEQUES'             DESCRIPTION                  
*                                                                               
*                                                                               
         DC    CL14'SW'                            ACCOUNT                      
         DC    AL1(0)                              L'ACCT TO MATCH              
         DC    AL1(DR)                             STATUS                       
         DC    AL1(0)                              DDS BATCH TYPE               
         DC    CL14' '                             C/A                          
         DC    C'22'                               MCKIM JE NUMBER              
         DC    C'2295'                             MCKIM GL ACCOUNT             
         DC    AL1(0,0,0)                          CLIENT STATUS BYTES          
         DC    AL1(0)                              EXTRA STATUS                 
         DC    CL30'CHEQUE PAYMENTS'               DESCRIPTION                  
         EJECT ,                                                                
******************************************************************              
*              U/L=SZ                                            *              
******************************************************************              
*                                                                               
         DC    CL14'SZP'                           ACCOUNT                      
         DC    AL1(1)                              L'ACCT TO MATCH              
         DC    AL1(DR)                             STATUS                       
         DC    AL1(0)                              DDS BATCH TYPE               
         DC    CL14' '                             C/A                          
         DC    C'27'                               MCKIM JE NUMBER              
         DC    C'2165'                             MCKIM GL ACCOUNT             
         DC    AL1(0,0,0)                          CLIENT STATUS BYTES          
         DC    AL1(0)                              EXTRA STATUS                 
         DC    CL30'CLEARANCES'                    DESCRIPTION                  
*                                                                               
*                                                                               
         DC    CL14'SZS'                           ACCOUNT                      
         DC    AL1(1)                              L'ACCT TO MATCH              
         DC    AL1(DR)                             STATUS                       
         DC    AL1(0)                              DDS BATCH TYPE               
         DC    CL14' '                             C/A                          
         DC    C'23'                               MCKIM JE NUMBER              
         DC    C'2165'                             MCKIM GL ACCOUNT             
         DC    AL1(0,0,0)                          CLIENT STATUS BYTES          
         DC    AL1(0)                              EXTRA STATUS                 
         DC    CL30'CLEARANCES'                    DESCRIPTION                  
*                                                                               
*                                                                               
         DC    CL14'SZ'                            ACCOUNT                      
         DC    AL1(0)                              L'ACCT TO MATCH              
         DC    AL1(CR)                             STATUS                       
         DC    AL1(0)                              DDS BATCH TYPE               
         DC    CL14' '                             C/A                          
         DC    C'24'                               MCKIM JE NUMBER              
         DC    C'2165'                             MCKIM GL ACCOUNT             
         DC    AL1(0,0,0)                          CLIENT STATUS BYTES          
         DC    AL1(0)                              EXTRA STATUS                 
         DC    CL30'FINAL BILLING (ACCTS RCVBLE)'  DESCRIPTION                  
*                                                                               
*                                                                               
         DC    X'FF'                               END OF TABLE                 
         EJECT ,                                                                
******************************************************************              
*        STORAGE                                                 *              
******************************************************************              
*                                                                               
SORTREC  DS    0D                  WHERE SORT RECORD IS BUILT                   
         DS    200C                                                             
*                                                                               
IOAREA1  DS    0D                  IOAREA #1                                    
         DS    2000C                                                            
*                                                                               
IOAREA2  DS    0D                  IOAREA #2                                    
         DS    2000C                                                            
*                                                                               
SORTAREA DS    0D                  SORT BUFFER                                  
         DS    10000C                                                           
         EJECT ,                                                                
******************************************************************              
*        DSECT TO COVER SAVE W/S                                 *              
******************************************************************              
*                                                                               
LWSD     DSECT                                                                  
VTYPES   DS    0A                  EXTERNAL ADDRESSES                           
VSORTER  DS    A                   SORTER                                       
ASRTREC  DS    A                   A(SORT RECORD - SORTREC)                     
ASRTAREA DS    A                   A(SORT AREA - SORTAREA)                      
AIOAREA1 DS    A                   IO AREA #1                                   
AIOAREA2 DS    A                   IO AREA #2                                   
ALEDGDEF DS    A                   LEDGER DEFAULT TABLE                         
DATVAL   DS    A                   DATVAL                                       
VTYPLNQ  EQU   *-VTYPES                                                         
*                                                                               
ADBOX    DS    A                   ADDRESS OF BOX ROUTINE                       
*                                                                               
TOTSW    DS    XL1                                                              
TOT1     EQU   X'01'                                                            
TOT2     EQU   X'02'                                                            
TOT3     EQU   X'04'                                                            
TOT4     EQU   X'08'                                                            
TOT5     EQU   X'10'                                                            
TOT6     EQU   X'20'                                                            
TOTRP    EQU   X'40'               REPORT TOTAL                                 
*                                                                               
MYSUBPRG DS    XL1                 HEADLINE INDICATOR                           
ELCODE   DS    XL1                 USED IN GETEL ROUTINE                        
TMPBYTE  DS    XL1                                                              
FIRSTSW  DS    CL1                 INDICATES FIRST TIME IN                      
CURUNIT  DS    CL4                                                              
*                                                                               
TAPECNT  DS    PL8                 NUMBER OF RECORDS PUT TO TAPE                
TOTAL1   DS    PL8                 UNIT TOTAL                                   
TOTAL2   DS    PL8                 JE TOTAL                                     
TOTAL3   DS    PL8                 MOA TOTAL                                    
TOTAL4   DS    PL8                 ACCT TOTAL                                   
TOTAL5   DS    PL8                 CLIENT TOTAL                                 
TOTAL6   DS    PL8                 TRNS DATE TOTAL                              
TOTREP   DS    PL8                 REPORT TOTAL                                 
WRK2     DS    CL50                                                             
RKEY     DS    CL42                                                             
SAVESRT  DS    CL(L'SRTKEY)        SAVE PREVIOUS SORT KEY                       
TAPEREC  DS    CL(TPLNQ)           TAPE BUFFER                                  
PRNTREC  DS    CL(PRLNQ)           PRINT BUFFER                                 
DUMMY    DS    CL1                                                              
         EJECT ,                                                                
******************************************************************              
*        SORT RECORD DSECT                                       *              
******************************************************************              
*                                                                               
SRTRECD  DSECT                                                                  
SRTKEY   DS    0CL21                                                            
SRTUNIT  DS    CL4                 UNIT(OFFICE) -SET TO HDS CODES               
SRTSRCE  DS    CL2                 SOURCE JE#   -DETERMINED FROM TBLE           
SRTENTP  DS    PL2                 ENTRY PERIOD -YYMM                           
SRTACC   DS    CL4                 ACCOUNT NUM  -MCKIM ACCT                     
SRTCLI   DS    CL6                 CLIENT/PROD                                  
SRTTRDTE DS    PL3                 TRANS DATE   -YYMMDD                         
SRTAMNT  DS    PL6                 TRNS AMOUNT                                  
SRTSTAT  DS    XL1                 STATUS                                       
SRTTYPE  DS    CL1                 BATCH TYPE                                   
SRTACC2  DS    CL14                HDS ACCT                                     
SRTCAC   DS    CL14                CONTRA ACCOUNT                               
SRTDESC  DS    CL30                DESCRIPTION                                  
SRTLNQ   EQU   *-SRTRECD                                                        
         EJECT ,                                                                
******************************************************************              
*        TAPE OUTPUT DSECT                                       *              
******************************************************************              
*                                                                               
PRNRECD  DSECT                                                                  
PRHDS    DS    CL4                 UNIT BATCH   -SET TO 'HDS'                   
PRNUM    DS    CL4                 BATCH NONUM  -SET TO BLANKS                  
PRBALPHA DS    CL2                 BATCH NOALPHA-SET TO BLANKS                  
PRUNIT   DS    CL4                 UNIT(OFFICE) -SET TO HDS CODES               
PRACC    DS    CL8                 ACCOUNT NUM  -LAST 4 CHARS OF HDS#           
PRTRNDTE DS    CL6                 TRANS DATE   -TRANSACTION DATE               
PRPROJCT DS    CL8                 PROJECT      -CLIENT CODE                    
PRREF    DS    CL8                 REFERENCE    -SET TO BLANKS                  
PRAMNTP  DS    PL8                 TRNS AMOUNT  -PACKED                         
PRAMOUNT DS    CL12                TRNS AMOUNT  -CHAR FORMAT                    
PRINTR   DS    CL4                 INTER UNIT   -SET TO BLANKS                  
PRREMS   DS    CL30                REMARKS      -TAKEN FROM JE# TBLE            
PRSOURCE DS    CL2                 SOURCE JE#   -DETERMINED FROM TBLE           
PRENTDTE DS    CL6                 ENTRY DATE   -RUN ON DATE                    
PRENTPER DS    CL4                 ENTRY PERIOD -MOA                            
PRACTPER DS    CL4                 ACTUAL PERIOD-SET TO C'00'                   
PRTRNPER DS    CL4                 TRANS PERIOD -MOA                            
PRTPTOT  DS    PL8                 TAPE TOTAL                                   
PRTYPE   DS    CL1                 BATCH TYPE                                   
PRACC2   DS    CL14                HDS ACCT                                     
PRLNQ    EQU   *-PRNRECD                                                        
         EJECT ,                                                                
******************************************************************              
*        PRINT LINE DSECT                                        *              
******************************************************************              
*                                                                               
PLINED   DSECT                                                                  
         DS    CL3                                                              
PSOURCE  DS    CL2                 SOURCE JE#                                   
         DS    CL3                                                              
PMOA     DS    CL6                                                              
         DS    CL4                                                              
PACC     DS    CL4                 MCKIM ACCOUNT                                
         DS    CL5                                                              
PPROJCT  DS    CL6                 PROJECT      -CLIENT CODE                    
         DS    CL3                                                              
PTRNDTE  DS    CL8                 TRANSACTION DATE                             
         DS    CL5                                                              
PTYPE    DS    CL3                 BATCH TYPE                                   
         DS    CL5                                                              
PULACC   DS    CL14                HDS ACCOUNT CODE                             
         DS    CL3                                                              
PREMS    DS    CL30                REMARKS                                      
         DS    CL4                                                              
PAMOUNT  DS    CL14                AMOUNT                                       
         DS    CL3                                                              
PTAPE    DS    CL14                TAPE SUBTOTAL                                
PLNQ     EQU   *-PLINED                                                         
         ORG   PREMS                                                            
PTOTAL   DS    CL30                ROOM TO PRINT TOTAL LINES                    
         EJECT ,                                                                
******************************************************************              
*        ERROR PRINT DSECT                                       *              
******************************************************************              
*                                                                               
PERRD    DSECT                                                                  
         DS    CL5                                                              
PERRACC  DS    CL14                                                             
         DS    CL5                                                              
PERRCACC DS    CL14                                                             
         DS    CL5                                                              
PERRSTAT DS    CL2                                                              
         DS    CL5                                                              
PERRTYPE DS    CL3                                                              
         DS    CL5                                                              
PERRBREF DS    CL6                                                              
         DS    CL5                                                              
PERRDATE DS    CL8                                                              
         DS    CL5                                                              
PERRREF  DS    CL6                                                              
         DS    CL5                                                              
PERROFFC DS    CL2                                                              
         DS    CL5                                                              
PERRAMT  DS    CL12                                                             
         EJECT ,                                                                
******************************************************************              
*        TAPE OUTPUT DSECT                                       *              
******************************************************************              
*                                                                               
TPRECD   DSECT                                                                  
TPHDS    DS    CL4                 UNIT BATCH   -SET TO 'HDS'                   
TPNUM    DS    CL4                 BATCH NONUM  -SET TO BLANKS                  
TPBALPHA DS    CL2                 BATCH NOALPHA-SET TO BLANKS                  
TPUNIT   DS    CL4                 UNIT(OFFICE) -SET TO HDS CODES               
TPACC    DS    CL8                 ACCOUNT NUM  -LAST 4 CHARS OF HDS#           
TPTRNDTE DS    CL6                 TRANS DATE   -TRANSACTION DATE               
TPPROJCT DS    CL8                 PROJECT      -CLIENT CODE                    
TPREF    DS    CL8                 REFERENCE    -SET TO BLANKS                  
TPAMOUNT DS    CL12                TRNS AMOUNT  -CHAR FORMAT                    
TPINTER  DS    CL4                 INTER UNIT   -SET TO BLANKS                  
TPREMS   DS    CL30                REMARKS      -TAKEN FROM JE# TBLE            
TPSOURCE DS    CL2                 SOURCE JE#   -DETERMINED FROM TBLE           
TPENTDTE DS    CL6                 ENTRY DATE   -RUN ON DATE                    
TPENTPER DS    CL4                 ENTRY PERIOD -MOA                            
TPACTPER DS    CL4                 ACTUAL PERIOD-SET TO C'00'                   
TPTRNPER DS    CL4                 TRANS PERIOD -MOA                            
         DS    CL120               SPARE                                        
TPLNQ    EQU   *-TPRECD                                                         
         EJECT ,                                                                
******************************************************************              
*        DEFAULT TABLE DSECT                                     *              
******************************************************************              
*                                                                               
DFTBLD   DSECT                                                                  
DFUL     DS    CL2                 UNIT/LEDGER                                  
DFACC    DS    CL12                ACCOUNT                                      
DFACCLNQ DS    AL1                 L'ACCT ENTERED                               
DFSTAT   DS    XL1                 DEBIT/CREDIT                                 
DFTYPE   DS    XL1                 BATCH TYPE                                   
DFCUL    DS    CL2                 CONTRA UNIT/LEDGER                           
DFCACC   DS    CL12                CONTRA ACCOUNT                               
DFJENUM  DS    CL2                 JE#                                          
DFMKACC  DS    CL4                 DEFAULT MCKIM ACCOUNT                        
DFCLI    DS    XL1                 LOCATION OF CLIENT CODE                      
DFCLIDSP DS    XL1                 DISPLACEMENT INTO ACCOUNT                    
DFCLILNQ DS    XL1                 LENGTH OF ACCT                               
DFXSTAT  DS    XL1                 EXTRA STATUS                                 
DFDESC   DS    CL30                DESCRIPTION                                  
DFLNQ    EQU   *-DFTBLD                                                         
         EJECT ,                                                                
******************************************************************              
*        OTHER INCLUDES                                          *              
******************************************************************              
*                                                                               
*DDREPXTRAD                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDREPXTRAD                                                     
         PRINT ON                                                               
*DDREPMASTD                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDREPMASTD                                                     
         PRINT ON                                                               
*DDCNTRL                                                                        
         PRINT OFF                                                              
       ++INCLUDE DDCNTRL                                                        
         PRINT ON                                                               
*ACBIGPRNTD                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACBIGPRNTD                                                     
         PRINT ON                                                               
*DDBOXEQUS                                                                      
         PRINT OFF                                                              
       ++INCLUDE DDBOXEQUS                                                      
         PRINT ON                                                               
*DDBIGBOX                                                                       
         PRINT OFF                                                              
       ++INCLUDE DDBIGBOX                                                       
         PRINT ON                                                               
*DDLOGOD                                                                        
         PRINT OFF                                                              
       ++INCLUDE DDLOGOD                                                        
         PRINT ON                                                               
*ACGENPOST                                                                      
         PRINT OFF                                                              
       ++INCLUDE ACGENPOST                                                      
         PRINT ON                                                               
*ACREPWORKD                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACREPWORKD                                                     
         PRINT ON                                                               
*ACGENFILE                                                                      
         PRINT OFF                                                              
       ++INCLUDE ACGENFILE                                                      
         PRINT ON                                                               
*ACGENMODES                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACGENMODES                                                     
         PRINT ON                                                               
*ACMASTD                                                                        
         PRINT OFF                                                              
       ++INCLUDE ACMASTD                                                        
         PRINT ON                                                               
*DDREMOTED                                                                      
         PRINT OFF                                                              
       ++INCLUDE DDREMOTED                                                      
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'061ACREPIA02 04/10/15'                                      
         END                                                                    
