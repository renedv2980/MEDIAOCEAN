*          DATA SET PRSFM14    AT LEVEL 099 AS OF 08/17/94                      
*PHASE T41C14A                                                                  
*        TITLE 'PRSFM14 LOAD EDR DATA TO CTFILE'                                
         TITLE 'PRSFM14 LOAD EDR DATA TO CTFILE'                                
*                                                                               
T41C14   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,*T41C14*,R7,RR=RE                                              
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA      BASE SCREEN FOR SYSTEM + THIS PROG           
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
*                                                                               
         ST    RE,WRKRELO          SAVE RELOCATION FACTOR                       
         ST    RC,WRKWORKA         SAVE WORKAREA ADDRESS                        
*                                                                               
         MVI   USEIO,C'Y'          INDICATE USER DOES IO                        
         MVI   IOOPT,C'Y'          OVERLAY DOES IO                              
         MVI   ACTELOPT,C'N'       NO ACTIVITY ELEMENTS                         
*                                                                               
         LA    RF,KEY              SET I/O AREA FOR KEYS                        
         ST    RF,AIO                                                           
*                                                                               
MAIN10   CLI   MODE,VALKEY         VALIDATE KEY                                 
         BE    VK                                                               
         CLI   MODE,PRINTREP       PRINT REPORT                                 
         BE    PR                                                               
         B     EXIT                NON-USABLE MODE                              
*                                                                               
EXIT     XIT1                                                                   
*                                                                               
         TITLE 'PRSFM14 LOAD EDR DATA TO CTFILE - VALKEY'                       
***********************************************************************         
*                                                                     *         
*        VALIDATE KEY                                                 *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
VK       DS    0H                                                               
*                                                                               
*        OPEN CONTROL SYSTEM FILES                                              
*                                                                               
         CLI   OFFLINE,C'Y'        IF OFF-LINE OPEN CONTROL SYS FILES           
         BNE   VKOPCTLX                                                         
*                                                                               
         L     RF,ATWA             POINT TO TWA                                 
         L     RF,TWAMASTC-TWATASK(RF) POINT TO MASTC                           
         L     RF,MCUTL-MCBLOCK(RF)  POINT TO UTL                               
         ST    RF,WRKUTLA          SAVE ADDRESS                                 
         MVC   WRKSYS,4(RF)        SAVE CURRENT SYSTEM ID                       
         MVI   4(RF),X'0A'         SET FOR CONTROL SYSTEM                       
*                                                                               
         GOTO1 DATAMGR,DMCB,=C'DMOPEN',=C'CONTROL',CONFILES,DMWORK              
*                                                                               
         L     RF,WRKUTLA          SWITCH BACK TO CURRENT SYSTEM                
         MVC   4(1,RF),WRKSYS                                                   
*                                                                               
         B     VKOPCTLX                                                         
*                                                                               
CONFILES DS    0D                  CONTROL SYSTEM FILE LIST                     
         DC    CL8' CTFILE'                                                     
         DC    CL8'X'              END OF LIST                                  
*                                                                               
VKOPCTLX DS    0H                                                               
*                                                                               
         MVC   AIO,AIO3                                                         
*                                                                               
VKX      DS    0H                                                               
*                                                                               
         B     EXIT                                                             
*                                                                               
         TITLE 'PRSFM14 LOAD EDR DATA TO CTFILE - PRNTREP'                      
***********************************************************************         
*                                                                     *         
*        PRINT REPORT - IN THIS CASE WE ARE UPDATING CEPRECS          *         
*                       ON CTFILE                                     *         
*                       DO THIS BY CREATING A NEW DUMPTAPE TO BE      *         
*                       FED INTO LOAD                                 *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
PR       DS    0H                                                               
*                                                                               
*        INITIALIZATION                                                         
*                                                                               
         MVI   USEIO,C'Y'          INDICATE USER DOES IO                        
         MVI   IOOPT,C'Y'          OVERLAY DOES IO                              
*                                                                               
         ZAP   WCEPIN,=P'0'        INIT RECORD COUNTERS                         
         ZAP   WEDRIN,=P'0'                                                     
         ZAP   WCEPOUT,=P'0'                                                    
         ZAP   WEDRADD,=P'0'                                                    
*                                                                               
         MVC   AIO,AIO3            SET IOAREA                                   
*                                                                               
         OPEN  (EDRSORT,OUTPUT)    OPEN OUTPUT TAPE                             
*                                                                               
         OPEN  (TOUT,OUTPUT)       OPEN OUTPUT TAPE                             
*                                                                               
         GOTO1 =A(SWTCNTL),RR=WRKRELO SWITCH TO CONTROL SYSTEM                  
*                                                                               
         OI    DMINBTS,X'08'       PASS DELETED RECORDS                         
*                                                                               
         XC    KEY,KEY             SET TO FIND FIRST RECORD ON FILE             
*                                                                               
         GOTO1 HIGH                READ IN START KEY                            
*                                                                               
PRRDLOOP DS    0H                                                               
*                                                                               
         TM    DMCB+8,X'80'        DONE AT END OF FILE                          
         BO    PRRDDONE                                                         
*                                                                               
         LA    R4,KEY              ESTABLISH EDR LINKAGE RECORD                 
         USING CEPREC,R4                                                        
*                                                                               
         CLI   WRKCEPSW,C'Y'       SKIP IF CEPRECS ALREADY ADDED                
         BE    PRRDCONT                                                         
*                                                                               
         CLC   CEPKTYP,=AL2(CEPKTYPQ) ONLY WANT CEPREC'S                        
         BL    PRRDCONT                                                         
*                                                                               
         TITLE 'PRSFM14 LOAD EDR DATA TO CTFILE - PRCTLOOP'                     
***********************************************************************         
*                                                                     *         
*        ADD CONTROL FILE CEPREC'S TO SORT                            *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
PRCT     DS    0H                                                               
*                                                                               
*        INITIALIZE SORT                                                        
*                                                                               
         GOTO1 SORTER,DMCB,SORTCRD1,RECCRD1 INITIALIZE SORT                     
*                                                                               
*        ADD CEPREC'S FROM CTFILE TO SORT                                       
*                                                                               
PRCTLOOP DS    0H                                                               
*                                                                               
         LA    R4,KEY              ESTABLISH EDR LINKAGE RECORD                 
         USING CEPREC,R4                                                        
*                                                                               
         CLC   CEPKTYP,=AL2(CEPKTYPQ) DONE WHEN CEPREC'S EXHAUSTED              
         BH    PRCTDONE                                                         
*                                                                               
         L     R4,AIO              POINT TO FOUND RECORD                        
*                                                                               
         LA    R6,CEPDATA          POINT TO FIRST ELEMENT                       
         USING CEDRELD,R6          ESTABLISH AS EDR ELEMENT                     
*                                                                               
         XC    CEPKPSQN,CEPKPSQN   CLEAR PUB     SEQUENCE NUMBER                
         XC    CEPKESQN,CEPKESQN   CLEAR EDITION SEQUENCE NUMBER                
*                                                                               
         GOTO1 DELSPC,DMCB,(L'CEDREDN,CEDREDN) STRIP LEADING SPACES             
         GOTO1 DELSPC,DMCB,(L'CEDRCTYP,CEDRCTYP) STRIP LEADING SPACES           
*                                                                               
         AP    WCEPIN,=P'1'        BUMP RECORDS IN COUNTER                      
*                                                                               
         MVI   L'CEPRECSV(R4),0    FLAG AS FROM FILE                            
*                                                                               
         GOTO1 SORTER,DMCB,=C'PUT',AIO   ADD RECORD TO SORT                     
*                                                                               
PRCTCONT DS    0H                                                               
*                                                                               
         GOTO1 SEQ                 READ NEXT RECORD                             
*                                                                               
         B     PRCTLOOP                                                         
*                                                                               
PRCTDONE DS    0H                                                               
         TITLE 'PRSFM14 LOAD EDR DATA TO CTFILE - PREDLOOP'                     
***********************************************************************         
*                                                                     *         
*        ADD EDR FILE RECORDS TO SORT                                 *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
PRED     DS    0H                                                               
*                                                                               
         OPEN  (EDRFILE,INPUT)     OPEN EDRFILE AS INPUT                        
*                                                                               
         LA    R3,EDRREC           ESTABLISH EDRREC                             
         USING EDRRECD,R3                                                       
*                                                                               
         L     R4,AIO              BUILD CEPREC IN CURRENT I/O AREA             
         USING CEPREC,R4           ESTABLISH CEPREC                             
*                                                                               
PREDLOOP DS    0H                                                               
*                                                                               
         GET   EDRFILE,EDRREC      READ IN EDR RECORD                           
*                                                                               
         CLC   EDRPUB,SPACES       IGNORE IF NO PUB NAME                        
         BNH   PREDCONT                                                         
*                                                                               
         XC    CEPKEY,CEPKEY       INIT KEY                                     
*                                                                               
         MVC   CEPKTYP,=AL2(CEPKTYPQ)  SET RECORD ID                            
         MVC   CEPKPSRT,EDRPUB     SET PUB SORT KEY                             
         MVC   CEPLEN,=AL2(CEPDATA-CEPREC+CEDRELML)  RECORD LENGTH              
*                                                                               
         LA    R6,CEPDATA          POINT TO FIRST ELEMENT                       
         USING CEDRELD,R6          ESTABLISH AS EDR ELEMENT                     
*                                                                               
         XC    CEDRELD(CEDRELML),CEDRELD INIT EDR ELEMENT                       
*                                                                               
         MVI   CEDRELEM,CEDRELTQ   SET ELEMENT ID                               
         MVC   CEDRNAME,EDRPUB     SET PUB NAME                                 
         MVI   CEDRLEN,CEDRELML    SET ELEMENT LENGTH                           
         MVC   CEDREDN,EDREDN      SET EDITION                                  
         GOTO1 DELSPC,DMCB,(L'CEDREDN,CEDREDN) STRIP LEADING SPACES             
         MVC   CEDRCTYP,EDRCRD     SET CARD TYPE                                
         GOTO1 DELSPC,DMCB,(L'CEDRCTYP,CEDRCTYP) STRIP LEADING SPACES           
         MVC   CEDRMED,EDRMED      SET EDR MEDIA                                
         MVC   CEDRPID,EDRPAKID    SET EDR ID                                   
*                                                                               
         MVI   CEDRELML(R6),X'FF'  FLAG AS EDRFILE RECORD                       
*                                                                               
         AP    WEDRIN,=P'1'        BUMP EDR IN COUNTER                          
*                                                                               
         GOTO1 SORTER,DMCB,=C'PUT',AIO  ADD TO SORT                             
*                                                                               
PREDCONT DS    0H                                                               
*                                                                               
         B     PREDLOOP                                                         
*                                                                               
PREDDONE DS    0H                  EDRFILE EXHAUSTED                            
*                                                                               
         CLOSE EDRFILE             CLOSE INPUT TAPE                             
*                                                                               
         TITLE 'PRSFM14 ELIMINATE DUPLICATE PAKIDS - PRPKLOOP'                  
***********************************************************************         
*                                                                     *         
*        RECORDS SORTED BY PAKID - PUT ONLY ONE COPY TO INTERMEDIATE  *         
*        FILE - ANY FROM EDR FILE COME FIRST                          *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
PRPK     DS    0H                                                               
*                                                                               
         XC    CEPRECSV,CEPRECSV   INIT SORT RECORD SAVEAREA                    
         ZAP   WRKDUPS,=P'0'       DUPLICATES COUNTER                           
         ZAP   WEDRDUPS,=P'0'      EDR DUPLICATES COUNTER                       
         ZAP   WCEPOLD,=P'0'       CTFILE UNMATCHED RECORDS                     
*                                                                               
PRPKLOOP DS    0H                                                               
*                                                                               
         GOTO1 SORTER,DMCB,=C'GET' GET RECORD FROM SORT                         
         ICM   R4,15,4(R1)         A(RETURNED RECORD)                           
         BZ    PRPKDONE            EOF                                          
*                                                                               
         USING CEPREC,R4           ESTABLISH CEPREC                             
*                                                                               
         CLC   CEPRECSV,0(R4)      IF SAME AS LAST RECORD                       
         BNE   PRPKLP05                                                         
*                                                                               
         CLI   CEPRECSV+L'CEPRECSV,X'FF' IF MASTER FROM EDR FILE                
         BNE   *+10                                                             
         SP    WEDRADD,=P'1'             DECREMENT EDR RECS ADDED CTR           
*                                                                               
         B     PRPKCONT               DROP RECORD                               
*                                                                               
PRPKLP05 DS    0H                                                               
*                                                                               
         LA    R6,CEPDATA          POINT TO FIRST ELEMENT                       
         USING CEDRELD,R6          ESTABLISH AS EDR ELEMENT                     
*                                                                               
         LA    R1,CEDRPID          GET DISPLACEMENT OF PAKID                    
         SR    R1,R4                                                            
*                                                                               
         LA    R1,CEPRECSV(R1)     POINT TO OLD PAKID                           
*                                                                               
         CLC   CEDRPID,0(R1)       IF PAKID IS UNCHANGED                        
         BNE   PRPKLP10                                                         
*                                                                               
         CLI   L'CEPRECSV(R4),X'FF' IF AN EDR RECORD                            
         BNE   *+14                                                             
         AP    WEDRDUPS,=P'1'          BUMP EDR DUPLICATES COUNTER              
         B     PRPKLP07                                                         
*                                                                               
         AP    WRKDUPS,=P'1'          BUMP DUPS DROPPED CTR                     
*                                                                               
PRPKLP07 DS    0H                                                               
*                                                                               
         B     PRPKCONT               DROP RECORD                               
*                                                                               
PRPKLP10 DS    0H                                                               
*                                                                               
         CLI   L'CEPRECSV(R4),X'FF'  IF FROM EDR FILE                           
         BNE   *+14                                                             
         AP    WEDRADD,=P'1'            BUMP EDR RECORD ADDED COUNTER           
         B     *+10                                                             
         AP    WCEPOLD,=P'1'       ELSE BUMP OLD PUBS CTR                       
*                                                                               
         MVC   CEPRECSV(L'CEPRECSV+1),0(R4) SAVE RECORD & EDR IND               
*                                                                               
         PUT   EDRSORT,0(R4)       WRITE TO INTERMEDIATE FILE                   
*                                                                               
PRPKCONT DS    0H                                                               
*                                                                               
         B     PRPKLOOP                                                         
*                                                                               
PRPKDONE DS    0H                                                               
*                                                                               
         GOTO1 SORTER,DMCB,=C'END' SHUT DOWN SORT                               
*                                                                               
         CLOSE EDRSORT             CLOSE INTERMEDIATE FILE                      
*                                                                               
         TITLE 'PRSFM14 SORT INTERMEDIATE FILE - PRSRLOOP'                      
***********************************************************************         
*                                                                     *         
*        SORT RECORDS IN INTERMEDIATE FILE BY TEXT IN KEY             *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
PRSR     DS    0H                                                               
*                                                                               
         OPEN  (EDRSORT,INPUT)     RE-OPEN INTERMEDIATE FILE                    
*                                                                               
         GOTO1 SORTER,DMCB,SORTCARD,RECCARD INITIALIZE SORT                     
*                                                                               
PRSRLOOP DS    0H                                                               
*                                                                               
         GET   EDRSORT,EDRREC      READ INTERMEDIATE FILE                       
*                                                                               
         GOTO1 SORTER,DMCB,=C'PUT',EDRREC                                       
*                                                                               
PRSRCONT DS    0H                                                               
*                                                                               
         B     PRSRLOOP                                                         
*                                                                               
PRSRDONE DS    0H                                                               
*                                                                               
         CLOSE EDRSORT                                                          
*                                                                               
         TITLE 'PRSFM14 LOAD EDR DATA TO CTFILE - PRWRLOOP'                     
***********************************************************************         
*                                                                     *         
*        ADD SORTED CEPREC'S TO OUTPUT TAPE                           *         
*        ALSO PRINT LISTING OF RECORDS ADDED                          *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
PRWR     DS    0H                                                               
*                                                                               
         LA    R1,EDRSPECS         SET SPECS ADDRESS                            
         ST    R1,SPECS                                                         
*                                                                               
         LA    R2,P                USE OFF-LINE PRINT AREA                      
         USING RPLINED,R2          ESTABLISH REPORT LINE                        
*                                                                               
         L     R3,AIO              POINT TO OUTPUT BUILD AREA                   
         LA    R4,4(R3)            POINT TO CEPREC AREA IN OUTPUT               
*                                                                               
         MVC   WRKPSQN,=AL2(1)     INIT PUB     SEQUENCE NUMBER                 
         MVC   WRKESQN,=AL2(1)     INIT EDITION SEQUENCE NUMBER                 
*                                                                               
         XC    CEPRECSV,CEPRECSV   INIT SORT RECORD SAVEAREA                    
*                                                                               
PRWRLOOP DS    0H                                                               
*                                                                               
         MVC   RPLINE,SPACES       INIT REPORT LINE                             
*                                                                               
         GOTO1 SORTER,DMCB,=C'GET' GET RECORD FROM SORT                         
         ICM   R5,15,4(R1)         A(RETURNED RECORD)                           
         BZ    PRWRDONE            EOF                                          
*                                                                               
         CLC   CEPRECSV,0(R5)      SKIP IF SAME AS LAST RECORD                  
         BE    PRWRCONT                                                         
*                                                                               
         MVC   CEPREC(CEPDATA-CEPREC+CEDRELML),0(R5)  COPY SORT REC             
*                                                                               
         XC    0(4,R3),0(R3)       INIT RECORD LENGTH AREA                      
         SR    RF,RF                                                            
         ICM   RF,3,CEPLEN         GET RECORD LENGTH                            
         LA    RF,4(RF)            ADD 4 FOR LRECL OF VB RECORD                 
         STCM  RF,3,0(R3)          SET RECORD LENGTH                            
*                                                                               
         LA    R6,CEPDATA          POINT TO FIRST ELEMENT                       
         USING CEDRELD,R6          ESTABLISH AS EDR ELEMENT                     
*                                                                               
         LA    R1,CEPDATA-CEPREC+CEPRECSV   FIRST ELM IN SAVED REC              
*                                                                               
         CLC   CEDRNAME(L'CEPKPSRT),CEDRNAME-CEDRELD(R1)  IF SORT NAME          
         BE    PRWRSSQX            CHANGED                                      
*                                                                               
         MVC   WRKPSQN,=AL2(1)     INIT PUB     SEQUENCE NUMBER                 
         MVC   WRKESQN,=AL2(1)     INIT EDITION SEQUENCE NUMBER                 
*                                                                               
         B     PRWRESQX                                                         
*                                                                               
PRWRSSQX DS    0H                                                               
*                                                                               
         CLC   CEDRNAME,CEDRNAME-CEDRELD(R1)  IF PUBNAME CHANGED                
         BE    PRWRPSQX                                                         
*                                                                               
         SR    RF,RF                                                            
         ICM   RF,3,WRKPSQN        BUMP PUB SEQUENCE NUMBER                     
         LA    RF,1(RF)                                                         
         STCM  RF,3,WRKPSQN                                                     
         MVC   WRKESQN,=AL2(1)     INIT EDITION SEQUENCE NUMBER                 
*                                                                               
         B     PRWRESQX                                                         
*                                                                               
PRWRPSQX DS    0H                                                               
*                                                                               
         SR    RF,RF               ELSE                                         
         ICM   RF,3,WRKESQN        BUMP EDITION SEQUENCE NUMBER                 
         LA    RF,1(RF)                                                         
         STCM  RF,3,WRKESQN                                                     
*                                                                               
PRWRESQX DS    0H                                                               
*                                                                               
         MVC   CEPRECSV,CEPREC     SAVE LAST SORT RECORD                        
*                                                                               
         MVC   CEPKPSQN,WRKPSQN    SET PUB     SEQUENCE NUMBER                  
         MVC   CEPKESQN,WRKESQN    SET EDITION SEQUENCE NUMBER                  
*                                                                               
         MVC   RPEDRNAM,CEDRNAME   EDR PUBNAME                                  
         MVC   RPEDREDN,CEDREDN    EDITION NAME                                 
         MVC   RPEDRCRD,CEDRCTYP   CARD TYPE                                    
         MVC   RPEDRMED,CEDRMED    MEDIA                                        
*                                                                               
         SR    RF,RF                                                            
         ICM   RF,3,CEPKPSQN       PRINT PUB     SEQUENCE NUMBER                
         CVD   RF,DUB                                                           
         MVC   RPPSQN,=X'402020202020'  SET EDIT PATTERN                        
         ED    RPPSQN,DUB+5        PRINT SEQUENCE NUMBER                        
*                                                                               
         ICM   RF,3,CEPKESQN       PRINT EDITION SEQUENCE NUMBER                
         CVD   RF,DUB                                                           
         MVC   RPESQN,=X'402020202020'  SET EDIT PATTERN                        
         ED    RPESQN,DUB+5        PRINT SEQUENCE NUMBER                        
*                                                                               
         MVC   RPEPID,CEDRPID      EDR PAK ID                                   
*                                                                               
         AP    WCEPOUT,=P'1'       BUMP RECORDS OUT COUNTER                     
*                                                                               
         PUT   TOUT,(R3)           WRITE TO DUMP FILE                           
*                                                                               
         CLC   CONWHEN+3(3),=C'PRT' PRINT ONLY IF ASKED                         
         BNE   PRWRPRTX                                                         
*                                                                               
         GOTO1 SPOOL,DMCB,(R8)     PRINT A LINE                                 
*                                                                               
PRWRPRTX DS    0H                                                               
*                                                                               
PRWRCONT DS    0H                                                               
         B     PRWRLOOP                                                         
*                                                                               
PRWRDONE DS    0H                                                               
*                                                                               
         GOTO1 SORTER,DMCB,=C'END' SHUT DOWN SORT                               
*                                                                               
         MVI   WRKCEPSW,C'Y'       INDICATE CEPRECS DONE                        
*                                                                               
         GOTO1 HIGH                RESET CTFILE POINTER                         
*                                                                               
PRRDCONT DS    0H                                                               
*                                                                               
         L     R4,AIO              POINT TO CTFILE RECORD                       
         USING CEPREC,R4           ESTABLISH AS CEPREC                          
*                                                                               
         LR    RE,R4               START OF CTFILE RECORD                       
*                                                                               
         SR    RF,RF                                                            
         ICM   RF,3,CEPLEN         RECORD LENGTH                                
*                                                                               
         LR    R3,RF               COPY RECORD LENGTH                           
*                                                                               
         L     R2,AIO2             OUTPUT AREA                                  
         LA    R2,4(R2)            START OF RECORD IN OUTPUT AREA               
*                                                                               
         MVCL  R2,RE               MOVE RECORD TO TAPE OUTPUT AREA              
*                                                                               
         L     R2,AIO2             START OF TAPE OUTPUT RECORD                  
*                                                                               
         XC    0(4,R2),0(R2)       INIT RECORD LENGTH AREA                      
*                                                                               
         SR    RF,RF                                                            
         ICM   RF,3,CEPLEN         RECORD LENGTH                                
         LA    RF,4(RF)            TAPE OUTPUT RECORD LENGTH                    
         STCM  RF,3,0(R2)          SET TAPE OUTPUT RECORD LENGTH                
*                                                                               
         PUT   TOUT,(R2)           WRITE RECORD TO DUMP TAPE                    
*                                                                               
         GOTO1 SEQ                 FIND NEXT RECORD                             
*                                                                               
         B     PRRDLOOP                                                         
*                                                                               
PRRDDONE DS    0H                                                               
*                                                                               
         LA    R1,RECSPECS         SET RECORD COUNTS SPECS ADDRESS              
         ST    R1,SPECS                                                         
*                                                                               
         LA    R2,P                USE OFF-LINE PRINT AREA                      
         USING RPLINED,R2          ESTABLISH REPORT LINE                        
*                                                                               
         MVI   FORCEHED,C'Y'       FORCE NEW PAGE                               
*                                                                               
         MVC   RPLINE1,SPACES      INIT PRINT LINE                              
         MVC   RPTITLE,=CL35'EDR RECORDS IN FROM UPDATE FILE'                   
         MVC   RPCOUNT,=X'40202020202020202020202020202120' ED PTRN             
         ED    RPCOUNT,WEDRIN         PRINT RECORDS IN                          
*                                                                               
         GOTO1 SPOOL,DMCB,(R8)     PRINT A LINE                                 
*                                                                               
         MVC   RPLINE1,SPACES      INIT PRINT LINE                              
         MVC   RPTITLE,=CL35'EDR RECORDS IN FROM CONTROL FILE'                  
         MVC   RPCOUNT,=X'40202020202020202020202020202120' ED PTRN             
         ED    RPCOUNT,WCEPIN         PRINT RECORDS IN                          
*                                                                               
         GOTO1 SPOOL,DMCB,(R8)     PRINT A LINE                                 
*                                                                               
         MVC   RPLINE1,SPACES      INIT PRINT LINE                              
         MVC   RPTITLE,=CL35'EDR RECORDS OUT TO CONTROL FILE'                   
         MVC   RPCOUNT,=X'40202020202020202020202020202120' ED PTRN             
         ED    RPCOUNT,WCEPOUT        PRINT RECORDS IN                          
*                                                                               
         GOTO1 SPOOL,DMCB,(R8)     PRINT A LINE                                 
*                                                                               
         MVC   RPLINE1,SPACES      INIT PRINT LINE                              
*                                                                               
         GOTO1 SPOOL,DMCB,(R8)     PRINT A LINE                                 
*                                                                               
         MVC   RPTITLE,=CL35'EDR RECORDS ADDED TO CONTROL FILE'                 
         MVC   RPCOUNT,=X'40202020202020202020202020202120' ED PTRN             
         ED    RPCOUNT,WEDRADD        PRINT RECORDS IN                          
*                                                                               
         GOTO1 SPOOL,DMCB,(R8)     PRINT A LINE                                 
*                                                                               
         MVC   RPTITLE,=CL35'DUPLICATES DROPPED'                                
         MVC   RPCOUNT,=X'40202020202020202020202020202120' ED PTRN             
         ED    RPCOUNT,WRKDUPS        PRINT RECORDS IN                          
*                                                                               
         GOTO1 SPOOL,DMCB,(R8)     PRINT A LINE                                 
*                                                                               
         MVC   RPTITLE,=CL35'DUPLICATES ON EDR FILE'                            
         MVC   RPCOUNT,=X'40202020202020202020202020202120' ED PTRN             
         ED    RPCOUNT,WEDRDUPS       PRINT RECORDS IN                          
*                                                                               
         GOTO1 SPOOL,DMCB,(R8)     PRINT A LINE                                 
*                                                                               
         MVC   RPTITLE,=CL35'UNMATCHED CTFILE RECS '                            
         MVC   RPCOUNT,=X'40202020202020202020202020202120' ED PTRN             
         ED    RPCOUNT,WCEPOLD        PRINT RECORDS IN                          
*                                                                               
         GOTO1 SPOOL,DMCB,(R8)     PRINT A LINE                                 
*                                                                               
PRX      DS    0H                                                               
*                                                                               
         GOTO1 =A(SWTBACK),RR=WRKRELO SWITCH BACK TO USER SYSTEM                
*                                                                               
         B     EXIT                                                             
*                                                                               
*        SORT PARAMETER CARDS                                                   
*                                                                               
SORTCARD DC    CL80'SORT FIELDS=(1,129,A),FORMAT=BI,WORK=1'                     
SORTCRD1 DC    CL80'SORT FIELDS=(107,8,A,129,1,D),FORMAT=BI,WORK=1'             
RECCARD  DC    CL80'RECORD TYPE=F,LENGTH=129'                                   
RECCRD1  DC    CL80'RECORD TYPE=F,LENGTH=129'                                   
*                                                                               
         TITLE 'PRSFM14 - DELETE LEADING SPACES - DELSPC'                       
***********************************************************************         
*                                                                     *         
*        DELETE LEADING SPACES FROM FIELD                             *         
*                                                                     *         
*NTRY    PARM0+0(1)   LENGTH OF FIELD                                 *         
*        PARM0+1(3)   AL3(FIELD)                                      *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
DELSPC   NTR1                                                                   
*                                                                               
         ICM   R2,15,0(R1)         POINT TO FIELD TO BE STRIPPED                
         BZ    DELSPCX             EXIT IF NO FIELD GIVEN                       
*                                                                               
         SR    R3,R3                                                            
         ICM   R3,1,0(R1)          GET FIELD LENGTH                             
         BZ    DELSPCX             EXIT IF NO LENGTH GIVEN                      
*                                                                               
         LR    RF,R2               POINT TO START OF FIELD                      
         LR    RE,R2                                                            
         LR    R4,R3               COPY FIELD LENGTH                            
         SR    R0,R0                                                            
*                                                                               
DELSPCLP DS    0H                                                               
*                                                                               
         LTR   R0,R0               SKIP OF LEADING SPACES ALL FOUND             
         BNZ   DELSPC10                                                         
*                                                                               
         CLI   0(RE),C' '          IF LEADING SPACE                             
         BNH   DELSPCCN               BAPASS MOVING LEFT                        
*                                                                               
DELSPC10 DS    0H                                                               
*                                                                               
         MVC   0(1,RF),0(RE)       MOVE DATA 1 PLACE TO THE LEFT                
         LA    RF,1(RF)            BUMP RECEIVING AREA POINTER                  
         LA    R0,1                INDICATE ALL LEADING SPACES FOUND            
         BCTR  R4,0                DECREMENT RECEIVING FIELD CTR                
*                                                                               
DELSPCCN DS    0H                                                               
*                                                                               
         LA    RE,1(RE)            BUMP AREA POINTER                            
         BCT   R3,DELSPCLP                                                      
*                                                                               
DELSPCDN DS    0H                                                               
*                                                                               
         LTR   R4,R4               DONE IF RECEIVING FIELD EXHAUSTED            
         BZ    DELSPCX                                                          
*                                                                               
         MVI   0(RF),C' '          FILL REMAINDER OF FIELD WITH SPACES          
         LA    RF,1(RF)                                                         
         BCT   R4,*-8                                                           
*                                                                               
DELSPCX  DS    0H                                                               
         XIT1                                                                   
*                                                                               
         TITLE 'PRSFM14 - MERGE EDR PUB FILE - DCBS'                            
***********************************************************************         
*                                                                     *         
*        DCB FOR EDR FILE AND OUTPUT TAPE                             *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
EDRFILE  DCB   DDNAME=EDRFILE,DSORG=PS,MACRF=(GM),EODAD=PREDDONE,      X        
               RECFM=FB,BUFNO=2                                                 
*                                                                               
TOUT     DCB   DDNAME=TOUT,DSORG=PS,MACRF=(PM),                        X        
               RECFM=VB,BUFNO=2,BLKSIZE=8200,LRECL=2048                         
*                                                                               
EDRSORT  DCB   DDNAME=EDRSORT,DSORG=PS,MACRF=(GM,PM),EODAD=PRSRDONE,   X        
               RECFM=FB,BUFNO=2,BLKSIZE=1290,LRECL=129                          
*                                                                               
         TITLE 'PRWRI13 - REPORT SPECS - EDRSPECS'                              
***********************************************************************         
*                                                                     *         
*        REPORT SPECS                                                 *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
         DS    0D                                                               
*                                                                               
EDRSPECS DS    0H                                                               
*                                                                               
         SSPEC H1,1,RUN                                                         
         SSPEC H1,100,REQUESTOR                                                 
         SSPEC H2,100,REPORT                                                    
         SSPEC H2,113,PAGE                                                      
*                                                                               
         SSPEC H2,53,C'EDR/CTFILE MERGE'                                        
         SSPEC H3,53,C'----------------'                                        
*                                                                               
         SSPEC H5,1,C'EDR PUB NAME'                                             
         SSPEC H5,32,C'EDR EDITION'                                             
         SSPEC H5,63,C'EDR RATE CARD'                                           
         SSPEC H5,79,C'MEDIA'                                                   
         SSPEC H5,85,C'PUB SEQ'                                                 
         SSPEC H5,93,C'EDN SEQ'                                                 
         SSPEC H5,102,C' PAKID'                                                 
*                                                                               
         SSPEC H6,1,C'------------'                                             
         SSPEC H6,32,C'-----------'                                             
         SSPEC H6,63,C'-------------'                                           
         SSPEC H6,79,C'-----'                                                   
         SSPEC H6,85,C'-------'                                                 
         SSPEC H6,93,C'-------'                                                 
         SSPEC H6,102,C' -----'                                                 
*                                                                               
         DC    X'00'                                                            
*                                                                               
         DS    0D                                                               
*                                                                               
RECSPECS DS    0H                  SPECS FOR RECORD COUNTS SPECS                
*                                                                               
         SSPEC H1,1,RUN                                                         
         SSPEC H1,100,REQUESTOR                                                 
         SSPEC H2,100,REPORT                                                    
         SSPEC H2,113,PAGE                                                      
         SPACE 1                                                                
         SSPEC H2,53,C'EDR/CTFILE MERGE'                                        
         SSPEC H3,53,C'----------------'                                        
*                                                                               
         DC    X'00'                                                            
         EJECT                                                                  
DASHS    DC    64C'-'              DASHES                                       
         DS    0F                                                               
         LTORG                                                                  
         TITLE 'PRWRI13 - SWITCH BACK TO USER SYSTEM - SWTBACK'                 
***********************************************************************         
*                                                                     *         
*        SWITCH BACK TO USER SYSTEM                                   *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
         DS    0D                                                               
SWTBACK  NMOD1 0,**#SBK                                                         
*                                                                               
         L     RC,WRKWORKA         RESTORE WORKAREA ADDRESS                     
*                                                                               
         MVC   USEIO,SVUSEIO       SET USER I/O OPTION                          
         MVC   SYSDIR,SVSYSDIR     SET FILE NAMES                               
         MVC   SYSFIL,SVSYSFIL                                                  
         MVC   DATADISP,SVDATADI   SET DISPLACEMENT OF DATA IN RECORD           
         MVC   LKEY,SVLKEY         SET KEY LENGTH                               
         XC    DMCB(8),DMCB                                                     
         MVC   DMCB+1(3),=X'FFFFFF'                                             
         MVC   DMCB(1),SVSYS       SYSTEM ID                                    
         MVC   FILENAME,SVFILENM                                                
*                                                                               
         CLI   OFFLINE,C'Y'        IF OFF-LINE                                  
         BNE   SWTBACK1                                                         
*                                                                               
         L     RF,WRKUTLA             POINT TO UTL                              
         MVC   4(1,RF),WRKSYS         RESTORE CURENT SYSTEM ID                  
         B     SWTBACKX                                                         
*                                                                               
SWTBACK1 DS    0H                                                               
*                                                                               
         GOTO1 SWITCH,DMCB         SWITCH BACK TO USER SYSTEM                   
         CLI   DMCB+4,0            MUST WORK                                    
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
SWTBACKX DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         TITLE 'PRWRI13 - SWITCH TO CONTROL SYSTEM - SWTCNTL'                   
***********************************************************************         
*                                                                     *         
*        SWITCH TO CONTROL SYSTEM                                     *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
         DS    0D                                                               
SWTCNTL  NMOD1 0,**#SCT                                                         
*                                                                               
         L     RC,WRKWORKA         RESTORE WORKAREA ADDRESS                     
*                                                                               
         MVI   USEIO,C'Y'          USER DOES I/O                                
         MVC   SYSDIR,=CL8'CTFILE' SET FILE NAMES                               
         MVC   SYSFIL,=CL8'CTFILE'                                              
         MVC   DATADISP,=H'28'     SET DISPLACEMENT OF DATA IN RECORD           
         MVC   LKEY,=H'25'         SET KEY LENGTH                               
*                                                                               
         CLI   OFFLINE,C'Y'        IF OFF-LINE                                  
         BNE   SWTCNTL1                                                         
*                                                                               
         L     RF,WRKUTLA             POINT TO UTL                              
         MVI   4(RF),X'0A'            SET TO CONTROL SYSTEM                     
         B     SWTCNTLX                                                         
*                                                                               
SWTCNTL1 DS    0H                                                               
*                                                                               
         GOTO1 SWITCH,DMCB,X'0AFFFFFF',0  SWITCH TO CONTROL SYSTEM              
         CLI   DMCB+4,0            MUST WORK                                    
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
SWTCNTLX DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         EJECT                                                                  
         TITLE 'PRSFM14 - REPORT DISPLAY LINE - RPLINE'                         
***********************************************************************         
*                                                                     *         
*        REPORT LINE LAYOUT                                           *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
RPLINED  DSECT                     REPORT LINE                                  
RPLINE   DS    0CL132              REPORT LINE                                  
RPEDRNAM DS    CL30                EDR PUB NAME                                 
         DS    CL1                 SPACING                                      
RPEDREDN DS    CL30                EDR EDITION                                  
         DS    CL1                 SPACING                                      
RPEDRCRD DS    CL15                EDR RATE CARD                                
         DS    CL1                 SPACING                                      
         DS    CL2                 SPACING                                      
RPEDRMED DS    CL1                 EDR MEDIA CODE                               
         DS    CL2                 SPACING                                      
RPPSQN   DS    CL6                 PUB     SEQUENCE NUMBER                      
         DS    CL2                 SPACING                                      
RPESQN   DS    CL6                 EDITION SEQUENCE NUMBER                      
         DS    CL4                 SPACING                                      
RPEPID   DS    CL8                 PAKID                                        
         DS    CL(132-(*-RPLINE))  SPARE                                        
*                                                                               
         ORG   RPLINE                                                           
*                                                                               
*        REPORT LINE FOR EDR RECORD STATISTICS                                  
*                                                                               
RPLINE1  DS    0CL132              REPORT LINE                                  
RPTITLE  DS    CL35                TITLE                                        
         DS    CL1                 SPACING                                      
RPCOUNT  DS    CL16                NUMBER OF RECORDS                            
         DS    CL1                 SPACING                                      
         DS    CL(132-(*-RPLINE1)) SPARE                                        
*                                                                               
         EJECT                                                                  
       ++INCLUDE PRSFMFFD                                                       
         PRINT OFF                                                              
       ++INCLUDE DDGENTWA                                                       
         PRINT ON                                                               
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE PRSFMD4D                                                       
         EJECT                                                                  
       ++INCLUDE PRSFMWORKD                                                     
         SPACE 1                                                                
         ORG   SYSSPARE                                                         
*                                                                               
WRKRELO  DS    A                   THIS MODULE'S RELO FACTOR                    
WRKWORKA DS    A                   COMMON WORKAREA ADDRESS                      
WRKUTLA  DS    A                   A(UTL)                                       
WRKSYS   DS    XL1                 CURRENT SYSTEM SE NUMBER                     
*                                    DON'T PAGE                                 
*                                                                               
WCEPIN   DS    PL8                 NUMBER OF CEPRECS  READ IN                   
WEDRIN   DS    PL8                 NUMBER OF EDR RECS READ IN                   
WCEPOUT  DS    PL8                 NUMBER OF CEPRECS  WRITTEN                   
WEDRADD  DS    PL8                 NUMBER OF CEPRECS  ADDED                     
WRKDUPS  DS    PL8                 NUMBER OF DUPLICATES ON FILE                 
WEDRDUPS DS    PL8                 NUMBER OF DUPLICATES ON EDR FILE             
WCEPOLD  DS    PL8                 NUMBER OF UNMATCHED CTFILE RECS              
*                                                                               
WRKPSQN  DS    XL2                 CURRENT PUB     SEQUENCE NUMBER              
WRKESQN  DS    XL2                 CURRENT EDITION SEQUENCE NUMBER              
WRKCEPSW DS    CL1                 'Y'-CEPRECS DONE                             
CEPRECSV DS    XL(CEPDATA-CEPREC+CEDRELML)  CEPREC SAVE                         
         DS    XL1                 EDR RECORD INDICATOR                         
EDRREC   DS    XL256               EDRREC INPUT AREA                            
*                                                                               
         TITLE 'PRSFM14 - MERGE EDR PUB FILE - EDRRECD'                         
***********************************************************************         
*                                                                     *         
*        DSECT FOR EDR FILE RECORD                                    *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
EDRRECD  DSECT                     EDR FILE RECORD                              
EDRPUB   DS    CL30                PUB NAME                                     
EDREDN   DS    CL30                EDITION                                      
EDRCRD   DS    CL15                RATE CARD                                    
EDRPAKID DS    CL8                 PAKID                                        
EDRPXTRA DS    CL5                 PAKID EXTRA - SHOULD BE 5C'*'                
         DS    CL2                 SPARE                                        
EDRMED   DS    CL1                 EDR MEDIA                                    
EDRRECL  EQU   *-EDRRECD           LENGTH OF EDR FILE RECORD                    
*                                                                               
         EJECT                                                                  
       ++INCLUDE CTEDRREC                                                       
         EJECT                                                                  
*       ++INCLUDE PRGENFILE                                                     
*       ++INCLUDE PRGLOBEQUS                                                    
*       ++INCLUDE DDSPLWORKD                                                    
*       ++INCLUDE DDSPOOLD                                                      
*       ++INCLUDE DDMASTD                                                       
         PRINT OFF                                                              
       ++INCLUDE PRGENFILE                                                      
       ++INCLUDE PRGLOBEQUS                                                     
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDMASTD                                                        
         EJECT                                                                  
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'099PRSFM14   08/17/94'                                      
         END                                                                    
