*          DATA SET ACREPSM02  AT LEVEL 031 AS OF 06/04/20                      
*PHASE ACSM02C                                                                  
*INCLUDE GETLOGO                                                                
*INCLUDE PRNTBL                                                                 
*INCLUDE DECODE                                                                 
***********************************************************************         
*  ID  LVL   DATE    TICKET            COMMENTS                       *         
* ---- --- ------- ------------ --------------------------------------*         
* VGUP 032 05JUN20 <ITMF-46821> Fixed the type filter issue           *         
***********************************************************************         
ACSM02   TITLE '- SORT MERGE AND WORKER FILE MAINTENANCE'                       
ACSM02   CSECT                                                                  
         PRINT NOGEN                                                            
         USING ACWORKD,R7          R7=A(GLOBAL W/S)                             
         USING LOGOD,R6            R6=A(LOGOC)                                  
         USING WORKD,RC            RC=A(LOCAL W/S)                              
         NMOD1 0,**ACSM**,RA,R9                                                 
         L     R7,0(,R1)           ACWORKD                                      
         L     R6,LOGOC                                                         
         LA    RC,SPACEND                                                       
*                                                                               
         CLI   MODE,RUNFRST                                                     
         BE    INIT01                                                           
         CLI   MODE,RUNLAST                                                     
         BE    INIT11                                                           
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* RUNFRST - INITIALISE VALUES                                         *         
***********************************************************************         
INIT01   MVI   RUNINDS,0                                                        
         MVI   FLTKEY,0                                                         
         MVI   SMF,NO              Set to No                                    
         CLI   RCFFPARM+5,C'N'     TEST SUPPRESS COMPANY ANALYSIS               
         BNE   *+8                                                              
         OI    RUNINDS,RUNISCOA    YES - SET RUN INDICATOR                      
         OI    RCFLAG1,RCFREPLC    SET LOWER CASE REQUIRED                      
         L     RF,ADBXAREA                                                      
         OI    BOXDDCTL-BOXD(RF),BOXDDUL+BOXDDLC                                
         L     RF,ADMASTC                                                       
         CLI   MCTSTRUN-MASTD(RF),X'FF'                                         
         BNE   *+8                                                              
         OI    RUNINDS,RUNITEST    SET TEST=YES                                 
         TM    MCPRTIND-MASTD(RF),MCPRTINL                                      
         BZ    *+8                                                              
         OI    RUNINDS,RUNINOLO    SET NOLOGOS                                  
         CLI   RCWRITE,YES                                                      
         BE    *+8                                                              
         OI    RUNINDS,RUNIWRNO    SET WRITE=NO                                 
*&&US                                                                           
         L     RF,VEXTRAS                                                       
         USING RUNXTRAD,RF                                                      
         MVC   ONEFILE,WKFILEID    SET SINGLE FILE                              
         LA    RE,SMFAREA                                                       
         ST    RE,ASMFAREA                                                      
         MVI   IS_ACSM,YES                                                      
         ICM   RE,15,ASMFBLK       External copy of SMFAREA                     
         BZ    INIT02              Not set so put to SMF as we process          
         MVI   IS_ACSM,NO          From Postwrk or some place else              
         ST    RE,ASMFAREA         Set to external area                         
         DROP  RF                                                               
*&&                                                                             
*                                                                               
         USING DICTATED,R1                                                      
INIT02   LA    R1,DMCB             GET REPORT LITERALS                          
         XC    DMCB(24),DMCB                                                    
         MVI   DDACTN,DDACTNL                                                   
         MVI   DDRETN,DDCASEL                                                   
         MVI   DDSYS,6                                                          
         MVC   DDLANG,RCLANG                                                    
         LA    R0,DICI                                                          
         STCM  R0,7,DDIADR                                                      
         LA    R0,DICO                                                          
         STCM  R0,7,DDOADR                                                      
         GOTO1 ADDICTAT                                                         
         DROP  R1                                                               
         EJECT                                                                  
***********************************************************************         
* READ AND VALIDATE INPUT CARDS                                       *         
***********************************************************************         
         CLC   =C'CARDS',RCFFPARM  TEST SPECIAL PARM VALUE FOR CARDS            
         BNE   XIT                                                              
         XC    RCFFPARM,RCFFPARM                                                
*                                                                               
INIT03   GOTO1 CARDS,DMCB,IO,=C'RE00'                                           
         CLI   IO,C'*'             Skip                                         
         BE    INIT03              Next card                                    
         CLC   IO(2),=C'/*'                                                     
         BE    XIT                                                              
*                                                                               
         USING OPTTABD,R2                                                       
INIT05   LA    R2,OPTTAB           OPTIONAL INPUT CARDS                         
         SR    RF,RF                                                            
*                                                                               
INIT07   CLI   OPTTABD,OPTTEOTQ    TEST END OF OPTION TABLE                     
         BNE   *+6                                                              
         DC    H'0'                INVALID INPUT OPTION                         
         IC    RF,OPTTKLEN         LENGTH FOR COMPARE                           
         EX    RF,*+8                                                           
         BE    INIT09                                                           
         CLC   IO(0),OPTTKWRD      MATCH CARD FIELD TO TABLE                    
         LA    R2,OPTTABL(R2)                                                   
         B     INIT07                                                           
*                                                                               
INIT09   LA    R1,IO+1(RF)         R1=A(FIRST DATA BYTE)                        
         ICM   RF,3,OPTTVDSP                                                    
         LA    RF,ACSM02(RF)       RF=A(VALIDATION ROUTINE)                     
         BASR  RE,RF               VALIDATE INPUT OPTION                        
         B     INIT03              GET NEXT CARD                                
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* RUNLAST                                                             *         
***********************************************************************         
INIT11   L     RF,ADMASTC                                                       
         MVC   SENO,MCIDSENO-MASTD(RF)                                          
         GOTO1 DATCON,DMCB,(4,RCDATE),(0,TODAY0)                                
         GOTO1 (RF),(R1),,(1,TODAY1)                                            
         MVC   OUTPDAY,TODAY1DD                                                 
*                                                                               
         MVC   WORK(6),TODAY0      BUILD TABLE OF 3-6 DAYS OLD                  
         LA    R0,L'ODDDAYS                                                     
         L     R2,=F'-3'                                                        
         LA    R3,ODDDAYS                                                       
INIT14   GOTO1 ADDAY,DMCB,WORK,WORK+6,(R2)                                      
         PACK  DUB(2),WORK+10(3)                                                
         MVC   0(1,R3),DUB                                                      
         LA    R3,1(R3)                                                         
         BCTR  R2,0                                                             
         BCT   R0,INIT14                                                        
*                                                                               
         LA    R2,IO                                                            
         USING CTWREC,R2           READ SYSTEM LIST RECORD (FOR FILE)           
         XC    CTWKEY,CTWKEY                                                    
         MVI   CTWKTYP,CTWKTYPQ                                                 
         MVI   CTWKREC,C'S'                                                     
         MVI   CTWKSYSN,CTWKACC                                                 
         GOTO1 DATAMGR,DMCB,DMREAD,CTFILE,CTWREC,CTWREC                         
         BNE   INIT18                                                           
*                                                                               
         LA    R1,CTWDATA                                                       
         SR    R0,R0                                                            
         USING SYSELD,R1           LOCATE SYSTEM ELEMENT FOR SENO               
INIT16   CLI   SYSEL,0             TEST EOR                                     
         BNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLI   SYSEL,SYSELQ        TEST SYSTEM ELEMENT                          
         BNE   INIT16G                                                          
         CLC   SYSSEN,SENO         TEST FOR CORRECT SE NUMBER                   
         BE    INIT17                                                           
INIT16G  IC    R0,SYSLEN           BUMP TO NEXT ELEMENT                         
         AR    R1,R0                                                            
         B     INIT16                                                           
*                                                                               
INIT17   MVC   OUTFILN,SYSNAME+3   Set system id number 1st char                
         MVC   OUTXTRA,SYSNAME+4   Set system id number 2nd char                
         DROP  R1,R2                                                            
*                                                                               
INIT18   XC    WKID,WKID           CLEAR WORKER-ID                              
         XC    OUTCNT,OUTCNT       CLEAR OUTPUT ELEMENT COUNT                   
*                                                                               
         MVI   RCSUBPRG,0                                                       
         CLC   RCPROG,PROGSORT     TEST SORT/MERGE                              
         BE    SORTI                                                            
*                                                                               
         MVI   RCSUBPRG,2                                                       
         MVC   OUTPROG,OUTPKEEP                                                 
         CLC   RCPROG,PROGKEEP     TEST KEEP WORKER FILES                       
         BE    KEEP                                                             
*                                                                               
         MVI   RCSUBPRG,3                                                       
         MVC   OUTPROG,OUTPKEEP                                                 
         CLC   RCPROG,PROGACTV     TEST UNKEEP WORKER FILES                     
         BE    ACTV                                                             
*                                                                               
         MVI   RCSUBPRG,4                                                       
         MVC   OUTPROG,OUTPHOLD                                                 
         CLC   RCPROG,PROGHOLD     TEST HOLD WORKER FILES                       
         BE    HOLD                                                             
*                                                                               
         MVI   OUTFILN,0           RESET FILE NUMBER                            
         MVC   OUTUSER,ORIGINUM    SET DDS USER ID NUMBER FOR FILE              
         MVC   OUTPROG,OUTPUPDT                                                 
         MVI   RCSUBPRG,255                                                     
         CLC   RCPROG,PROGODDS     TEST CREATE ODDS                             
         BE    ODDS                                                             
         DC    H'0'                                                             
         EJECT                                                                  
***********************************************************************         
* READ ALL WKFILE INDEX RECORDS AND FILTER OUT WORKER FILES THAT ARE  *         
* REQUIRED FOR THIS ACCOUNT SYSTEM. WORKER FILES THAT APPLY ARE READ  *         
* AND (IF VALID) ARE PASSED TO SORTER FOR SORTING INTO ASCENDING KEY  *         
* SEQUENCE. BUFFALO RECORDS ARE GENERATED FOR EACH WORKER FILE WHICH  *         
* GET PRINTED AFTER ALL FILES HAVE BEEN PROCESSED.                    *         
***********************************************************************         
SORTI    GOTO1 CLRTOT                                                           
         MVI   LOGOEND,C'X'                                                     
         MVC   LOGO1,CONLOGO                                                    
         MVC   LOGONAME,CONNAME                                                 
         MVC   LOGOADD,CONADDR                                                  
         MVC   LOGOADD2,SPACES                                                  
         MVC   LOGOADD3,SPACES                                                  
         GOTO1 LOGO,DMCB,LOGOD                                                  
         GOTO1 BUFFALO,PARA,BUFSET,ABUFF                                        
*                                                                               
         L     RF,ADBXAREA                                                      
         USING BOXD,RF             INITIALISE BOX                               
         MVI   BOXCOLS,C' '                                                     
         MVC   BOXCOLS+1(L'BOXCOLS-1),BOXCOLS                                   
         MVI   BOXROWS,C' '                                                     
         MVC   BOXROWS+1(L'BOXROWS-1),BOXROWS                                   
         MVI   BOXROWS+07,C'T'                                                  
         MVI   BOXROWS+10,C'M'                                                  
         MVI   BOXROWS+99,C'B'                                                  
         MVI   BOXCOLS+(LINBXL-LINED),C'L'                                      
         MVI   BOXCOLS+(LINBX1-LINED),C'C'                                      
         MVI   BOXCOLS+(LINBX2-LINED),C'C'                                      
         MVI   BOXCOLS+(LINBXR-LINED),C'R'                                      
         MVI   BOXINIT,0                                                        
         MVI   BOXYORN,YES                                                      
         DROP  RF                                                               
*                                                                               
SORTI02  GOTO1 WORKER,DMCB,WKINDX,AIBUFF,WKID,0                                 
         TM    8(R1),X'80'         TEST END OF INDEX                            
         BNZ   SORTIX                                                           
         ZAP   IDRECS,PZERO        CLEAR WORK FILE RECORD COUNT                 
         CLI   WKTYPE,WKTPOST      POSTING-TYPE ONLY                            
         BNE   SORTI02                                                          
         TM    WKSTAT,WKSKEEP      DON'T WANT KEEP-STATUS IDS                   
         BNZ   SORTI02                                                          
         TM    WKSTAT,WKSHOLD      DON'T WANT HOLD-STATUS IDS                   
         BNZ   SORTI02                                                          
*&&US                                                                           
         OC    ONEFILE,ONEFILE     TEST SINGLE FILE INPUT                       
         BZ    SORTI03                                                          
         CLC   WKKEY,ONEFILE                                                    
         BNE   SORTI02                                                          
         CLC   WKSEQN,ONEFILE+(WKSEQN-WKID)                                     
         BNE   SORTI02                                                          
         B     SORTI07                                                          
*&&                                                                             
SORTI03  CLC   RCFFPARM(L'WKSYSPRG),SPACES                                      
         BNH   SORTIO4                                                          
         CLC   WKSYSPRG,RCFFPARM   APPLY SINGLE WORK FILE FILTER                
         BNE   SORTI02                                                          
         B     SORTI07                                                          
*                                                                               
SORTIO4  CLI   FLTTYP,0            TEST ANY FILE TYPE FILTERS                   
         BE    SORTI07                                                          
         LA    RF,FLTTYP           MATCH FILE TO FILTER TABLE                   
         LA    R0,FLTTYPN                                                       
*                                                                               
SORTI05  CLI   0(RF),0             TEST END OF TABLE                            
         BE    SORTI02             NOT FOUND, SKIP WORKER FILE                  
         CLC   WKSYSPRG,0(RF)      TAKE ONLY FILES IN TABLE                     
         BE    SORTI07                                                          
         LA    RF,L'FLTTYP(RF)                                                  
         BCT   R0,SORTI05                                                       
         B     SORTI02                                                          
*                                                                               
SORTI07  GOTO1 GETUID,WKUSER       GET USER-ID VALUES                           
         BNE   SORTI02                                                          
         CLI   WKTYPE,WKTODDS      TEST ODDS FILE                               
         BE    SORTI02                                                          
         GOTO1 GETPRG,WKSYSPRG     GET PROGRAM VALUES                           
         BNE   SORTI02                                                          
         USING PRGTABD,R5                                                       
         TM    PRGTIND2,PRGTINOT   TEST DON'T PROCESS FILES FOR TODAY           
         BZ    *+14                                                             
         CLC   WKDAY,TODAY1DD      MATCH FILE DAY TO DAY TODAY                  
         BE    SORTI02                                                          
         TM    PRGTIND2,PRGTICPY   TEST COMPANY TAKEN FROM WKCPY                
         BZ    SORTI09                                                          
         L     R1,AUIDNTRY         YES - EXTRACT FROM ID TABLE ENTRY            
         MVC   THISCPY,UIDTCPY-UIDTABD(R1)                                      
*                                                                               
SORTI09  XC    WKIO(256),WKIO                                                   
         GOTO1 WORKER,DMCB,WKREAD,AIBUFF,WKID,WKIOLN                            
         CLI   8(R1),0                                                          
         BNE   SORTI02                                                          
*                                                                               
         CLC   WKIOLN,=H'104'                                                   
         BH    *+10                                                             
         MVC   WKIOLN,=H'104'                                                   
         LH    R1,WKIOLN                                                        
         LA    R1,WKIOLN(R1)                                                    
         MVI   0(R1),0             SET EOR                                      
*                                                                               
         LH    R1,WKIOLN                                                        
         LA    R1,SORTKEYL(R1)                                                  
         STH   R1,SORTRLEN         SET SORTER RECORD LENGTH                     
         XC    SORTKEY,SORTKEY                                                  
         LA    R3,WKIO                                                          
*                                                                               
         USING FFTELD,R3                                                        
         CLI   FFTEL,FFTELQ       TEST FREE FORM TEXT ELEMENT                   
         BNE   SORTI10                                                          
         CLI   FFTTYPE,FFTTWFSA   TEST WORKER FILE START ACCOUNT                
         BE    SORTI09            YES, SKIP RECORD                              
*                                                                               
         USING PSLTOTD,R3                                                       
SORTI10  CLI   PSLTEL,PSLTELQ      TEST LEDGER TOTAL ELEMENT                    
         BNE   SORTI11                                                          
         MVC   SORTKEY(L'PSLTCUL),PSLTCUL                                       
         MVC   THISCPY,PSLTCPY                                                  
         ZAP   TOTDR,PSLTDR                                                     
         ZAP   TOTCR,PSLTCR                                                     
         B     SORTI50                                                          
*                                                                               
         USING PSTKEYD,R3                                                       
SORTI11  CLI   PSTKEL,MPDKTYPQ     TEST SPECIAL BILLING TRANSFER                
         BNE   *+14                                                             
         MVC   SORTKEY,PSTKEL                                                   
         B     SORTI64                                                          
*                                                                               
         CLI   PSTKEL,PSTKELQ      TEST TRANSACTION KEY ELEMENT                 
         BNE   SORTI12                                                          
         MVC   SORTKEY,PSTKEY      SET SORT KEY                                 
         GOTO1 DATAMGR,DMCB,DMREAD,ACCDIR,PSTKEY,IOKEY                          
         BNE   SORTI09                                                          
         LA    RF,IOKEY                                                         
         USING TRNRECD,RF                                                       
         MVC   THISCPY,TRNKCPY                                                  
         LA    R0,ACCMST                                                        
         TM    TRNKSTAT,TRNSARCH                                                
         BZ    *+8                                                              
         LA    R0,ACCARC                                                        
         GOTO1 DATAMGR,DMCB,DMGETREC,(R0),TRNKDA,IO,IOWORK                      
         BE    *+6                                                              
         DC    H'0'                                                             
         DROP  RF                                                               
         LA    R3,IO+(TRNRFST-TRNRECD)                                          
         USING TRNELD,R3                                                        
         CLI   TRNEL,TRNELQ                                                     
         BE    SORTI40                                                          
         DC    H'0'                                                             
*                                                                               
         USING PSSUBFD,R3                                                       
SORTI12  CLI   PSSBEL,PSHDELQ      TEST POSTING ELEMENT                         
         BE    SORTI30                                                          
         CLI   PSSBEL,PSSBELQ      TEST SUB-FILE ELEMENT                        
         BNE   WKERRS                                                           
         TM    PRGTIND1,PRGTITRL   TEST TRAILER CONTAINS DR/CR                  
         BZ    SORTI22                                                          
         ZAP   TOTDR,PSSBRECS                                                   
         ZAP   TOTCR,PSSBCASH                                                   
         TM    PRGTIND1,PRGTIPOF   TEST PEEL-OFF PROGRAM                        
         BZ    *+10                                                             
         MP    TOTDR,=P'-1'        REVERSE SIGN OF DEBIT AMOUNT                 
         B     SORTI50                                                          
*                                                                               
SORTI22  TM    PRGTIND1,PRGTINOC                                                
         BNZ   SORTI26                                                          
         CP    IDRECS,PSSBRECS     TEST RECORD COUNT                            
         BNE   *+14                                                             
         CP    IDCASH,PSSBCASH     TEST CASH TOTAL                              
         BE    SORTI26                                                          
*                                                                               
         MVC   P+1(L'NOTBAL1),NOTBAL1                                           
         GOTO1 DISWRK,P+1+L'NOTBAL1                                             
         MVC   0(L'NOTBAL2,R1),NOTBAL2                                          
         GOTO1 PRINTIT                                                          
*                                                                               
         MVC   P+1(6),=C'Actual'                                                
         CURED IDCASH,(15,P+14),2,MINUS=YES                                     
         CURED IDRECS,(6,P+30),0                                                
         GOTO1 PRINTIT                                                          
*                                                                               
         MVC   P+1(12),=C'File trailer'                                         
         CURED PSSBCASH,(11,P+18),2,MINUS=YES                                   
         CURED PSSBRECS,(6,P+30),0                                              
         MVI   SPACING,2                                                        
         GOTO1 PRINTIT                                                          
*                                                                               
SORTI26  ZAP   IDCASH,PZERO                                                     
         ZAP   IDRECS,PZERO                                                     
         B     SORTI09                                                          
*                                                                               
         USING PSHEADD,R3                                                       
SORTI30  OC    PSHDANAL,SPACES                                                  
         TM    PRGTIND2,PRGTICPY   TEST COMPANY TAKEN FROM WKCPY                
         BNZ   *+10                                                             
         MVC   THISCPY,PSHDACPY    FIRST DIG OUT COMPANY AND AMOUNT             
         MVC   WORK(L'FILMULUK),PSHDAUNT                                        
         LA    RE,PSHEADD                                                       
         SR    R0,R0                                                            
         IC    R0,PSHDLEN                                                       
         AR    RE,R0               R1=A(NEXT ELEMENT)                           
         USING TRNELD,RE                                                        
         CLI   TRNEL,TRNELQ        TEST IF A TRANSACTION ELEMENT                
         BNE   SORTI09                                                          
*                                                                               
         LA    RF,SORTKEY                                                       
         USING TRNRECD,RF          BUILD TRANSACTION SORT KEY                   
         MVC   TRNKCULA,PSHDACC                                                 
         MVC   TRNKOFF,PSHDANAL                                                 
         MVC   TRNKCULC,PSHDSBAC                                                
         MVC   TRNKDATE,TRNDATE                                                 
         MVC   TRNKREF,TRNREF                                                   
         MVC   TRNKSBR,TRNSUB                                                   
         DROP  RE,RF                                                            
*                                                                               
SORTI32  SR    R0,R0               CHECK FOR ZERO LENGTH ELEMENTS               
         ICM   R0,1,PSHDLEN                                                     
         BNZ   *+8                                                              
         MVI   PSHDEL,0                                                         
         AR    R3,R0                                                            
         CLI   PSHDEL,0                                                         
         BE    SORTI62                                                          
         CLI   0(R3),TRNELQ                                                     
         BNE   SORTI32                                                          
         USING TRNELD,R3                                                        
*                                                                               
         CLI   RCCTRY,CTRYGBR      TEST UK                                      
         BNE   *+14                                                             
         CLC   FILMULUK,WORK       FILM LEDGER HAS STRANGE MOS VALUES           
         BE    SORTI40                                                          
*                                                                               
         CLI   TRNMOS,C'0'         OR ANY VALID MOS                             
         BL    SORTI36                                                          
         CLI   TRNMOS+1,C'A'                                                    
         BE    SORTI40                                                          
         CLI   TRNMOS+1,C'B'                                                    
         BE    SORTI40                                                          
         CLI   TRNMOS+1,C'C'                                                    
         BE    SORTI40                                                          
         CLI   TRNMOS+1,C'0'                                                    
         BNL   SORTI40                                                          
SORTI36  MVC   TRNMOS,THISMNTH                                                  
*                                                                               
SORTI40  ZAP   TOTDR,PZERO         SET AMOUNT                                   
         ZAP   TOTCR,PZERO                                                      
         LA    R1,TOTDR                                                         
         TM    TRNSTAT,TRNSDR                                                   
         BNZ   *+8                                                              
         LA    R1,TOTCR                                                         
         ZAP   0(L'TOTCR,R1),TRNAMNT                                            
*                                                                               
SORTI50  XC    BUFKEY(BUFKEYL),BUFKEY                                           
         MVI   BUFRTYPE,BUFRTDDS   BUILD DDS SUMMARY RECORD                     
         MVC   BUFRSYSP,WKSYSPRG                                                
         MVC   BUFRKSEQ,WKSYSPRG   GIVES DDS REPORT IN TYPE ORDER               
         TM    PRGTIND1,PRGTIJRN   TEST DAILY JOURNAL                           
         BNZ   SORTI54                                                          
         TM    PRGTIND1,PRGTICLR   TEST SPOT/PRINT CLEARANCES                   
         BO    SORTI52                                                          
         MVC   BUFRUSER,WKUSER                                                  
         MVC   BUFRCPY,THISCPY                                                  
         TM    PRGTIND1,PRGTICHQ   TEST CHEQUE RUNS                             
         BZ    SORTI54                                                          
SORTI52  MVC   BUFRSPRG,WKSUB      SPOT/PRINT SYSTEM NUMBER                     
         MVC   BUFRXTRA,WKEXTRA                                                 
         OI    BUFRXTRA,C' '                                                    
         TM    PRGTIND1,PRGTICLR   TEST SPOT/PRINT CLEARANCES                   
         BZ    SORTI54                                                          
         BRAS  RE,REVSE#           for now reverse letter to SE#                
         MVC   OTHERSE#,SE#                                                     
*                                                                               
SORTI54  ZAP   BUFRDR,TOTDR                                                     
         ZAP   BUFRCR,TOTCR                                                     
         ZAP   BUFRPONE,=P'1'      FORCE BUFFALO NOT TO DROP RECORD             
         MVC   BUFRDAY,WKDAY                                                    
         GOTO1 GETUID,WKUSER                                                    
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         L     R1,AUIDNTRY                                                      
         MVC   BUFRCODE,UIDTCODE-UIDTABD(R1)                                    
         GOTO1 BUFFALO,PARA,BUFPUT,ABUFF,BUFREC                                 
         MVI   BUFFSW,1            SET BUFFALO INITIALISED                      
*                                                                               
         MVI   BUFRTYPE,BUFRTCPY   BUILD COMPANY RECORD                         
         XC    BUFRKSEQ,BUFRKSEQ                                                
         MVC   BUFRCPY,THISCPY                                                  
         MVC   BUFRUSER,WKUSER     USER-ID# & DAY FOR EACH RECORD               
         MVC   BUFRDAY,WKDAY                                                    
         TM    RUNINDS,RUNISCOA    TEST SUPPRESSING COMPANY ANALYSIS            
         BNZ   SORTI55                                                          
         GOTO1 (RF),(R1)                                                        
                                                                                
SORTI55  MVI   BUFRTYPE,BUFRTSMF   SMF type                                     
         MVC   BUFRSEQ#,WKSEQN                                                  
         MVC   BUFROSE#,OTHERSE#   Replace with SE# from SPT/NET/PRT            
         GOTO1 (RF),(R1)                                                        
*                                                                               
SORTI56  CLI   WKIO,PSLTELQ                                                     
         BE    SORTI64                                                          
         CLI   WKIO,PSTKELQ                                                     
         BE    SORTI64                                                          
         TM    PRGTIND1,PRGTITRL                                                
         BNZ   SORTI62                                                          
         AP    IDRECS,=P'1'                                                     
         AP    IDCASH,TOTDR                                                     
         B     SORTI32                                                          
*                                                                               
SORTI62  CLI   WKIO,PSHDELQ        TEST POSTING ELEMENT                         
         BNE   SORTI09                                                          
*                                                                               
SORTI64  TM    RUNINDS,RUNIWRNO    TEST WRITE=NO                                
         BNZ   SORTI09                                                          
         CLI   SORTSW,0            TEST SORT INITIALISED                        
         BNE   SORTI66                                                          
         MVI   SORTSW,1            SET SORT INITIALISED                         
         GOTO1 ADSORTER,DMCB,SORTCARD,SORTTYPE                                  
*                                                                               
SORTI66  GOTO1 ADSORTER,DMCB,SORTPUT,SORTRLEN                                   
         B     SORTI09                                                          
*                                                                               
SORTIX   B     SORTO                                                            
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* READ BACK THE BUFFALO RECORDS AND PRINT SUMMARY FOR EACH COMPANY    *         
* AND A DDS SUMMARY FOR ALL COMPANIES. GET RECORDS FROM SORT AND PUT  *         
* TO A SEQUENTIAL DISK FILE FOR THE UPDATE PROGRAM TO PROCESS.        *         
***********************************************************************         
SORTO    GOTO1 CLRTOT                                                           
         CLI   BUFFSW,0            TEST ANY BUFFALO RECORD PUT                  
         BE    SORTO12                                                          
         XC    BUFKEY(BUFKEYL),BUFKEY                                           
         MVI   BUFRTYPE,BUFRTDDS                                                
         GOTO1 BUFFALO,DMCB,BUFRDH,ABUFF,BUFKEY,1                               
         B     SORTO06                                                          
*                                                                               
SORTO04  GOTO1 BUFFALO,DMCB,BUFSEQ,(1,ABUFF),BUFKEY,1                           
*                                                                               
SORTO06  TM    8(R1),X'80'         TEST EOF                                     
         BNZ   SORTO12                                                          
         GOTO1 TOTED,BUFRDR                                                     
         GOTO1 GETPRG,BUFRSYSP                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         TM    PRGTIND1,PRGTIJRN   TEST DAILY JOURNAL                           
         BZ    *+14                                                             
         XC    BUFRCODE,BUFRCODE   CLEAR SPURIOUS UID                           
         B     SORTO08                                                          
                                                                                
         TM    PRGTIND1,PRGTICLR   TEST SPOT/PRINT CLEARANCES                   
         BNZ   SORTO08                                                          
         GOTO1 GETCPY,BUFRCPY                                                   
         BNE   SORTO04                                                          
*                                                                               
SORTO08  GOTO1 DISBUF                                                           
         GOTO1 ADDTOT                                                           
*                                                                               
SORTO10  MVI   SPACING,2                                                        
         GOTO1 PRINTIT                                                          
         B     SORTO04                                                          
*                                                                               
SORTO12  GOTO1 PRTTOT              PRINT DDS TOTALS AND END LOGO                
         CLI   BUFFSW,0            TEST ANY BUFFALO RECORDS PUT                 
         BE    SORTO30                                                          
         TM    RUNINDS,RUNISCOA    TEST SUPPRESS COMPANY ANALYSIS               
         BNZ   SORTO18                                                          
*                                                                               
         MVI   RCSUBPRG,1                                                       
         GOTO1 CLRTOT                                                           
         MVI   LASTCPY,0                                                        
         XC    BUFKEY(BUFKEYL),BUFKEY                                           
         MVI   BUFRTYPE,BUFRTCPY                                                
         GOTO1 BUFFALO,DMCB,BUFRDH,ABUFF,BUFKEY,1                               
         B     SORTO16                                                          
*                                                                               
SORTO14  GOTO1 BUFFALO,DMCB,BUFSEQ,(2,ABUFF),BUFKEY,1                           
*                                                                               
SORTO16  TM    8(R1),X'80'         TEST EOF                                     
         BZ    *+8                                                              
         MVI   BUFRCPY,X'FF'       SET COMPANY NOT EQUAL                        
         GOTO1 GETPRG,BUFRSYSP                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         CLC   LASTCPY,BUFRCPY     TEST CHANGE OF COMPANY                       
         BE    SORTO26                                                          
         CLI   LASTCPY,0           TEST FIRST TIME                              
         BE    SORTO22                                                          
         GOTO1 PRTTOT                                                           
*                                                                               
         TM    RUNINDS,RUNIREMO    TEST REMOTE COMPANY                          
         BZ    SORTO18                                                          
         TM    RUNINDS,RUNITEST    TEST TEST=YES                                
         BNZ   SORTO18                                                          
         GOTO1 PRINT,DMCB,SPACES,BC01                                           
         GOTO1 (RF),(R1),(L'PRTCLOSE,PRTCLOSE)                                  
         MVI   NEWPAGE,NO          SET DON'T SKIP TO CHANNEL 1 NEXT             
         B     SORTO22                                                          
*                                                                               
SORTO18  TM    RUNINDS,RUNINOLO    TEST NOLOGOS                                 
         BZ    SORTO20                                                          
         GOTO1 PRINT,DMCB,SPACES,BC01                                           
         MVI   NEWPAGE,NO          SET DON'T SKIP TO CHANNEL 1 NEXT             
         B     SORTO22                                                          
*                                                                               
SORTO20  MVI   LOGOTYPE,C'E'       PRINT END LOGOS                              
         GOTO1 LOGO,DMCB,LOGOD                                                  
*                                                                               
SORTO22  TM    RUNINDS,RUNISCOA    TEST SUPPRESS COMPANY ANALYSIS               
         BNZ   SORTO30                                                          
         L     R3,REMOTEC                                                       
         USING REMOTED,R3          R3=A(REMOTE CSECT)                           
         XC    REMOTKEY,REMOTKEY                                                
         CLI   BUFRCPY,X'FF'       TEST END OF BUFFALO FILE                     
         BE    SORTO30                                                          
         GOTO1 CLRTOT                                                           
         MVC   LASTCPY,BUFRCPY                                                  
         GOTO1 GETCPY,LASTCPY                                                   
         TM    RUNINDS,RUNIREMO    TEST REMOTE COMPANY                          
         BZ    SORTO24                                                          
         TM    RUNINDS,RUNITEST    TEST TEST=YES                                
         BNZ   SORTO24                                                          
         MVI   REMOTJID,WKSYSACC   BUILD REMOTE KEY                             
         MVC   REMOTJID+1(L'RCPROG),RCPROG                                      
         MVC   REMOTKEY(L'REPLIT),REPLIT                                        
         MVC   REMOTDST,COMPUSER                                                
         MVI   REMOTCLS,C'Q'                                                    
         B     SORTO26                                                          
*                                                                               
SORTO24  MVI   LOGOTYPE,C'S'                                                    
         MVC   LOGO1,COMPLOGO                                                   
         MVC   LOGONAME,COMPNAME                                                
         MVC   LOGOADD,COMPADD1                                                 
         MVC   LOGOADD2,COMPADD2                                                
         MVC   LOGOADD3,COMPADD3                                                
         GOTO1 VGETLOGO,DMCB,COMPUSER,LOGOD,DATAMGR                             
         MVC   LOGOJOB(4),DMCB                                                  
         CLI   LOGOJOB+3,C' '                                                   
         BH    *+8                                                              
         MVI   LOGOJOB+3,C'X'                                                   
         MVC   LOGOJOB+4(4),=C'ASM '                                            
         GOTO1 LOGO,DMCB,LOGOD                                                  
*                                                                               
SORTO26  GOTO1 TOTED,BUFRDR                                                     
         GOTO1 DISBUF                                                           
*                                                                               
SORTO28  MVI   SPACING,2                                                        
         GOTO1 PRINTIT                                                          
         GOTO1 ADDTOT                                                           
         B     SORTO14                                                          
*                                                                               
SORTO30  TM    RUNINDS,RUNIWRNO                                                 
         BNZ   SORTO60                                                          
         L     R2,AIN              WRITE SORTED RECORDS TO DCB IN               
         OPEN  ((2),(OUTPUT))                                                   
         LTR   RF,RF                                                            
         JNZ   *+2                                                              
*                                                                               
         CLI   SORTSW,0            TEST ANY SORT RECORDS PUT                    
         BE    SORTO50                                                          
*                                                                               
SORTO32  GOTO1 ADSORTER,DMCB,SORTGET                                            
         ICM   R3,15,4(R1)                                                      
         BZ    SORTO50                                                          
         LA    R4,SORTKEY-SORTRLEN(R3)                                          
         LA    R3,SORTKEYL(R3)                                                  
                                                                                
SORTO34  TM    FLTKEY,FLTKLOW                                                   
         BZ    SORTO36                                                          
         CLC   0(L'LOWKEY,R4),LOWKEY                                            
         BL    SORTO32             Get next                                     
                                                                                
SORTO36  TM    FLTKEY,FLTKHI                                                    
         BZ    SORTO40                                                          
         CLC   0(L'HIGHKEY,R4),HIGHKEY                                          
         BH    SORTO32             Get next                                     
                                                                                
SORTO40  TM    DUMPSW,DUMPSKEY                                                  
         BZ    SORTO42                                                          
         LA    RF,L'SORTKEY                                                     
         GOTOR VPRNTBL,DMCB,=C'SORT KEY',(R4),C'DUMP',(RF),=C'1D',     +        
               (C'P',PRINT)                                                     
                                                                                
SORTO42  TM    DUMPSW,DUMPSREC                                                  
         BZ    SORTO46                                                          
         LH    RF,SORTRLEN                                                      
         GOTOR VPRNTBL,DMCB,=C'SORT REC',(R3),C'DUMP',(RF),=C'1D',     +        
               (C'P',PRINT)                                                     
                                                                                
SORTO46  PUT   (2),(3)                                                          
         B     SORTO32                                                          
*                                                                               
SORTO50  CLOSE ((2))                                                            
         LTR   RF,RF                                                            
         JNZ   *+2                                                              
*                                                                               
         FREEPOOL (2)                                                           
         LTR   RF,RF                                                            
         JNZ   *+2                                                              
*                                                                               
         GOTO1 ADSORTER,DMCB,SORTEND                                            
         MVI   SORTSW,0                                                         
                                                                                
         GOTO1 LOGIO,PARA,1,=C'SORT/MERGE COMPLETE'                             
                                                                                
***********************************************************************         
* Put out SMF records for balancing file                                        
***********************************************************************         
SORTO60  CLI   BUFFSW,0            Test any Buffalo records put                 
         BE    SORTOX              No, so done                                  
         XC    BUFKEY(BUFKEYL),BUFKEY                                           
         MVI   BUFRTYPE,BUFRTSMF                                                
         GOTO1 BUFFALO,DMCB,BUFRDH,ABUFF,BUFKEY,1                               
         B     SORTO66                                                          
*                                                                               
SORTO64  GOTO1 BUFFALO,DMCB,BUFSEQ,(3,ABUFF),BUFKEY,1                           
*                                                                               
SORTO66  TM    8(R1),X'80'         Test EOF                                     
         BO    SORTOX              Done                                         
         GOTO1 GOSMF                                                            
         B     SORTO64             Next record                                  
*                                                                               
SORTOX   DS    0H                                                               
         MVI   BUFFSW,0                                                         
         B     XIT                                                              
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* READ WORKER FILES FOR REQUESTED SYSTEM AND OUTPUT A00 ODDS FILES    *         
* FOR ALL USERS WHO HAVE POSTINGS TO BE UPDATED TONIGHT, ALSO OUTPUT  *         
* A07 (DAILY TRIAL BALANCE) ODDS FILES FOR ALL COMPANIES.             *         
***********************************************************************         
ODDS     GOTO1 BUFFALO,PARA,BUFSET,ABUFF                                        
*                                                                               
ODDS02   GOTO1 WORKER,DMCB,WKINDX,AIBUFF,WKID,0                                 
         TM    8(R1),X'80'         TEST END OF INDEX                            
         BNZ   ODDS04                                                           
         CLI   WKTYPE,WKTPOST      POSTING-TYPE ONLY                            
         BNE   ODDS02                                                           
         TM    WKSTAT,WKSKEEP      DONT WANT KEEP-STATUS IDS                    
         BNZ   ODDS02                                                           
         TM    WKSTAT,WKSHOLD      DONT WANT HOLD-STATUS IDS                    
         BNZ   ODDS02                                                           
*                                                                               
         GOTO1 GETUID,WKUSER       GET USER-ID VALUES                           
         BNE   ODDS02                                                           
         L     R1,AUIDNTRY                                                      
         CLI   WKTYPE,WKTODDS      TEST ODDS FILE                               
         BE    ODDS02                                                           
         GOTO1 GETPRG,WKSYSPRG     GET PROGRAM VALUES                           
         BNE   ODDS02                                                           
         USING PRGTABD,R5                                                       
         TM    PRGTIND2,PRGTINOT   TEST DON'T PROCESS FILES FOR TODAY           
         BZ    *+14                                                             
         CLC   WKDAY,TODAY1DD      MATCH FILE DAY TO DAY TODAY                  
         BE    ODDS02                                                           
*                                                                               
         XC    BUFKEY(BUFKEYL),BUFKEY                                           
         MVI   BUFRTYPE,BUFRTDDS   BUILD COMPANY RECORD                         
         L     R1,AUIDNTRY                                                      
         MVC   BUFRCPY,UIDTCPY-UIDTABD(R1)                                      
         MVC   BUFRUSER,WKUSER     USER-ID NO AND DAY FOR EACH                  
         ZAP   BUFRDR,PZERO                                                     
         ZAP   BUFRCR,PZERO                                                     
         ZAP   BUFRPONE,=P'1'      FORCE BUFFALO NOT TO DROP RECORD             
         GOTO1 BUFFALO,PARA,BUFPUT,ABUFF,BUFREC                                 
         B     ODDS02                                                           
*                                                                               
ODDS04   XC    BUFKEY(BUFKEYL),BUFKEY                                           
         MVI   BUFRTYPE,BUFRTDDS                                                
         GOTO1 BUFFALO,DMCB,BUFRDH,ABUFF,BUFKEY,1                               
         B     ODDS08                                                           
*                                                                               
ODDS06   GOTO1 BUFFALO,DMCB,BUFSEQ,(1,ABUFF),BUFKEY,1                           
*                                                                               
ODDS08   TM    8(R1),X'80'         TEST EOF                                     
         BNZ   ODDS10                                                           
         XC    ODDREC(ODDRECL),ODDREC                                           
         MVC   ODDRLEN,=Y(ODDRECL) BUILD A00 ODDS RECORD                        
         GOTO1 GETCPY,BUFRCPY                                                   
         MVC   ODDRCOMP,BUFRCPY                                                 
         MVC   ODDRUSER,BUFRUSER                                                
         MVC   ODDRNAME,COMPNAME                                                
         GOTO1 PUTOUT02                                                         
         B     ODDS06                                                           
*                                                                               
ODDS10   MVC   OUTPROG,OUTPDTBL    SET DAILY TRIAL BALANCE                      
         L     R2,ADCOMP                                                        
         USING CPYRECD,R2                                                       
         MVC   CPYKEY,SPACES                                                    
ODDS12   IC    RF,CPYKCPY          BUMP TO NEXT COMPANY                         
         LA    RF,1(RF)                                                         
         MVC   CPYKEY,SPACES                                                    
         STC   RF,CPYKCPY                                                       
         GOTO1 DATAMGR,DMCB,DMRDHI,ACCFIL,CPYRECD,CPYRECD                       
         BNE   ODDS14                                                           
         CLI   CPYKCPY,X'FF'       TEST EOF RECORD                              
         BE    ODDS14                                                           
         GOTO1 GETCPY,CPYKCPY                                                   
         XC    ODDREC(ODDRECL),ODDREC                                           
         MVC   ODDRLEN,=Y(ODDRECL) BUILD A07 ODDS RECORD                        
         MVC   ODDRCOMP,CPYKCPY                                                 
         MVC   ODDRUSER,COMPUSER                                                
         MVC   ODDRNAME,COMPNAME                                                
         GOTO1 PUTOUT02                                                         
         B     ODDS12                                                           
*                                                                               
ODDS14   L     RF,WORKER                                                        
         GOTO1 ,DMCB,WKCLOS,AOBUFF,0,0                                          
         TM    RUNINDS,RUNIWRNO                                                 
         BNZ   *+6                                                              
         BASR  RE,RF                                                            
         CLI   8(R1),0             TEST FOR ERRORS                              
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
ODDSX    B     XIT                                                              
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* READ ALL WKFILE INDEX RECORDS AND FILTER OUT WORKER FILES THAT ARE  *         
* REQUIRED FOR THIS ACCOUNT SYSTEM AND SET STATUS TO KEEP.            *         
***********************************************************************         
*                                                                               
HOLD     LA    R2,WKHOLD                                                        
         MVI   FORCEHED,C'Y'                                                    
         B     *+8                                                              
*                                                                               
KEEP     LA    R2,WKKEEP                                                        
*                                                                               
KEEP01   GOTO1 WORKER,DMCB,WKINDX,AIBUFF,WKID,0                                 
         TM    8(R1),X'80'         TEST END OF INDEX                            
         BNZ   KEEPX                                                            
         TM    WKSTAT,WKSKEEP      DONT WANT KEEP-STATUS IDS                    
         BNZ   KEEP01                                                           
         TM    WKSTAT,WKSHOLD      DONT WANT HOLD-STATUS IDS                    
         BNZ   KEEP01                                                           
*&&US                                                                           
         OC    ONEFILE,ONEFILE     TEST SINGLE FILE INPUT                       
         BZ    KEEP02                                                           
         CLC   WKKEY,ONEFILE                                                    
         BNE   KEEP01                                                           
         CLC   WKSEQN,ONEFILE+(WKSEQN-WKID)                                     
         BNE   KEEP01                                                           
         B     KEEP09                                                           
*&&                                                                             
*                                                                               
KEEP02   CLC   RCFFPARM(L'WKSYSPRG),SPACES                                      
         BNH   KEEP03                                                           
         CLC   WKSYSPRG,RCFFPARM   APPLY SINGLE WORK FILE FILTER                
         BNE   KEEP01                                                           
         B     KEEP05                                                           
*                                                                               
KEEP03   CLI   FLTTYP,0            TEST ANY FILE TYPE FILTERS                   
         BE    KEEP05                                                           
         LA    RF,FLTTYP           MATCH FILE TO TABLE OF FILTERS               
         LA    R0,FLTTYPN                                                       
*                                                                               
KEEP04   CLI   0(RF),0             TEST END OF TABLE                            
         BE    KEEP01              NOT FOUND, SKIP WORKER FILE                  
         CLC   WKSYSPRG,0(RF)      TAKE ONLY FILES IN TABLE                     
         BE    KEEP05                                                           
         LA    RF,L'FLTTYP(RF)                                                  
         BCT   R0,KEEP04                                                        
         B     KEEP01                                                           
*                                                                               
KEEP05   CLI   WKTYPE,WKTCHQS      TEST CHEQUE FILE                             
         BE    KEEP09                                                           
         CLI   WKTYPE,WKTPOST      TEST POSTING FILE                            
         BE    KEEP09                                                           
         CLI   WKTYPE,WKTODDS      TEST ODDS FILE                               
         BNE   KEEP01                                                           
*                                                                               
         LA    R1,ODDDAYS          TEST ODDS FILE IS 3 TO 6 DAYS OLD            
         LA    R0,L'ODDDAYS                                                     
KEEP07   CLC   WKDAY,0(R1)                                                      
         BE    KEEP09                                                           
         LA    R1,1(R1)                                                         
         BCT   R0,KEEP07                                                        
         B     KEEP01                                                           
*                                                                               
KEEP09   GOTO1 GETUID,WKUSER       GET USER-ID VALUES                           
         BNE   KEEP01                                                           
         L     R1,AUIDNTRY                                                      
         CLI   WKTYPE,WKTODDS      TEST ODDS FILE                               
         BE    KEEP11                                                           
         GOTO1 GETPRG,WKSYSPRG     GET PROGRAM VALUES                           
         BNE   KEEP01                                                           
         TM    PRGTIND2,PRGTINOT   TEST DON'T PROCESS FILES FOR TODAY           
         BZ    *+14                                                             
         CLC   WKDAY,TODAY1DD      MATCH FILE DAY TO DAY TODAY                  
         BE    KEEP01                                                           
*                                                                               
KEEP11   L     RF,WORKER                                                        
         GOTO1 ,DMCB,(R2),AIBUFF,WKID,0                                         
         TM    RUNINDS,RUNIWRNO                                                 
         BNZ   *+6                                                              
         BASR  RE,RF                                                            
         MVC   P,SPACES                                                         
         GOTO1 DISWRK,P+1                                                       
         GOTO1 ACREPORT                                                         
*                                                                               
         LH    R1,OUTCNT           ADD ELEMENT TO OUTPUT RECORD                 
         LA    R0,1(R1)                                                         
         STH   R0,OUTCNT                                                        
         MH    R1,=Y(OUTLNQ)                                                    
         LA    R1,OUTIO+4(R1)                                                   
         USING OUTELD,R1                                                        
         MVI   OUTEL,OUTELQ        BUILD OUTIO ELEMENT                          
         MVI   OUTLN,OUTLNQ                                                     
         MVC   OUTKEY,WKID                                                      
         LA    R1,OUTLNQ+1(R1)                                                  
         LA    R0,OUTIO                                                         
         SR    R1,R0                                                            
         SLL   R1,16                                                            
         STCM  R1,15,OUTIO         SET OUTPUT RECORD LENGTH                     
         CLC   OUTCNT,=Y(OUTMAXN)  TEST RECORD FULL                             
         BL    KEEP01                                                           
         GOTO1 PUTOUT              YES - WRITE OUTPUT RECORD                    
         B     KEEP01                                                           
         DROP  R1                                                               
*                                                                               
KEEPX    GOTO1 PUTOUT              PUT LAST OUTPUT RECORD (IF ANY)              
         L     RF,WORKER                                                        
         GOTO1 ,DMCB,WKCLOS,AOBUFF,0,0                                          
         TM    RUNINDS,RUNIWRNO                                                 
         BNZ   *+6                                                              
         BASR  RE,RF                                                            
         CLI   8(R1),0             TEST FOR ERRORS                              
         BE    *+6                                                              
         DC    H'0'                                                             
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* READ A09 ODDS FILE AND UNKEEP ALL FILES (THAT ARE IN KEEP STATUS)   *         
***********************************************************************         
ACTV     GOTO1 WORKER,DMCB,WKINDX,AOBUFF,OUTID,0                                
         CLI   8(R1),0                                                          
         BNE   ACTVX                                                            
*                                                                               
ACTV02   GOTO1 WORKER,DMCB,WKREAD,AOBUFF,OUTID,OUTIO                            
         TM    8(R1),X'80'         TEST EOF                                     
         BNZ   ACTVX                                                            
         CLI   8(R1),0             TEST FOR ERRORS                              
         BE    *+6                                                              
         DC    H'0'                                                             
         SR    RE,RE               SET END OF RECORD                            
         ICM   RE,3,OUTIO                                                       
         LA    RE,OUTIO(RE)                                                     
         MVI   0(RE),0                                                          
         LA    R2,OUTIO+4                                                       
         USING OUTELD,R2                                                        
*                                                                               
ACTV04   CLI   OUTEL,0             TEST END OF RECORD                           
         BE    ACTV02                                                           
         XC    WKID,WKID                                                        
         MVC   WKKEY,OUTKEY                                                     
         GOTO1 WORKER,DMCB,WKINDX,AIBUFF,WKID,0                                 
         CLI   8(R1),0                                                          
         BNE   ACTV06                                                           
         TM    WKSTAT,WKSKEEP      TEST FILE IN KEEP STATUS                     
         BZ    ACTV06                                                           
         L     RF,WORKER                                                        
         GOTO1 ,DMCB,WKACTV,AIBUFF,WKID,0                                       
         TM    RUNINDS,RUNIWRNO                                                 
         BNZ   *+6                                                              
         BASR  RE,RF                                                            
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 GETPRG,WKSYSPRG     GET PROGRAM VALUES                           
         GOTO1 DISWRK,P+1                                                       
         GOTO1 ACREPORT                                                         
*                                                                               
ACTV06   LA    R2,OUTLNQ(R2)       BUMP TO NEXT ELEMENT                         
         B     ACTV04                                                           
*                                                                               
ACTVX    B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO ADD RECORD TO OUTPUT WORKER FILE                         *         
***********************************************************************         
PUTOUT   OC    OUTCNT,OUTCNT       TEST RECORD EMPTY                            
         BZR   RE                                                               
         XC    OUTCNT,OUTCNT                                                    
PUTOUT02 NTR1  ,                                                                
         L     RF,WORKER                                                        
         GOTO1 ,DMCB,WKADDR,AOBUFF,OUTID,OUTIO                                  
         TM    RUNINDS,RUNIWRNO                                                 
         BNZ   *+6                                                              
         BASR  RE,RF                                                            
         CLI   8(R1),0             TEST WORKER ERRORS                           
         BE    *+6                                                              
         DC    H'0'                                                             
         LA    R0,OUTIO                                                         
         LA    R1,OUTIOL                                                        
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
PUTOUTX  B     XIT                                                              
***********************************************************************         
* ROUTINE TO LOOK-UP PROGRAM VALUE IN PRGTAB                          *         
*                                                                     *         
* NTRY - R1=A(SYSTEM/PROGRAM VALUE)                                   *         
* EXIT - R5=A(PRGTAB ENTRY) WITH CC EQUAL IF FOUND                    *         
*        R5=A(0) WITH CC NOT EQUAL IF NOT FOUND                       *         
***********************************************************************         
GETPRG   L     R5,APRGTAB                                                       
GETPRG02 CLI   PRGTABD,PRGTEOTQ    TEST EOT                                     
         BE    GETPRGN                                                          
         CLC   PRGTSP,0(R1)        MATCH ON SYSTEM & PROGRAM                    
         BE    GETPRGY                                                          
GETPRG04 LA    R5,PRGTABL(R5)                                                   
         B     GETPRG02                                                         
*                                                                               
GETPRGY  CR    RE,RE               SET CC EQUAL                                 
         BR    RE                                                               
GETPRGN  LTR   RE,RE               SET CC NOT EQUAL                             
         BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO FORMAT A WORKER KEY INTO PRINTABLE FORMAT                *         
*                                                                     *         
* NTRY - R1=A(OUTPUT AREA), WKID CONTAINS WORKER KEY                  *         
*                                                                     *         
* EXIT - R1=A(NEXT AVAILABLE OUTPUT BYTE)                             *         
***********************************************************************         
DISWRK   NTR1  ,                                                                
         LR    R2,R1               R2=A(OUTPUT DISPLAY AREA)                    
         USING UIDTABD,R1                                                       
         ICM   R1,15,AUIDNTRY                                                   
         BZ    *+14                                                             
         CLC   UIDTUSER,WKUSER     TEST CURRENT USER-ID                         
         BE    DISWRK02                                                         
         GOTO1 GETUID,WKUSER       LOOK UP USER-ID VALUES                       
         BE    *+6                                                              
         DC    H'0'                                                             
         ICM   R1,15,AUIDNTRY                                                   
DISWRK02 MVC   0(L'UIDTCODE,R2),UIDTCODE                                        
         LA    R2,L'UIDTCODE-1(R2)                                              
         CLI   0(R2),C' '                                                       
         BH    *+8                                                              
         BCT   R2,*-8                                                           
*                                                                               
         AHI   R2,1                                                             
         MVI   0(R2),C','                                                       
         AHI   R2,1                                                             
*                                                                               
         MVC   0(L'WKSYSPRG,R2),WKSYSPRG                                        
         LA    R2,L'WKSYSPRG(R2)                                                
*                                                                               
         MVC   0(L'WKSUB,R2),WKSUB                                              
         CLI   0(R2),0                                                          
         BNE   *+8                                                              
         MVI   0(R2),C'*'                                                       
         LA    R2,L'WKSUB(R2)                                                   
*                                                                               
         UNPK  WORK(3),WKDAY(2)                                                 
         MVC   0(2,R2),WORK                                                     
         AHI   R2,2                                                             
*                                                                               
         MVC   0(L'WKTYPE,R2),WKTYPE                                            
         AHI   R2,L'WKTYPE                                                      
*                                                                               
         CLI   WKEXTRA,C' '                                                     
         BNH   DISWRK08                                                         
         MVC   0(L'WKEXTRA,R2),WKEXTRA                                          
         AHI   R2,L'WKEXTRA                                                     
*                                                                               
DISWRK08 MVI   0(R2),C','                                                       
         AHI   R2,1                                                             
*                                                                               
         SR    R0,R0                                                            
         ICM   R0,3,WKSEQN                                                      
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  0(5,R2),DUB                                                      
         CLI   0(R2),C'0'                                                       
         BNE   DISWRK20                                                         
         MVC   0(5,R2),1(R2)                                                    
         B     *-14                                                             
*                                                                               
DISWRK20 LA    R1,4(R2)                                                         
         CLI   0(R1),C' '                                                       
         BH    *+8                                                              
         BCT   R1,*-8                                                           
         LA    R1,2(R1)                                                         
DISWRKX  XIT1  REGS=(R1)           RETURN A(NEXT SLOT) TO CALLER                
         DROP  R1                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO EDIT TOTALS INTO PRINT LINE                              *         
*                                                                     *         
* NTRY - R1=A(2 8 BYTE ACCUMULATORS)                                  *         
***********************************************************************         
TOTED    NTR1  ,                                                                
         LR    R2,R1                                                            
         CURED (P8,0(R2)),(L'LINDR,LINDR),2,MINUS=YES                           
         CURED (P8,8(R2)),(L'LINCR,LINCR),2,MINUS=YES                           
TOTEDX   B     XIT                                                              
***********************************************************************         
* ROUTINE TO CLEAR ACCUMS TO ZERO AND SET NEW PAGE                    *         
***********************************************************************         
CLRTOT   MVI   FORCEHED,YES                                                     
         MVC   PAGE,=H'1'                                                       
         ZAP   TOTDR,PZERO                                                      
         ZAP   TOTCR,PZERO                                                      
         ZAP   IDRECS,PZERO                                                     
         ZAP   IDCASH,PZERO                                                     
         ZAP   DELCR,PZERO                                                      
         ZAP   DELDR,PZERO                                                      
         ZAP   ADDCR,PZERO                                                      
         ZAP   ADDDR,PZERO                                                      
         ZAP   TALDR,PZERO                                                      
         ZAP   TALCR,PZERO                                                      
         ZAP   EXPDR,PZERO                                                      
         ZAP   EXPCR,PZERO                                                      
CLRTOTX  BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO ADD AMOUNTS TO TOTALS ACCUMULATORS                       *         
***********************************************************************         
ADDTOT   AP    TOTDR,BUFRDR                                                     
         AP    TOTCR,BUFRCR                                                     
         TM    PRGTIND1,PRGTIPOF   TEST PEEL-OFF PROGRAM                        
         BNZ   ADDTPOF                                                          
         TM    PRGTIND1,PRGTITPY   TEST TALENT PAYMENTS                         
         BNZ   ADDTTPY                                                          
         TM    PRGTIND1,PRGTICEX   TEST COKE EXPENDITURE                        
         BNZ   ADDTCEX                                                          
*                                                                               
         AP    ADDDR,BUFRDR        ALL OTHER FILES                              
         AP    ADDCR,BUFRCR                                                     
         B     ADDTOTX                                                          
*                                                                               
ADDTPOF  AP    DELDR,BUFRDR                                                     
         AP    DELCR,BUFRCR                                                     
         B     ADDTOTX                                                          
*                                                                               
ADDTTPY  AP    TALDR,BUFRDR                                                     
         AP    TALCR,BUFRCR                                                     
         B     ADDTOTX                                                          
*                                                                               
ADDTCEX  AP    EXPDR,BUFRDR                                                     
         AP    EXPCR,BUFRCR                                                     
*                                                                               
ADDTOTX  BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO PRINT FILE TOTALS                                        *         
***********************************************************************         
PRTTOT   NTR1  ,                                                                
         L     RF,ADBXAREA         DRAW A BOX LINE                              
         MVI   BOXREQ-BOXD(RF),C'B'                                             
         GOTO1 PRINTIT                                                          
         LA    R2,TOTTAB                                                        
         USING TOTTABD,R2          R2=A(TOTALS TABLE)                           
PRTTOT02 CLI   TOTTDR,TOTTEOTQ     TEST EOT                                     
         BE    PRTTOTX                                                          
         TM    TOTTINDS,TOTTITST   TEST DON'T PRINT ZERO AMOUNTS                
         BZ    PRTTOT04                                                         
         CP    TOTTDR,PZERO        TEST BOTH AMOUNTS ZERO                       
         BNE   PRTTOT04                                                         
         CP    TOTTCR,PZERO                                                     
         BE    PRTTOT06                                                         
PRTTOT04 SR    RF,RF                                                            
         ICM   RF,3,TOTTDISP                                                    
         LA    RF,DICO(RF)                                                      
         MVC   P+4(TOTTDSCL),0(RF)                                              
         GOTO1 TOTED,TOTTDR                                                     
         GOTO1 PRINTIT                                                          
PRTTOT06 LA    R2,TOTTABL(R2)      BUMP TO NEXT TABLE ENTRY                     
         B     PRTTOT02                                                         
PRTTOTX  B     XIT                                                              
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* SMFOUT for balancing                                                          
***********************************************************************         
         USING SMFBRECD,R2                                                      
         USING SMFBLKD,R8                                                       
GOSMF    NTR1  ,                                                                
         L     R8,ASMFAREA                                                      
         LA    R2,SMFREC                                                        
         XC    SMFREC,SMFREC                                                    
         MVC   SMFBTXT1,SPACES                                                  
         MVC   SMFBTXT2,SPACES                                                  
         LHI   RF,SMFBRECX-SMFBRECD Length of SMF data record                   
         STH   RF,SMFBLEN                                                       
         MVC   SMFBDRS(6*L'SMFBMNY),=6PL8'0'                                    
         MVC   SMFBINFO,=C'BAL1'                                                
         CLI   SMF,YES             This is a draft with SMF recs                
         BE    GOSMF10             Yes so BAL1                                  
         CLI   IS_ACSM,YES         Was it the AC135Nx                           
         BE    GOSMF10             Probably                                     
         MVC   SMFBINFO,=C'BAL2'   No so make it BAL2                           
                                                                                
GOSMF10  GOTO1 GETPRG,BUFRSYSP     Get program table entry in R5                
         BE    *+6                                                              
         DC    H'00'                                                            
                                                                                
         TM    PRGTIND1,PRGTIJRN   Test DAILY JOURNAL                           
         BZ    GOSMF12                                                          
         XC    BUFRCODE,BUFRCODE   Clear superfluous ids                        
         B     GOSMF15                                                          
                                                                                
GOSMF12  XC    AUIDNTRY,AUIDNTRY                                                
         GOTO1 GETUID,BUFRUSER     Get User-id information                      
         BE    *+6                                                              
         DC    H'00'                                                            
                                                                                
GOSMF15  MVC   SMFBSRCE,SPACES                                                  
         MVC   SMFBWPRG,BUFRSYSP   Program, ie A21, ADJ etc                     
         MVC   SMFBWSUB,BUFRSPRG   Sub-type or ledger / SE char                 
         CLI   SMFBWSUB,0                                                       
         BNE   *+8                                                              
         MVI   SMFBWSUB,C'*'                                                    
         CLI   BUFRXTRA,C' '                                                    
         BNH   *+10                                                             
         MVC   SMFBWXTA,BUFRXTRA   2nd letter of system id                      
         MVC   SMFBWDAY,BUFRDAY                                                 
         MVI   SMFBWCLS,C'P'                                                    
         MVC   SMFBWSEQ,BUFRSEQ#                                                
         MVI   SMFBTYPE,C'M'       Dollars/Pounds                               
         MVI   SMFBTYPO,C' '                                                    
         TM    PRGTIND1,PRGTICLR   Test SPOT/PRINT clearances                   
         BZ    GOSMF18                                                          
         MVI   SMFBTYPO,C'S'       Other system  (Clearences)                   
         MVC   SMFBOSE#,BUFROSE#                                                
         LLH   R1,SMFBOSE#         Get Other SE name                            
         GOTOR GETSYSN             Get ACC file                                 
         MVC   SMFBOSEN,SYSNME     SPTXX,NETXX,PRTXX                            
         B     GOSMF20                                                          
                                                                                
GOSMF18  TM    PRGTIND1,PRGTICHQ   Checks                                       
         BZ    GOSMF20                                                          
         MVI   SMFBTYPO,C'L'       Ledger        (Checks)                       
         MVI   SMFBLDGR,C'S'                                                    
         MVC   SMFBLDGR+1(1),BUFRSPRG                                           
                                                                                
         USING MASTD,RF                                                         
GOSMF20  L     RF,ADMASTC                                                       
                                                                                
         USING SSBOFFD,RE          Set DSPACE from SSB                          
         ICM   RE,15,MCSSB                                                      
         BNZ   *+6                                                              
         DC    H'00'                                                            
         MVC   SMFBDSPC,SSODSPAC                                                
         DROP  RE                                                               
                                                                                
         USING UTLD,RE             Set system from UTL                          
         L     RE,MCUTL                                                         
         MVC   SMFBSENO,TSYS                                                    
         DROP  RE,RF                                                            
                                                                                
         LLC   R1,SMFBSENO                                                      
         GOTOR GETSYSN             Get ACC file                                 
         MVC   SMFBSENM,SYSNME     ACCxx                                        
         MVC   SMFBUID,BUFRUSER    Company user number                          
         MVC   SMFBAGY,BUFRCPY     Company code                                 
                                                                                
         USING UIDTABD,R4                                                       
         ICM   R4,15,AUIDNTRY                                                   
         BZ    GOSMF38                                                          
         MVC   SMFBAGYA,UIDTALPH   Company alpha                                
         MVC   SMFBTXT,UIDTCODE    User id code                                 
         DROP  R4                                                               
GOSMF38  MVC   SMFBDATE,FILEJU     Date assoicated with file                    
         ZAP   SMFBDRS,BUFRDR      Debits                                       
         ZAP   SMFBCRS,BUFRCR      Credits                                      
         CP    SMFBDRS,SMFBCRS                                                  
         BE    *+8                                                              
         MVI   SMFBWARN+3,1        Debits NEQ to Credits                        
                                                                                
         TM    SMFIND,SMFIXA+SMFIDCB                                            
         BZ    GOSMF70             Put to SMF as we process                     
***********************************************************************         
* Put SMF record to output file DISP=(PASS,SHR)                       *         
***********************************************************************         
         TM    SMFIND,SMFIDCB                                                   
         BZ    GOSMF60                                                          
         ICM   R5,15,SMFADCB                                                    
         PUT   (R5),(R2)                                                        
         B     GOSMF68             Add one to count                             
                                                                                
***********************************************************************         
* Put SMF record to XA to be output to SMF by alternate caller        *         
***********************************************************************         
GOSMF60  TM    SMFIND,SMFIXA                                                    
         BO    *+6                                                              
         DC    H'00'                                                            
                                                                                
         ICM   R5,15,SMFXAMEM                                                   
         LLH   RF,SMF#RECS                                                      
         CH    RF,SMF#MAX                                                       
         BNH   *+6                 Put directly                                 
         DC    H'00'               Fail for the moment                          
                                                                                
         MHI   RF,SMFBRECX-SMFBRECD                                             
         AR    R5,RF                                                            
         SAM31                                                                  
         MVC   0(SMFBLNQ,R5),SMFREC                                             
         SAM24                                                                  
*                                                                               
GOSMF68  LLH   RF,SMF#RECS                                                      
         AHI   RF,1                                                             
         STH   RF,SMF#RECS                                                      
         B     GOSMFXIT                                                         
                                                                                
***********************************************************************         
* Print SMF records for debug                                         *         
***********************************************************************         
GOSMF70  TM    RUNINDS,RUNITEST    RUN=TEST                                     
         BZ    GOSMF80                                                          
         CLI   SMF,YES             Over-ride                                    
         BE    GOSMF80                                                          
         LLH   RF,SMFBLEN                                                       
         GOTOR VPRNTBL,DMCB,=C'SMF REC',(R2),C'DUMP',(RF),=C'1D',      +        
               (C'P',PRINT)                                                     
         B     GOSMFXIT                                                         
                                                                                
***********************************************************************         
* Put SMF record direct to SMF                                        *         
***********************************************************************         
         USING COMFACSD,R3                                                      
GOSMF80  ICM   R3,15,ADCOMFAC      A(COMFACS)                                   
         JZ    GOSMFXIT                                                         
         ICM   RF,15,CSMFOUT       SMFOUT routine                               
         BZ    GOSMFXIT                                                         
         LHI   R6,12               Type 12 file balance                         
         TM    RUNINDS,RUNIWRNO                                                 
         BZ    *+8                                                              
         OILH  GR6,X'8000'         R6 is not really that grand                  
GOSMF82  GOTO1 (RF),SMFPARM,(R6),SMFREC                                         
                                                                                
GOSMFXIT J     XIT                                                              
         DROP  R2,R3,R8                                                         
                                                                                
         DS    0D                                                               
         DC    4CL8'*SMFREC*'                                                   
SMFAREA  DS    XL(SMFBLNQ)         SMF balance record                           
         EJECT                                                                  
***********************************************************************         
* Get system name, SPOT, ACC, PRINT                                   *         
***********************************************************************         
GETSYSN  NTR1  ,                                                                
         MVC   SYSNME,SPACES                                                    
         STH   R1,SE#              Save off SE#                                 
         STCM  R1,3,SYSSE#                                                      
         CHI   R1,0                Do we have an SE#?                           
         JE    XIT                   No - Don't make a call to DDNAME           
         GOTO1 DATAMGR,PARA,(0,DDNAME),SYSSE,0                                  
         TM    8(R1),X'10'         SYSTEM NOT FOUND                             
         JO    *+2                                                              
         LT    RE,8(,R1)           GET A(DDNADATA)                              
         JZ    *+2                                                              
         USING DDNAMED,RE                                                       
         MVC   SYSNME,DDNASENA                                                  
         DROP  RE                                                               
         J     XIT                                                              
*                                                                               
SYSSE    DC    C'SE=',X'0000'                                                   
         ORG   SYSSE+3                                                          
SYSSE#   DS    XL2                                                              
         EJECT                                                                  
***********************************************************************         
* Get SE# from one character value                                    *         
***********************************************************************         
REVSE#   NTR1                                                                   
         MVC   SYSNME,SPACES                                                    
         MVC   SYSALPHA,BUFRSYSP                                                
         MVC   SYSLETTR,BUFRSPRG                                                
         OC    SYSLETTR,SPACES                                                  
         CLI   SYSALPHA,C'A'       Is this on-line clearance?                   
         JE    XIT                   Yes                                        
         GOTO1 DATAMGR,PARA,(0,DDNAME),SYSINFO                                  
         TM    8(R1),X'10'         SYSTEM NOT FOUND                             
         JO    *+2                                                              
         LT    RE,8(,R1)           GET A(DDNADATA)                              
         JZ    *+2                                                              
*                                                                               
         USING DDNAMED,RE                                                       
         MVC   SE#,DDNASENO-1      SE number                                    
         MVC   SYSNME,DDNASENA     SE Name                                      
         J     XIT                                                              
         DROP  RE                                                               
         J     XIT                                                              
DDNAME   DC    CL8'DDNAME'                                                      
SYSINFO  DC    C'S='                                                            
SYSALPHA DC    C' '                                                             
SYSLETTR DC    C'  '                                                            
         EJECT ,                                                                
***********************************************************************         
* Routine to build a display line from a Buffalo record               *         
***********************************************************************         
DISBUF   NTR1  ,                                                                
         LA    R2,LINFILE                                                       
         GOTO1 GETPRG,BUFRSYSP     GET PROGRAM TABLE ENTRY                      
         BE    *+6                                                              
         DC    H'0'                                                             
         TM    PRGTIND1,PRGTICLR   TEST SPOT/PRINT CLEARANCES                   
         BNZ   DISPCLR                                                          
         SR    RF,RF                                                            
         ICM   RF,3,PRGTDISP                                                    
         LA    RF,DICO(RF)                                                      
         MVC   0(PRGTNAML,R2),0(RF)                                             
         CLC   0(PRGTNAML,R2),SPACES                                            
         BNE   *+8                                                              
         MVI   0(R2),C'?'                                                       
         LA    R2,PRGTNAML-1(R2)                                                
         TM    PRGTIND1,PRGTICHQ   TEST CHEQUE RUNS                             
         BNZ   DISPCHQ                                                          
         B     DISBUF12                                                         
*                                                                               
DISPCLR  SR    RF,RF                                                            
         ICM   RF,3,PRGTDISP                                                    
         LA    RF,DICO(RF)                                                      
         MVC   0(PRGTNAML,R2),0(RF)                                             
         LA    R1,PRGTNAML-1(R2)                                                
         LA    R2,1(R1)                                                         
         LA    R0,PRGTNAML                                                      
         CLI   0(R1),C'&&'         SEARCH FOR REPLACEMENT CHARACTER             
         BE    *+14                                                             
         BCTR  R1,0                                                             
         BCT   R0,*-10                                                          
         B     DISBUF12                                                         
         CLI   BUFRSPRG,0          TEST REPLACEMENT CHARACTER PRESENT           
         BE    *+14                                                             
         MVC   0(L'BUFRSPRG,R1),BUFRSPRG                                        
         B     *+10                                                             
         MVC   0(2,R1),1(R1)       SQUASH OUT THE AMPERSAND                     
         B     DISBUF12                                                         
*                                                                               
DISPCHQ  L     R1,ACHQTAB                                                       
         USING CHQTABD,R1          R1=A(CHEQUE LEDGER TABLE)                    
DISPCHQ2 CLI   CHQTABD,CHQTEOTQ    TEST EOT                                     
         BE    DISBUF12                                                         
         CLC   CHQTLEDG,BUFRSPRG   MATCH ON LEDGER CODE                         
         BE    *+12                                                             
         LA    R1,CHQTABL(R1)      BUMP TO NEXT TABLE ENTRY                     
         B     DISPCHQ2                                                         
         SR    RF,RF                                                            
         ICM   RF,3,CHQTDISP                                                    
         LA    RF,DICO(RF)                                                      
         CLI   0(R2),C' '                                                       
         BH    *+8                                                              
         BCT   R2,*-8                                                           
         LA    R2,2(R2)                                                         
         MVC   0(CHQTDSCL,R2),0(RF)                                             
         LA    R2,CHQTDSCL-1(R2)                                                
         DROP  R1                                                               
*                                                                               
DISBUF12 CLI   0(R2),C' '                                                       
         BH    *+8                                                              
         BCT   R2,*-8                                                           
         LA    R2,2(R2)                                                         
         OC    BUFRCODE,BUFRCODE   TEST ID CODE PRESENT                         
         BZ    DISBUF14                                                         
         MVC   0(L'BUFRCODE,R2),BUFRCODE                                        
         LA    R2,L'BUFRCODE-1(R2)                                              
         CLI   0(R2),C' '                                                       
         BH    *+8                                                              
         BCT   R2,*-8                                                           
         LA    R2,2(R2)                                                         
*                                                                               
DISBUF14 UNPK  DUB(3),BUFRDAY(2)   CALCULATE THE FILE DATE                      
         MVC   WORK+0(4),TODAY0YY                                               
         MVC   WORK+4(2),DUB                                                    
         CLC   WORK+4(2),TODAY0DD                                               
         BNH   DISBUF16                                                         
         MVC   WORK+4(2),=C'01'    IF DAY IS HIGHER MUST BE LAST MONTH          
         GOTO1 ADDAY,PARA,WORK,WORK+6,-1                                        
         MVC   WORK+0(4),WORK+6                                                 
         MVC   WORK+4(2),DUB                                                    
DISBUF16 GOTO1 DATCON,PARA,(0,WORK),(8,0(R2))                                   
         GOTO1 DATCON,PARA,(0,WORK),(15,FILEJU)                                 
*                                                                               
DISBUFX  B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* Routine to establish user-id values                                 *         
*                                                                     *         
* NTRY - R1=A(User-id number)                                         *         
* EXIT - AUIDTAB points to UIDTAB entry                               *         
***********************************************************************         
         USING UIDTABD,R2          R2=A(USER-ID TABLE)                          
GETUID   NTR1  ,                                                                
         L     R2,AUIDTAB                                                       
         LA    R0,UIDTABMX         R0=MAXIMUM NUMBER OF ENTRIES                 
GETUID04 OC    UIDTUSER,UIDTUSER   TEST EOT                                     
         BZ    GETUID06                                                         
         CLC   UIDTUSER,0(R1)      MATCH ON USER-ID NUMBER                      
         BE    GETUID40                                                         
         LA    R2,UIDTABL(R2)                                                   
         BCT   R0,GETUID04                                                      
         DC    H'0'                USER-ID TABLE FULL                           
GETUID06 MVC   UIDTUSER,0(R1)                                                   
         LA    R3,IO                                                            
         USING CTIREC,R3                                                        
         XC    CTIKEY,CTIKEY       READ CONTROL FILE FOR NUMBER                 
         MVI   CTIKTYP,CTIKTYPQ                                                 
         MVC   CTIKNUM,UIDTUSER                                                 
         GOTOR DATAMGR,PARA,DMREAD,CTFILE,CTIKEY,CTIKEY                         
         BNE   GETUID40                                                         
*                                                                               
         LA    R1,CTIDATA          SEARCH USER-ID RECORD FOR VALUES             
         SR    R0,R0                                                            
GETUID08 CLI   0(R1),0             TEST EOR                                     
         BE    GETUID40                                                         
         CLI   0(R1),CTDSCELQ      TEST DESCRIPTION ELEMENT                     
         BE    GETUID12                                                         
         CLI   0(R1),CTSYSELQ      TEST SYSTEM ELEMENT                          
         BE    GETUID20                                                         
         CLI   0(R1),CTAGYELQ      TEST AGENCY ELEMENT                          
         BE    GETUID30                                                         
*                                                                               
GETUID10 IC    R0,1(R1)            BUMP TO NEXT ELEMENT                         
         AR    R1,R0                                                            
         B     GETUID08                                                         
*                                                                               
         USING CTDSCEL,R1                                                       
GETUID12 MVC   UIDTCODE,CTDSC      EXTRACT USER-ID CODE                         
         B     GETUID10                                                         
*                                                                               
         USING CTSYSEL,R1                                                       
GETUID20 CLI   CTSYSNUM,6          TEST ACCOUNTING SYTEM                        
         BNE   GETUID10                                                         
         CLC   CTSYSSE,SENO        MATCH ON SE NUMBER                           
         BNE   GETUIDN                                                          
         MVC   UIDTSENO,CTSYSSE    EXTRACT ACCOUNT SE NUMBER                    
         MVC   UIDTCPY,CTSYSAGB    EXTRACT COMPANY CODE                         
         B     GETUID10                                                         
*                                                                               
         USING CTAGYD,R1                                                        
GETUID30 MVC   UIDTALPH,CTAGYID    EXTRACT AGENCY ALPHA-ID                      
         MVC   UIDTLANG,CTAGYLNG   EXTRACT AGENCY LANGUAGE                      
         B     GETUID10                                                         
*                                                                               
GETUID40 CLI   UIDTSENO,0          TEST ACCOUNT SYSTEM SE NUMBER SET            
         BE    GETUIDN                                                          
         ST    R2,AUIDNTRY                                                      
         OC    UIDTCODE,UIDTCODE   TEST USER-ID CODE SET                        
         BNZ   GETUIDY                                                          
         SR    R0,R0               EDIT USER-ID NUMBER IF NO CODE               
         ICM   R0,3,UIDTUSER                                                    
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         MVC   UIDTCODE,SPACES                                                  
         UNPK  UIDTCODE(5),DUB                                                  
*                                                                               
GETUIDY  CR    RE,RE               OK - EXIT WITH CC=EQUAL                      
         B     GETUIDX                                                          
*                                                                               
GETUIDN  XC    AUIDNTRY,AUIDNTRY   ERROR - CLEAR VALUES & EXIT                  
         XC    UIDTABD(UIDTABL),UIDTABD                                         
         CLI   *,0                                                              
*                                                                               
GETUIDX  B     XIT                                                              
         DROP  R2,R3                                                            
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO READ COMPANY RECORD AND EXTRACT VALUES                   *         
***********************************************************************         
GETCPY   NTR1  ,                                                                
         LA    R2,KEY                                                           
         USING CPYRECD,R2                                                       
         MVC   CPYKEY,SPACES                                                    
         MVC   CPYKCPY,0(R1)                                                    
         L     R2,ADCOMP                                                        
         CLC   CPYKEY,KEY          TEST COMPANY REORD ALREADY THERE             
         BE    GETCPY02                                                         
         MVC   CPYKEY,KEY          NO - READ IT                                 
         GOTO1 DATAMGR,DMCB,DMREAD,ACCFIL,CPYRECD,CPYRECD                       
         BNE   GETCPYN                                                          
*                                                                               
GETCPY02 MVC   COMPADD1,SPACES                                                  
         MVC   COMPADD2,SPACES                                                  
         MVC   COMPADD3,SPACES                                                  
         MVC   COMPLOGO,SPACES                                                  
         MVC   COMPNAME,SPACES                                                  
         XC    COMPUSER,COMPUSER                                                
         LA    R1,CPYRECD                                                       
         AH    R1,DATADISP                                                      
GETCPY04 CLI   0(R1),0             TEST END OF RECORD                           
         BE    GETCPY12                                                         
*                                                                               
         USING CPYELD,R1                                                        
         CLI   CPYEL,CPYELQ        TEST COMPANY ELEMENT                         
         BNE   GETCPY06                                                         
         MVC   COMPLOGO,CPYLOGO    EXTRACT COMPANY LOGO                         
         MVC   COMPUSER,CPYUID     EXTRACT PRINCIPAL USER-ID                    
         B     GETCPY10                                                         
*                                                                               
         USING NAMELD,R1                                                        
GETCPY06 CLI   NAMEL,NAMELQ        TEST NAME ELEMENT                            
         BNE   GETCPY08                                                         
         LLC   RE,NAMLN                                                         
         SHI   RE,NAMLN1Q+1                                                     
         EXMVC RE,COMPNAME,NAMEREC                                              
         B     GETCPY10                                                         
*                                                                               
         USING ADRELD,R1                                                        
GETCPY08 CLI   ADREL,ADRELQ        TEST ADDRESS ELEMENT                         
         BNE   GETCPY10                                                         
         MVC   COMPADD1(L'ADRADD1),ADRADD1                                      
         MVI   COMPADD2,0          FORCE PRINTING OF ADDRESS LINES 2&3          
         MVI   COMPADD3,0                                                       
         CLI   ADRNUM,1                                                         
         BE    GETCPY10                                                         
         CLC   ADRADD2,SPACES                                                   
         BE    *+10                                                             
         MVC   COMPADD2(L'ADRADD2),ADRADD2                                      
         CLI   ADRNUM,2                                                         
         BE    GETCPY10                                                         
         MVC   COMPADD3(L'ADRADD3),ADRADD3                                      
*                                                                               
GETCPY10 LLC   R0,1(R1)            BUMP TO NEXT ELEMENT                         
         AR    R1,R0                                                            
         B     GETCPY04                                                         
*                                                                               
GETCPY12 OC    COMPUSER,COMPUSER   ENSURE PRINCIPAL ID SET                      
         BZ    GETCPYN                                                          
         NI    RUNINDS,255-RUNIREMO                                             
*                                                                               
         USING CTPREC,R2           R2=A(PROFILE RECORD)                         
         LA    R2,KEY                                                           
         XC    CTPKEY,CTPKEY                                                    
         MVI   CTPKTYP,CTPKTYPQ                                                 
         MVI   CTPKSYS,WKSYSACC                                                 
         MVC   CTPKPROG,RCPROG                                                  
         MVC   CTPKORIG,COMPUSER                                                
         GOTO1 DATAMGR,DMCB,DMREAD,CTFILE,CTPREC,CTPREC                         
         BNE   GETCPY16                                                         
*                                                                               
         SR    R0,R0                                                            
         LA    R1,CTPDATA          R1=A(FIRST ELEMENT)                          
         USING CTOCOD,R1                                                        
GETCPY14 IC    R0,CTOCOLEN         LOCATE OUTPUT TYPE CODE ELEMENT              
         AR    R1,R0                                                            
         CLI   CTOCOEL,0           TEST EOR                                     
         BE    GETCPY16                                                         
         CLI   CTOCOEL,CTOCOELQ    TEST OUTPUT CODE ELEMENT                     
         BNE   GETCPY14                                                         
         CLC   CTOCODE(L'REMLIT),REMLIT                                         
         BNE   GETCPY16                                                         
         OI    RUNINDS,RUNIREMO    SET COMPANY IS REMOTE                        
*                                                                               
GETCPY16 DS    0H                                                               
GETCPYY  CR    RE,RE                                                            
         B     GETCPYX                                                          
GETCPYN  CLI   *,0                                                              
*                                                                               
GETCPYX  B     XIT                                                              
         DROP  R1,R2                                                            
         EJECT                                                                  
***********************************************************************         
* BUILD TYPE FILTER TABLE FROM TYPE=SPP CARDS                                   
***********************************************************************         
VALTYP   LA    RF,FLTTYP           LOCATE FIRST FREE SLOT                       
         LA    R0,FLTTYPN                                                       
VALTYP02 CLI   0(RF),0                                                          
         BE    *+14                                                             
         LA    RF,L'FLTTYP(RF)                                                  
         BCT   R0,VALTYP02                                                      
         DC    H'0'                TOO MANY FILE FILTERS REQUESTED              
         MVC   0(L'FLTTYP,RF),0(R1)                                             
         BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
* SET SORT DUMP SWITCH ON                                                       
***********************************************************************         
VALDMP   CLC   0(3,R1),=C'KEY'     Dump sort key                                
         BNE   *+8                                                              
         OI    DUMPSW,DUMPSKEY                                                  
         CLC   0(3,R1),=C'REC'     Dump sort rec                                
         BNER  RE                                                               
         OI    DUMPSW,DUMPSREC                                                  
         BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
* Set SMF switch                                                                
***********************************************************************         
VALSMF   MVC   SMF,0(R1)           Force SMF on                                 
         BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
* Set LOWKEY to filter on                                                       
***********************************************************************         
VALLKY   ST    RE,SVRE                                                          
         LR    RF,R1                                                            
         OI    FLTKEY,FLTKLOW                                                   
         GOTO1 VDECODE,DMCB,(L'LOWKEY,(RF)),(X'00',LOWKEY)                      
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'00'                                                            
                                                                                
VALLKYX  L     RE,SVRE                                                          
         BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
* Set HIGHKEY to filter on                                                      
***********************************************************************         
VALHKY   ST    RE,SVRE                                                          
         LR    RF,R1                                                            
         OI    FLTKEY,FLTKHI                                                    
         GOTO1 VDECODE,DMCB,(L'HIGHKEY,(RF)),(X'00',HIGHKEY)                    
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'00'                                                            
                                                                                
VALHKYX  L     RE,SVRE                                                          
         BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
* PRINT A LINE                                                        *         
***********************************************************************         
PRINTIT  LR    R0,RE                                                            
         CLI   RCSUBPRG,0          TEST DDS SUMMARY                             
         BNE   *+12                                                             
         MVI   HEAD7+48,0          YES - FORCE HEAD7 TO PRINT                   
         B     PRINTIT2                                                         
         CLI   RCSUBPRG,1                                                       
         BNE   PRINTIT2                                                         
         MVC   HEAD4+48(L'COMPLOGO),COMPLOGO                                    
         MVC   HEAD5+48(L'COMPNAME),COMPNAME                                    
         MVC   HEAD6+48(L'COMPADD1),COMPADD1                                    
         MVC   HEAD7+48(L'COMPADD2),COMPADD2                                    
*                                                                               
PRINTIT2 GOTO1 ACREPORT                                                         
         LR    RE,R0                                                            
         BR    RE                                                               
***********************************************************************         
* HANDLE WORKER FILE ERRORS                                           *         
***********************************************************************         
WKERRS   MVC   P+1(22),=C'INVALID INPUT RECORD -'                               
         GOTO1 HEXOUT,WORK,WKIOLN,P+23,20,=C'TOG'                               
         GOTO1 LOGIO,WORK,1,(64,P+1)                                            
         GOTO1 PRINTIT                                                          
         MVC   P+1(22),=C'JOB HAS BEEN CANCELLED'                               
         GOTO1 LOGIO,WORK,1,(22,P+1)                                            
         GOTO1 PRINTIT                                                          
         ABEND 0,DUMP                                                           
XIT      XIT1  ,                                                                
         EJECT                                                                  
         LTORG                                                                  
*                                                                               
YES      EQU   C'Y'                                                             
NO       EQU   C'N'                                                             
EOT      EQU   X'FF'                                                            
                                                                                
AIN      DC    A(IN)                                                            
ABUFF    DC    A(BUFFALOC)                                                      
AIBUFF   DC    A(IBUFF)                                                         
AOBUFF   DC    A(OBUFF)                                                         
APRGTAB  DC    A(PRGTAB)                                                        
AUIDTAB  DC    A(UIDTAB)                                                        
AUIDNTRY DC    A(0)                                                             
ACHQTAB  DC    A(CHQTAB)                                                        
VGETLOGO DC    V(GETLOGO)                                                       
VPRNTBL  DC    V(PRNTBL)                                                        
VDECODE  DC    V(DECODE)                                                        
VSCANNER DC    V(SCANNER)                                                       
ASMFAREA DS    A                                                                
         EJECT                                                                  
TOTTAB   DS    0X                  ** TOTALS TABLE **                           
ADDDR    DC    PL8'0'                                                           
ADDCR    DC    PL8'0'                                                           
         DC    AL1(0)                                                           
         DC    AL2(AC@TPSTS-DICO)                                               
DELDR    DC    PL8'0'                                                           
DELCR    DC    PL8'0'                                                           
         DC    AL1(TOTTITST)                                                    
         DC    AL2(AC@TDELS-DICO)                                               
TALDR    DC    PL8'0'                                                           
TALCR    DC    PL8'0'                                                           
         DC    AL1(TOTTITST)                                                    
         DC    AL2(AC@TLPST-DICO)                                               
EXPDR    DC    PL8'0'                                                           
EXPCR    DC    PL8'0'                                                           
         DC    AL1(TOTTITST)                                                    
         DC    AL2(AC@EXPST-DICO)                                               
TOTDR    DC    PL8'0'                                                           
TOTCR    DC    PL8'0'                                                           
         DC    AL1(0)                                                           
         DC    AL2(AC@NETCF-DICO)                                               
TOTTABX  DC    AL1(TOTTEOTQ)                                                    
*                                                                               
PROGSORT DC    C'SM'               SORT/MERGE INPUT FILES                       
PROGODDS DC    C'OD'               OUTPUT WORKER FILES FOR UPDATE               
PROGKEEP DC    C'KE'               KEEP WORKER FILES                            
PROGACTV DC    C'UK'               ACTIVATE WORKER FILES                        
PROGHOLD DC    C'HO'               HOLD FILE IF ERROR                           
*                                                                               
OUTPUPDT DC    C'00'               UPDATE PROGRAM                               
OUTPKEEP DC    C'AU'               LIST OF KEEP FILES                           
OUTPDTBL DC    C'07'               DAILY TRIAL BALANCE                          
OUTPHOLD DC    C'SN'               FILE HELD BECAUSE OF ERRORS                  
*                                                                               
BC01     DC    C'BC01'                                                          
PRTCLOSE DC    C'CLOSE'                                                         
REMLIT   DC    C'REMOTE'                                                        
REPLIT   DC    C'**AUTO**'                                                      
*                                                                               
CTFILE   DC    C'CTFILE '                                                       
ACCFIL   DC    C'ACCOUNT'                                                       
ACCDIR   DC    C'ACCDIR '                                                       
ACCMST   DC    C'ACCMST '                                                       
ACCARC   DC    C'ACCARC '                                                       
DMGETREC DC    C'GETREC '                                                       
*                                                                               
BUFFSW   DC    X'00'               BUFFALO INDICTATOR BYTE                      
BUFSET   DC    C'SET '                                                          
BUFPUT   DC    C'PUT '                                                          
BUFSEQ   DC    C'SEQ '                                                          
BUFRDH   DC    C'HIGH'                                                          
*                                                                               
WKREAD   DC    C'READ   '                                                       
WKINDX   DC    C'INDEX  '                                                       
WKKEEP   DC    C'KEEP   '                                                       
WKADDR   DC    C'ADD    '                                                       
WKCLOS   DC    C'CLOSE  '                                                       
WKACTV   DC    C'UNKEEP '                                                       
WKHOLD   DC    C'HOLD   '                                                       
*                                                                               
PZERO    DC    P'0'                                                             
FILMULUK DC    C'2F'               UK FILM LEDGER                               
MOSTAB   DC    C'123456789ABC'                                                  
*                                                                               
SORTSW   DC    X'00'               SORTER INDICATOR BYTE                        
SORTCARD DC    C'SORT FIELDS=(5,42,A),FORMAT=BI,WORK=1 '                        
SORTTYPE DC    C'RECORD TYPE=V,LENGTH=(2048,,,100,150) '                        
SORTGET  DC    C'GET'                                                           
SORTPUT  DC    C'PUT'                                                           
SORTEND  DC    C'END'                                                           
*                                                                               
CONLOGO  DC    CL(L'LOGO1)'CONTROL'                                             
CONNAME  DC    CL(L'LOGONAME)'******* INTERNAL CONTROL *******'                 
CONADDR  DC    CL(L'LOGOADD)'******* DO NOT SEND OUT  *******'                  
*                                                                               
NOTBAL1  DC    C'** Worker file '                                               
NOTBAL2  DC    C'out of balance - Please check original documents **'           
*                                                                               
OUTID    DS    0XL16               ** OUTPUT WORKER FILE KEY **                 
OUTUSER  DS    0XL2                USER-ID NUMBER                               
*&&UK*&& DC    AL2(38)             ID=DDS                                       
*&&US*&& DC    AL2(17)             ID=SJR                                       
OUTSYST  DC    C'A'                SYSTEM ID                                    
OUTPROG  DS    CL2                 PROGRAM CODE                                 
OUTFILN  DS    CL1                 FILE (SYSTEM) ID                             
OUTPDAY  DS    PL1                 DAY (PWOS DD)                                
OUTTYPE  DC    AL1(WKTODDS)        ODDS FILE                                    
OUTXTRA  DC    XL1'00'             SYSTEM ID PART 2                             
OUTREST  DC    XL7'00'             REST OF WORKER KEY                           
*                                                                               
FLTTYPN  EQU   20                                                               
FLTTYP   DC    (FLTTYPN)XL4'00'    FILTER TYPE TABLE                            
         EJECT                                                                  
***********************************************************************         
* INPUT CARD OPTIONS                                                  *         
***********************************************************************         
OPTTAB   DS    0X                                                               
         DC    AL1(3),C'SMF=      ',AL2(VALSMF-ACSM02)                          
         DC    AL1(4),C'TYPE=     ',AL2(VALTYP-ACSM02)                          
         DC    AL1(5),C'DEBUG=    ',AL2(VALDMP-ACSM02)                          
         DC    AL1(6),C'LOWKEY=   ',AL2(VALLKY-ACSM02)                          
         DC    AL1(7),C'HIGHKEY=  ',AL2(VALHKY-ACSM02)                          
*                                                                               
OPTTABX  DC    AL1(OPTTEOTQ)                                                    
*                                                                               
OPTTABD  DSECT                     ** CARD OPTION TABLE **                      
OPTTEOTQ EQU   0                   END OF TABLE INDICATOR                       
OPTTKLEN DS    XL1                 LENGTH OF KEYWORD                            
OPTTKWRD DS    CL10                KEYWORD                                      
OPTTVDSP DS    AL2                 DISPLACEMENT TO VALIDATION ROUTINE           
OPTTABL  EQU   *-OPTTABD           LENGTH OF TABLE ENTRY                        
*                                                                               
         EJECT ,                                                                
***********************************************************************         
* ACSM02 CSECT                                                        *         
***********************************************************************         
ACSM02   CSECT                                                                  
         EJECT                                                                  
DICI     DS    0X                  ** DICTIONARY INPUT **                       
         DCDDL AC#TPSTS,20                                                      
         DCDDL AC#TDELS,20                                                      
         DCDDL AC#TLPST,20                                                      
         DCDDL AC#EXPST,20                                                      
         DCDDL AC#NETCF,20                                                      
         DCDDL AC#PGAPE,30                                                      
         DCDDL AC#PGA08,30                                                      
         DCDDL AC#PGADJ,30                                                      
         DCDDL AC#PGA21,30                                                      
         DCDDL AC#PGA25,30                                                      
         DCDDL AC#PGA90,30                                                      
         DCDDL AC#PGA91,30                                                      
         DCDDL AC#PGA98,30                                                      
         DCDDL AC#PGA55,30                                                      
         DCDDL AC#PGA5P,30                                                      
         DCDDL AC#PGAHP,30                                                      
         DCDDL AC#PGA23,30                                                      
         DCDDL AC#PGA05,30                                                      
         DCDDL AC#PGA07,30                                                      
         DCDDL AC#PGA09,30                                                      
         DCDDL AC#PGADK,30                                                      
         DCDDL AC#PGA47,30                                                      
         DCDDL AC#PGA8A,30                                                      
         DCDDL AC#PGAA1,30                                                      
         DCDDL AC#PGAA2,30                                                      
         DCDDL AC#PGAA8,30                                                      
         DCDDL AC#PGAAJ,30                                                      
         DCDDL AC#PGAAT,30                                                      
         DCDDL AC#PGACE,30                                                      
         DCDDL AC#PGADB,30                                                      
         DCDDL AC#PGADC,30                                                      
         DCDDL AC#PGADS,30                                                      
         DCDDL AC#PGADT,30                                                      
         DCDDL AC#PGADU,30                                                      
         DCDDL AC#PGADV,30                                                      
         DCDDL AC#PGADW,30                                                      
         DCDDL AC#PGAD1,30                                                      
         DCDDL AC#PGAD2,30                                                      
         DCDDL AC#PGAD4,30                                                      
         DCDDL AC#PGAD5,30                                                      
         DCDDL AC#PGAFA,30                                                      
         DCDDL AC#PGAF2,30                                                      
         DCDDL AC#PGAPA,30                                                      
         DCDDL AC#PGAT1,30                                                      
         DCDDL AC#PGAT4,30                                                      
         DCDDL AC#PGAT5,30                                                      
         DCDDL AC#PGF0C,30                                                      
         DCDDL AC#PGM09,30                                                      
         DCDDL AC#PGM10,30                                                      
         DCDDL AC#PGM14,30                                                      
         DCDDL AC#PGM15,30                                                      
         DCDDL AC#PGM50,30                                                      
         DCDDL AC#PGMD4,30                                                      
         DCDDL AC#PGMD7,30                                                      
         DCDDL AC#PGMAP,30                                                      
         DCDDL AC#PGMDA,30                                                      
         DCDDL AC#PGS54,30                                                      
         DCDDL AC#PGP54,30                                                      
         DCDDL AC#PGA54,30                                                      
         DCDDL AC#PGSBA,30                                                      
         DCDDL AC#PGPBA,30                                                      
         DCDDL AC#PGNBA,30                                                      
         DCDDL AC#PGS03,30                                                      
         DCDDL AC#PGP05,30                                                      
         DCDDL AC#PGAD0,30                                                      
         DCDDL AC#VENSF,30                                                      
         DCDDL AC#VENSP,30                                                      
         DCDDL AC#VENSQ,30                                                      
         DCDDL AC#VENSS,30                                                      
         DCDDL AC#VENST,30                                                      
         DCDDL AC#VENSU,30                                                      
         DCDDL AC#VENSV,30                                                      
         DCDDL AC#VENSW,30                                                      
         DCDDL AC#VENSX,30                                                      
         DCDDL AC#VENSY,30                                                      
         DCDDL AC#PGPMX,30                                                      
         DCDDL AC#PGPMY,30                                                      
         DCDDL AC#PGSMV,30                                                      
         DCDDL AC#PGSMX,30                                                      
         DCDDL AC#PGSMY,30                                                      
         DCDDL AC#PGNMX,30                                                      
         DCDDL AC#PGNMY,30                                                      
         DCDDL AC#PGA27,30                                                      
         DCDDL AC#PGA29,30                                                      
         DCDDL AC#PGARU,30                                                      
         DCDDL AC#PGASA,30                                                      
         DCDDL AC#PGTBI,30                                                      
         DCDDL AC#PGTCK,30                                                      
         DCDDL AC#PGTCN,30                                                      
         DCDDL AC#PGMH4,30                                                      
         DCDDL AC#PGTEC,30                                                      
         DCDDL AC#PGAIO,30                                                      
         DCDDL AC#PGTPK,30                                                      
         DCDDL AC#PGTVB,30                                                      
DICIX    DC    AL1(0)                                                           
         SPACE 2                                                                
DICO     DS    0C                  ** DICTIONARY OUTPUT **                      
         DSDDL                                                                  
DICOX    DS    0C                                                               
         EJECT                                                                  
WORKD    DSECT                     ** LOCAL WORKING STORAGE **                  
PARA     DS    6F                                                               
PARMSMF  DS    6F                                                               
SVRE     DS    A                                                                
SE#      DS    H                                                                
OTHERSE# DS    H                                                                
FILEJU   DS    XL4                 Worker file date full juilian                
SYSNME   DS    CL5                                                              
RUNINDS  DS    XL1                 RUN CONTROL INDICATORS                       
RUNITEST EQU   X'80'               .  TEST=YES                                  
RUNIWRNO EQU   X'40'               .  WRITE=NO                                  
RUNINOLO EQU   X'20'               .  NOLOGOS                                   
RUNIREMO EQU   X'10'               .  REMOTE COMPANY                            
RUNISCOA EQU   X'08'               .  SUPPRESS COMPANY ANALYSIS                 
IDRECS   DS    PL8                 POSTING FILE TOTAL RECORDS                   
IDCASH   DS    PL8                 POSTING FILE TOTAL CASH                      
TODAY0   DS    0CL6                TODAY'S DATE (EBCDIC YYMMDD)                 
TODAY0YY DS    CL2                                                              
TODAY0MM DS    CL2                                                              
TODAY0DD DS    CL2                                                              
TODAY1   DS    0PL3                TODAY'S DATE (PACKED YYMMDD)                 
TODAY1YY DS    PL1                                                              
TODAY1MM DS    PL1                                                              
TODAY1DD DS    PL1                                                              
LASTCPY  DS    XL1                 LAST TIME COMPANY CODE                       
THISCPY  DS    XL1                 THIS TIME COMPANY CODE                       
SENO     DS    XL1                 ACCOUNT SYSTEM SE NUMBER                     
ODDDAYS  DS    XL4                 EXTRA ACODDS DAYS                            
OUTCNT   DS    H                   N'ENTRIES IN OUTPUT RECORD                   
ONEFILE  DS    XL16                WORKER ID FOR SINGLE FILE INPUT              
FLTKEY   DS    X                                                                
FLTKLOW  EQU   X'80'                                                            
FLTKHI   EQU   X'40'                                                            
                                                                                
DUMPSW   DS    X                   Dump switch                                  
DUMPSKEY EQU   X'80'               .  Sort key                                  
DUMPSREC EQU   X'40'               .  Sort record                               
                                                                                
SMF      DS    CL1                 Yes / No (default)                           
IS_ACSM  DS    CL1                 Yes / No (Is it pure ACSM)                   
                                                                                
LOWKEY   DS    XL42                                                             
HIGHKEY  DS    XL42                                                             
*                                                                               
WKID     DS    0XL16               ** WORKER FILE KEY **                        
WKKEY    DS    0XL9                WORKER KEY                                   
WKUSER   DS    XL2                 USER-ID NUMBER                               
WKSYSPRG DS    0CL3                                                             
WKSYS    DS    CL1                 SYSTEM CODE                                  
WKSYSACC EQU   C'A'                ACCOUNTING                                   
WKSYSMED EQU   C'M'                MEDIA                                        
WKSYSNET EQU   C'N'                NETPAK                                       
WKSYSPRT EQU   C'P'                SPOT                                         
WKSYSSPT EQU   C'S'                PRINT                                        
WKSYSFEE EQU   C'F'                ARTISTE FEES                                 
WKSYSTAL EQU   C'T'                TALENT                                       
WKPRG    DS    CL2                 PROGRAM ID                                   
WKSUB    DS    XL1                 SUB-PROGRAM/LEDGER CODE                      
WKCPY    EQU   WKSUB               COMPANY CODE (SOME WORKER FILES)             
WKDAY    DS    PL1                 DAY ADDED (PWOS DD)                          
WKTYPE   DS    CL1                 FILE TYPE                                    
WKTPOST  EQU   C'P'                POSTING FILE                                 
WKTODDS  EQU   C'O'                ACODDS FILE                                  
WKTCHQS  EQU   C'C'                CHEQUE FILE                                  
WKEXTRA  DS    XL1                 EXTRA                                        
         DS    XL1                 N/D                                          
WKSEQN   DS    XL2                 FILE SEQUENCE NUMBER                         
WKSTAT   DS    XL1                 WORKER STATUS                                
WKSHOLD  EQU   X'40'               STATUS HOLD                                  
WKSKEEP  EQU   X'08'               STATUS KEEP                                  
         ORG   WKID+L'WKID                                                      
*                                                                               
COMPUSER DS    XL2                 USER-ID NUMBER                               
COMPLOGO DS    CL(L'CPYLOGO)       COMPANY LOGO                                 
COMPNAME DS    CL(L'LOGONAME)      COMPANY NAME                                 
COMPADD1 DS    CL(L'LOGOADD)       COMPANY ADDRESS LINE 1                       
COMPADD2 DS    CL(L'LOGOADD2)      COMPANY ADDRESS LINE 2                       
COMPADD3 DS    CL(L'LOGOADD3)      COMPANY ADDRESS LINE 3                       
*                                                                               
BUFREC   DS    0X                  ** BUFFALO RECORD **                         
BUFKEY   DS    0X                                                               
BUFRTYPE DS    XL1                 RECORD TYPE                                  
BUFRTDDS EQU   1                   .  DDS     summary                           
BUFRTCPY EQU   2                   .  Company summary                           
BUFRTSMF EQU   3                   .  SMF     summary                           
BUFRKSEQ DS    CL3                 GIVES DDS SECTION IN REPORT SEQUENCE         
BUFRCPY  DS    XL1                 COMPANY CODE                                 
BUFRSYSP DS    CL3                 SYSTEM/PROGRAM CODE                          
BUFRSPRG DS    XL1                 LEDGER/MEDIA FILE NUMBER OR ZERO             
BUFRXTRA DS    XL1                 SPOT/NET/PRINT FILE NUMBER OF ZERO           
BUFRUSER DS    XL2                 USER-ID NUMBER                               
BUFRDAY  DS    PL1                 DAY NUMBER (PWOS DD)                         
BUFROSE# DS    XL2                 Used currently for balancing (SMF)           
BUFRSEQ# DS    XL2                 Worker file sequence number  (SMF)           
BUFKEYL  EQU   *-BUFKEY                                                         
BUFRCODE DS    CL10                USER-ID CODE                                 
BUFRDR   DS    PL8                 TOTAL DEBITS                                 
BUFRCR   DS    PL8                 TOTAL CREDITS                                
BUFRPONE DS    PL8                 FORCE ZERO POSTINGS TO PRINT                 
BUFRECL  EQU   *-BUFREC                                                         
*                                                                               
OUTIO    DS    ((OUTMAXN*OUTLNQ)+5)X                                            
OUTIOL   EQU   *-OUTIO                                                          
         ORG   OUTIO                                                            
ODDREC   DS    0X                  ** ACODDS RECORD LAYOUT **                   
ODDRLEN  DS    XL2                 RECORD LENGTH                                
         DS    XL2                 N/D                                          
ODDRCOMP DS    XL1                 COMPANY CODE                                 
ODDRUSER DS    XL2                 USER-ID                                      
ODDRNAME DS    CL33                COMPANY NAME                                 
         DS    XL8                 N/D                                          
ODDRECL  EQU   *-ODDREC            LENGTH OF RECORD                             
*                                                                               
SORTRLEN DS    H                   SORTER RECORD LENGTH                         
         DS    H                                                                
SORTKEY  DS    XL(L'ACCKEY)        SORT KEY BUILT HERE                          
SORTKEYL EQU   *-SORTRLEN                                                       
*                                                                               
WKIOLN   DS    H                   WORKER RECORD I/O AREA                       
         DS    H                                                                
WKIO     DS    2000X                                                            
*                                                                               
IOKEY    DS    XL64                KEY                                          
IOWORK   DS    XL64                DMWORK                                       
IO       DS    XL2048              I/O AREA                                     
         EJECT                                                                  
* ACREPWORKD                                                                    
         PRINT OFF                                                              
       ++INCLUDE ACREPWORKD                                                     
         ORG   P                                                                
LINED    DS    0CL(L'P)            ** SORT/MERGE LINE DEFINITION **             
LINBXL   DS    CL1                                                              
LINFILE  DS    CL67                                                             
LINBX1   DS    CL1                                                              
LINDR    DS    CL14                                                             
LINBX2   DS    CL1                                                              
LINCR    DS    CL14                                                             
LINBXR   DS    CL1                                                              
         PRINT ON                                                               
* ACGENMODES                                                                    
         PRINT OFF                                                              
       ++INCLUDE ACGENMODES                                                     
         PRINT ON                                                               
* ACDDEQUS                                                                      
         PRINT OFF                                                              
       ++INCLUDE ACDDEQUS                                                       
         PRINT ON                                                               
* DDMASTD                                                                       
         PRINT OFF                                                              
       ++INCLUDE DDMASTD                                                        
         PRINT ON                                                               
* DDDICTATED                                                                    
         PRINT OFF                                                              
       ++INCLUDE DDDICTATED                                                     
         PRINT ON                                                               
* DDSYSELD                                                                      
         PRINT OFF                                                              
       ++INCLUDE DDSYSELD                                                       
         PRINT ON                                                               
* DDLOGOD                                                                       
         PRINT OFF                                                              
       ++INCLUDE DDLOGOD                                                        
         PRINT ON                                                               
* DDREMOTED                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDREMOTED                                                      
         PRINT ON                                                               
* DDREPXTRAD                                                                    
         PRINT OFF                                                              
       ++INCLUDE DDREPXTRAD                                                     
         PRINT ON                                                               
* ACGENFILE                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACGENFILE                                                      
         PRINT ON                                                               
* DDCTRYEQUS                                                                    
         PRINT OFF                                                              
       ++INCLUDE DDCTRYEQUS                                                     
CTRYNOT  EQU   X'80'                                                            
CTRYALL  EQU   X'00'                                                            
         PRINT ON                                                               
* ACGENPOST                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACGENPOST                                                      
         PRINT ON                                                               
* ACSMFBLK                                                                      
         PRINT OFF                                                              
       ++INCLUDE ACSMFBLK                                                       
         PRINT ON                                                               
* CTGENFILE                                                                     
         PRINT OFF                                                              
       ++INCLUDE CTGENFILE                                                      
         PRINT ON                                                               
* DDBUFFALOD                                                                    
         PRINT OFF                                                              
       ++INCLUDE DDBUFFALOD                                                     
         PRINT ON                                                               
* DDBIGBOX                                                                      
         PRINT OFF                                                              
       ++INCLUDE DDBIGBOX                                                       
         PRINT ON                                                               
* DDCOMFACS                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDCOMFACS                                                      
         PRINT ON                                                               
* FAUTL                                                                         
         PRINT OFF                                                              
       ++INCLUDE FAUTL                                                          
         PRINT ON                                                               
* FASSBOFF                                                                      
         PRINT OFF                                                              
SSBOFFD  DSECT                                                                  
       ++INCLUDE FASSBOFF                                                       
         PRINT ON                                                               
* DMDDNAMED                                                                     
DDNAMED  DSECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE DMDDNAMED                                                      
         PRINT ON                                                               
* DDSMFFBAL                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDSMFFBAL                                                      
         ORG   SMFBTXT1                                                         
SMFBSENM DS    CL5                                                              
         ORG   SMFBTXT2                                                         
SMFBTYPO DS    CL1                 L=Ledger, S=SE system                        
SMFBLDGR DS    0CL2                Unit/Ledger                                  
SMFBOSE# DS    CL2                 Other SE# (SPOT/NET/PRINT)                   
SMFBOSEN DS    CL5                 Other SE name                                
SMFBINFO DS    CL4                 Info for SAS                                 
         ORG                                                                    
         PRINT ON                                                               
         EJECT                                                                  
PRGTABD  DSECT                     ** PROGRAM TABLE DSECT **                    
PRGTSP   DS    0CL3                                                             
PRGTSYS  DS    CL1                 SYSTEM CODE                                  
PRGTEOTQ EQU   255                 END OF TABLE INDICATOR                       
PRGTPRG  DS    CL2                 PROGRAM ID                                   
PRGTNAML EQU   30                  PROGRAM NAME LENGTH                          
PRGTDISP DS    AL2                 DISPLACEMENT TO NAME IN DICO                 
PRGTIND1 DS    XL1                 PROGRAM INDICATORS BYTE 1                    
PRGTITRL EQU   X'80'               PROCESS TRAILER FOR TOTALS                   
PRGTINOC EQU   X'40'               DON'T CHECK TRAILER TOTALS                   
PRGTIJRN EQU   X'20'               DAILY JOURNAL PROGRAM                        
PRGTICLR EQU   X'10'               SPOT/PRINT DAILY CLEARANCE                   
PRGTICHQ EQU   X'08'               CHEQUE PROGRAM                               
PRGTIPOF EQU   X'04'               PEEL-OFF PROGRAM                             
PRGTITPY EQU   X'02'               TALENT PAYMENTS                              
PRGTICEX EQU   X'01'               COKE EXPENDITURE                             
PRGTIND2 DS    XL1                 PROGRAM INDICATORS BYTE 2                    
PRGTINOT EQU   X'80'               DON'T PROCESS FILES FOR TODAY                
PRGTICPY EQU   X'40'               COMPANY CODE FROM WKCPY                      
PRGTABL  EQU   *-PRGTABD                                                        
         SPACE 1                                                                
UIDTABD  DSECT                     ** USER-ID TABLE **                          
UIDTUSER DS    XL2                 USER-ID NUMBER                               
UIDTCODE DS    CL10                USER-ID CODE                                 
UIDTCPY  DS    XL1                 COMPANY CODE                                 
UIDTSENO DS    XL1                 ACCOUNT SYSTEM SE NUMBER                     
UIDTLANG DS    XL1                 LANGUAGE CODE                                
UIDTALPH DS    CL2                 ALPHA-ID CODE                                
UIDTABL  EQU   *-UIDTABD                                                        
UIDTABMX EQU   1024                MAXIMUM NUMBER OF USER-ID'S                  
         SPACE 1                                                                
CHQTABD  DSECT                     ** CHEQUE LEDGER TABLE **                    
CHQTEOTQ EQU   255                 END OF TABLE INDICATOR                       
CHQTLEDG DS    CL1                 LEDGER CODE                                  
CHQTDSCL EQU   30                  LENGTH OF LEDGER DESCRIPTION                 
CHQTDISP DS    AL2                 DISPLACEMENT TO DESCRIPTION IN DICO          
CHQTABL  EQU   *-CHQTABD                                                        
         SPACE 1                                                                
TOTTABD  DSECT                     ** TOTALS TABLE **                           
TOTTDR   DS    PL8                 DEBIT AMOUNT                                 
TOTTEOTQ EQU   255                 END OF TABLE INDICATOR                       
TOTTCR   DS    PL8                 CREDIT AMOUNT                                
TOTTINDS DS    XL1                 INDICATORS                                   
TOTTITST EQU   X'80'               DON'T PRINT ZERO AMOUNTS                     
TOTTDSCL EQU   20                  LENGTH OF TOTAL DESCRIPTION                  
TOTTDISP DS    AL2                 DISPLACEMENT TO PHRASE IN DICO               
TOTTABL  EQU   *-TOTTABD                                                        
         SPACE 1                                                                
OUTELD   DSECT                     ** OUTPUT ELEMENT **                         
OUTEL    DS    XL1                 ELEMENT CODE                                 
OUTELQ   EQU   X'01'                                                            
OUTLN    DS    XL1                 ELEMENT LENGTH                               
OUTKEY   DS    XL(L'OUTID)         WORKER FILE KEY                              
OUTLNQ   EQU   *-OUTELD                                                         
OUTMAXN  EQU   20                  MAXIMUM N'OUTELS ON RECORD                   
         EJECT                                                                  
ACSM02   CSECT                                                                  
CHQTAB   DS    0XL24               ** CHEQUE LEDGERS TABLE **                   
*&&UK                                                                           
         DC    C'F',AL2(AC@VENSF-DICO)                                          
         DC    C'T',AL2(AC@VENST-DICO)                                          
         DC    C'V',AL2(AC@VENSV-DICO)                                          
         DC    C'X',AL2(AC@VENSX-DICO)                                          
*&&                                                                             
*&&US                                                                           
         DC    C'P',AL2(AC@VENSP-DICO)                                          
         DC    C'Q',AL2(AC@VENSQ-DICO)                                          
         DC    C'S',AL2(AC@VENSS-DICO)                                          
         DC    C'T',AL2(AC@VENST-DICO)                                          
         DC    C'U',AL2(AC@VENSU-DICO)                                          
         DC    C'V',AL2(AC@VENSV-DICO)                                          
         DC    C'W',AL2(AC@VENSW-DICO)                                          
         DC    C'X',AL2(AC@VENSX-DICO)                                          
         DC    C'Y',AL2(AC@VENSY-DICO)                                          
*&&                                                                             
CHQTABX  DC    AL1(CHQTEOTQ)                                                    
         EJECT                                                                  
PRGTAB   DS    0X                  ** PROGRAM TYPE TABLE **                     
*                                                                               
         DC    AL1(WKSYSACC),C'PE',AL2(AC@PGAPE-DICO)                           
         DC    AL1(PRGTINOC,PRGTINOT)                                           
*                                                                               
         DC    AL1(WKSYSACC),C'08',AL2(AC@PGA08-DICO)                           
         DC    AL1(PRGTINOC+PRGTIJRN,0)                                         
*                                                                               
         DC    AL1(WKSYSACC),C'09',AL2(AC@PGA09-DICO)                           
         DC    AL1(PRGTITRL,PRGTICPY)                                           
*                                                                               
         DC    AL1(WKSYSACC),C'DJ',AL2(AC@PGADJ-DICO)                           
         DC    AL1(PRGTINOC+PRGTIJRN,0)                                         
*                                                                               
         DC    AL1(WKSYSACC),C'DK',AL2(AC@PGADK-DICO)                           
         DC    AL1(PRGTITRL,PRGTICPY)                                           
*                                                                               
         DC    AL1(WKSYSACC),C'21',AL2(AC@PGA21-DICO)                           
         DC    AL1(0,0)                                                         
*                                                                               
         DC    AL1(WKSYSACC),C'25',AL2(AC@PGA25-DICO)                           
         DC    AL1(0,0)                                                         
*                                                                               
         DC    AL1(WKSYSACC),C'90',AL2(AC@PGA90-DICO)                           
         DC    AL1(0,0)                                                         
*                                                                               
         DC    AL1(WKSYSACC),C'91',AL2(AC@PGA91-DICO)                           
         DC    AL1(0,0)                                                         
*                                                                               
         DC    AL1(WKSYSACC),C'98',AL2(AC@PGA98-DICO)                           
         DC    AL1(PRGTITRL+PRGTIPOF,PRGTICPY)                                  
*                                                                               
         DC    AL1(WKSYSACC),C'54',AL2(AC@PGA54-DICO)                           
         DC    AL1(PRGTITRL+PRGTICLR,PRGTICPY)                                  
*                                                                               
         DC    AL1(WKSYSACC),C'55',AL2(AC@PGA55-DICO)                           
         DC    AL1(PRGTICHQ,0)                                                  
*&&US                                                                           
         DC    AL1(WKSYSACC),C'23',AL2(AC@PGA23-DICO)                           
         DC    AL1(0,0)                                                         
*                                                                               
         DC    AL1(WKSYSACC),C'05',AL2(AC@PGA05-DICO)                           
         DC    AL1(PRGTITRL,PRGTICPY)                                           
*                                                                               
         DC    AL1(WKSYSACC),C'07',AL2(AC@PGA07-DICO)                           
         DC    AL1(PRGTITRL+PRGTITPY,PRGTICPY)                                  
*                                                                               
         DC    AL1(WKSYSACC),C'47',AL2(AC@PGA47-DICO)                           
         DC    AL1(0,0)                                                         
*                                                                               
         DC    AL1(WKSYSACC),C'A1',AL2(AC@PGAA1-DICO)                           
         DC    AL1(0,0)                                                         
*                                                                               
         DC    AL1(WKSYSACC),C'A8',AL2(AC@PGAA8-DICO)                           
         DC    AL1(0,0)                                                         
*                                                                               
         DC    AL1(WKSYSACC),C'CE',AL2(AC@PGACE-DICO)                           
         DC    AL1(PRGTITRL,PRGTICPY)                                           
*                                                                               
         DC    AL1(WKSYSACC),C'FA',AL2(AC@PGAFA-DICO)                           
         DC    AL1(0,0)                                                         
*                                                                               
         DC    AL1(WKSYSACC),C'PA',AL2(AC@PGAPA-DICO)                           
         DC    AL1(0,0)                                                         
*                                                                               
         DC    AL1(WKSYSACC),C'T4',AL2(AC@PGAT4-DICO)                           
         DC    AL1(0,PRGTINOT)                                                  
*                                                                               
         DC    AL1(WKSYSACC),C'T5',AL2(AC@PGAT5-DICO)                           
         DC    AL1(0,0)                                                         
*                                                                               
         DC    AL1(WKSYSSPT),C'54',AL2(AC@PGS54-DICO)                           
         DC    AL1(PRGTICLR,0)                                                  
*                                                                               
         DC    AL1(WKSYSPRT),C'54',AL2(AC@PGP54-DICO)                           
         DC    AL1(PRGTICLR,0)                                                  
*                                                                               
         DC    AL1(WKSYSPRT),C'BA',AL2(AC@PGPBA-DICO)                           
         DC    AL1(0,0)                                                         
*                                                                               
         DC    AL1(WKSYSSPT),C'BA',AL2(AC@PGSBA-DICO)                           
         DC    AL1(0,0)                                                         
*                                                                               
         DC    AL1(WKSYSNET),C'BA',AL2(AC@PGNBA-DICO)                           
         DC    AL1(0,0)                                                         
*                                                                               
         DC    AL1(WKSYSSPT),C'03',AL2(AC@PGS03-DICO)                           
         DC    AL1(0,0)                                                         
*                                                                               
         DC    AL1(WKSYSPRT),C'05',AL2(AC@PGP05-DICO)                           
         DC    AL1(0,0)                                                         
*                                                                               
         DC    AL1(WKSYSACC),C'8A',AL2(AC@PGA8A-DICO)                           
         DC    AL1(0,0)                                                         
*                                                                               
         DC    AL1(WKSYSACC),C'A2',AL2(AC@PGAA2-DICO)                           
         DC    AL1(0,0)                                                         
*                                                                               
         DC    AL1(WKSYSACC),C'AJ',AL2(AC@PGAAJ-DICO)                           
         DC    AL1(0,0)                                                         
*                                                                               
         DC    AL1(WKSYSACC),C'AT',AL2(AC@PGAAT-DICO)                           
         DC    AL1(0,0)                                                         
*                                                                               
         DC    AL1(WKSYSPRT),C'ML',AL2(AC@PGPMX-DICO)                           
         DC    AL1(0,PRGTINOT)                                                  
*                                                                               
         DC    AL1(WKSYSPRT),C'MX',AL2(AC@PGPMX-DICO)                           
         DC    AL1(0,PRGTINOT)                                                  
*                                                                               
         DC    AL1(WKSYSPRT),C'MY',AL2(AC@PGPMY-DICO)                           
         DC    AL1(0,PRGTINOT)                                                  
*                                                                               
         DC    AL1(WKSYSSPT),C'ML',AL2(AC@PGSMX-DICO)                           
         DC    AL1(0,PRGTINOT)                                                  
*                                                                               
         DC    AL1(WKSYSSPT),C'MV',AL2(AC@PGSMV-DICO)                           
         DC    AL1(0,0)                                                         
*                                                                               
         DC    AL1(WKSYSSPT),C'MX',AL2(AC@PGSMX-DICO)                           
         DC    AL1(0,PRGTINOT)                                                  
*                                                                               
         DC    AL1(WKSYSSPT),C'MY',AL2(AC@PGSMY-DICO)                           
         DC    AL1(0,PRGTINOT)                                                  
*                                                                               
         DC    AL1(WKSYSNET),C'ML',AL2(AC@PGNMX-DICO)                           
         DC    AL1(0,PRGTINOT)                                                  
*                                                                               
         DC    AL1(WKSYSNET),C'MX',AL2(AC@PGNMX-DICO)                           
         DC    AL1(0,PRGTINOT)                                                  
*                                                                               
         DC    AL1(WKSYSNET),C'MY',AL2(AC@PGNMY-DICO)                           
         DC    AL1(0,PRGTINOT)                                                  
*                                                                               
         DC    AL1(WKSYSACC),C'27',AL2(AC@PGA27-DICO)                           
         DC    AL1(0,0)                                                         
*                                                                               
         DC    AL1(WKSYSACC),C'29',AL2(AC@PGA29-DICO)                           
         DC    AL1(0,0)                                                         
*                                                                               
         DC    AL1(WKSYSACC),C'RU',AL2(AC@PGARU-DICO)                           
         DC    AL1(0,0)                                                         
*                                                                               
         DC    AL1(WKSYSACC),C'SA',AL2(AC@PGASA-DICO)                           
         DC    AL1(0,0)                                                         
*                                                                               
         DC    AL1(WKSYSTAL),C'BI',AL2(AC@PGTBI-DICO)                           
         DC    AL1(0,PRGTINOT)                                                  
*                                                                               
         DC    AL1(WKSYSTAL),C'PB',AL2(AC@PGTBI-DICO)                           
         DC    AL1(0,PRGTINOT)                                                  
*                                                                               
         DC    AL1(WKSYSTAL),C'CK',AL2(AC@PGTCK-DICO)                           
         DC    AL1(0,PRGTINOT)                                                  
*                                                                               
         DC    AL1(WKSYSTAL),C'CU',AL2(AC@PGTCK-DICO)                           
         DC    AL1(0,0)                                                         
*                                                                               
         DC    AL1(WKSYSTAL),C'PC',AL2(AC@PGTCK-DICO)                           
         DC    AL1(0,PRGTINOT)                                                  
*                                                                               
         DC    AL1(WKSYSTAL),C'CN',AL2(AC@PGTCN-DICO)                           
         DC    AL1(0,PRGTINOT)                                                  
*                                                                               
         DC    AL1(WKSYSTAL),C'EC',AL2(AC@PGTEC-DICO)                           
         DC    AL1(0,PRGTINOT)                                                  
*                                                                               
         DC    AL1(WKSYSTAL),C'PK',AL2(AC@PGTPK-DICO)                           
         DC    AL1(0,PRGTINOT)                                                  
*                                                                               
         DC    AL1(WKSYSTAL),C'VB',AL2(AC@PGTVB-DICO)                           
         DC    AL1(0,PRGTINOT)                                                  
*                                                                               
         DC    AL1(WKSYSACC),C'IO',AL2(AC@PGAIO-DICO)                           
         DC    AL1(0,0)                                                         
*                                                                               
         DC    AL1(WKSYSTAL),C'P1',AL2(AC@PGTCK-DICO)                           
         DC    AL1(0,0)                                                         
*                                                                               
         DC    AL1(WKSYSTAL),C'P2',AL2(AC@PGTPK-DICO)                           
         DC    AL1(0,0)                                                         
*                                                                               
*&&                                                                             
*&&UK                                                                           
         DC    AL1(WKSYSACC),C'5P',AL2(AC@PGA5P-DICO)                           
         DC    AL1(PRGTICHQ,0)                                                  
*                                                                               
         DC    AL1(WKSYSACC),C'HP',AL2(AC@PGAHP-DICO)                           
         DC    AL1(PRGTICHQ,0)                                                  
*                                                                               
         DC    AL1(WKSYSACC),C'AU',AL2(AC@PGA09-DICO)                           
         DC    AL1(PRGTITRL,PRGTICPY)                                           
*                                                                               
         DC    AL1(WKSYSACC),C'F2',AL2(AC@PGAF2-DICO)                           
         DC    AL1(0,0)                                                         
*                                                                               
         DC    AL1(WKSYSACC),C'T1',AL2(AC@PGAT1-DICO)                           
         DC    AL1(0,0)                                                         
*                                                                               
         DC    AL1(WKSYSFEE),C'0C',AL2(AC@PGF0C-DICO)                           
         DC    AL1(0,0)                                                         
*                                                                               
         DC    AL1(WKSYSMED),C'09',AL2(AC@PGM09-DICO)                           
         DC    AL1(0,0)                                                         
*                                                                               
         DC    AL1(WKSYSMED),C'10',AL2(AC@PGM10-DICO)                           
         DC    AL1(0,0)                                                         
*                                                                               
         DC    AL1(WKSYSMED),C'14',AL2(AC@PGM14-DICO)                           
         DC    AL1(0,0)                                                         
*                                                                               
         DC    AL1(WKSYSMED),C'15',AL2(AC@PGM15-DICO)                           
         DC    AL1(0,0)                                                         
*                                                                               
         DC    AL1(WKSYSMED),C'50',AL2(AC@PGM50-DICO)                           
         DC    AL1(0,0)                                                         
*                                                                               
         DC    AL1(WKSYSACC),C'DS',AL2(AC@PGADS-DICO)                           
         DC    AL1(0,0)                                                         
*                                                                               
         DC    AL1(WKSYSACC),C'DT',AL2(AC@PGADT-DICO)                           
         DC    AL1(0,0)                                                         
*                                                                               
         DC    AL1(WKSYSACC),C'D2',AL2(AC@PGAD2-DICO)                           
         DC    AL1(0,0)                                                         
*                                                                               
         DC    AL1(WKSYSACC),C'D4',AL2(AC@PGAD4-DICO)                           
         DC    AL1(0,0)                                                         
*                                                                               
         DC    AL1(WKSYSACC),C'D5',AL2(AC@PGAD5-DICO)                           
         DC    AL1(0,0)                                                         
*                                                                               
         DC    AL1(WKSYSACC),C'DU',AL2(AC@PGADU-DICO)                           
         DC    AL1(0,0)                                                         
*                                                                               
         DC    AL1(WKSYSACC),C'DV',AL2(AC@PGADV-DICO)                           
         DC    AL1(0,0)                                                         
*                                                                               
         DC    AL1(WKSYSMED),C'D4',AL2(AC@PGAD4-DICO)                           
         DC    AL1(0,0)                                                         
*                                                                               
         DC    AL1(WKSYSMED),C'DA',AL2(AC@PGMDA-DICO)                           
         DC    AL1(0,0)                                                         
*                                                                               
         DC    AL1(WKSYSMED),C'D7',AL2(AC@PGMD7-DICO)                           
         DC    AL1(0,0)                                                         
*                                                                               
         DC    AL1(WKSYSMED),C'AP',AL2(AC@PGMAP-DICO)                           
         DC    AL1(0,0)                                                         
*                                                                               
         DC    AL1(WKSYSACC),C'D1',AL2(AC@PGAD1-DICO)                           
         DC    AL1(0,0)                                                         
*                                                                               
         DC    AL1(WKSYSACC),C'D!',AL2(AC@PGADW-DICO)                           
         DC    AL1(0,0)                                                         
*                                                                               
         DC    AL1(WKSYSACC),C'DB',AL2(AC@PGADB-DICO)                           
         DC    AL1(0,0)                                                         
*                                                                               
         DC    AL1(WKSYSACC),C'DC',AL2(AC@PGADC-DICO)                           
         DC    AL1(PRGTICHQ,0)                                                  
*                                                                               
         DC    AL1(WKSYSACC),C'D0',AL2(AC@PGAD0-DICO)                           
         DC    AL1(0,0)                                                         
*                                                                               
         DC    AL1(WKSYSMED),C'H4',AL2(AC@PGMH4-DICO)                           
         DC    AL1(0,0)                                                         
*&&                                                                             
PRGTABX  DC    AL1(PRGTEOTQ)                                                    
         EJECT                                                                  
***********************************************************************         
* File and tables                                                               
***********************************************************************         
IN       DCB   DDNAME=IN,DSORG=PS,RECFM=VB,LRECL=1992,BLKSIZE=16000,   X        
               MACRF=PM                                                         
                                                                                
UIDTAB   DC    (UIDTABMX)XL(UIDTABL)'00'                                        
                                                                                
IBUFF    DC    6144X'00'                                                        
                                                                                
OBUFF    DC    6144X'00'                                                        
                                                                                
         BUFF  LINES=300,ROWS=1,COLUMNS=3,FLAVOR=PACKED,               X        
               KEYLIST=(17,A),COMMENT=10                                        
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'031ACREPSM02 06/04/20'                                      
         END                                                                    
