*          DATA SET SRUPD00    AT LEVEL 101 AS OF 10/16/18                      
*PROCESS USING(WARN(15))                                                        
*PHASE T10D00C                                                                  
*                                                                               
***********************************************************************         
*        HISTORY                                                                
***********************************************************************         
* WHO  DDMMMYY LVL JIRA       DESCRIPTION                                       
* **** ******* *** ********** *****************************************         
* CPAT 17OCT18 101 SPEC-18149 SKIP DATA TO AVOID 21 BILLING DUMPS               
*                             ACTRECD ACTRSTAT                                  
*                             RSTELD  RSTSTAT1/2/3                              
*                             RACELD                                            
*                             RAPRECD RAPKDATE, RAPKTIME                        
*                                                                               
* HWON 17OCT18 101 SPEC-22487 FIX SOA EXIT END OF RECORD CHECK                  
*                             ADD SOA SKIP LOGIC TO AVOID DUMPS                 
***********************************************************************         
*                                                                               
         TITLE '$UPD - FACWRK RECOVERY UPDATE FACILITY'                         
UPD      CSECT                                                                  
         PRINT NOGEN                                                            
         NMODL WORKX-WORKD,**$UPD**,RA,CLEAR=YES,RR=RE                          
         USING WORKD,RC            RC=A(W/S)                                    
         ST    RE,RELO                                                          
         MVC   SRPARS,0(R1)        SAVE S/R PARAMETER LIST                      
         L     R9,SRPAR1                                                        
         USING SYSFACD,R9          R9=A(SYSFACS)                                
         L     R8,VSSB                                                          
         USING SSBD,R8             R8=A(SSB)                                    
         NI    SSBJFLAG,255-SSBJFWKR STOP ABEND LOOPS                           
         MVC   SYSNAME,SSBSYSNA                                                 
         MVC   ATIA,SRPAR2                                                      
         MVC   AUTL,SRPAR3                                                      
         L     R1,AUTL                                                          
         USING UTLD,R1                                                          
         OI    TPRGIND,X'14'       SET CONVERTED MAX IOS                        
         LA    RF,MYPGMLST                                                      
         STCM  RF,7,TASVC          SET DUMMY PGMLST ENTRY ALSO                  
         DROP  R1                                                               
*                                                                               
         USING COMFACSD,R1                                                      
         L     R1,SRPAR4                                                        
         MVC   VSWITCH,CSWITCH                                                  
         MVC   VHELLO,CHELLO                                                    
         MVC   VDATCON,CDATCON                                                  
         MVC   VCALLOVL,CCALLOV                                                 
         MVC   VLOCKET,CLOCKET                                                  
         MVC   VHEXOUT,CHEXOUT                                                  
         MVC   VRECUP,CRECUP                                                    
         MVC   VHEXIN,CHEXIN                                                    
         MVC   VGETFACT,CGETFACT                                                
         MVC   VSMFOUT,CSMFOUT                                                  
         DROP  R1                                                               
*                                                                               
         LHI   RF,IOA1-WORKD                                                    
         LA    RF,WORKD(RF)                                                     
         ST    RF,AIOA1                                                         
         LHI   RF,IOA2-WORKD                                                    
         LA    RF,WORKD(RF)                                                     
         ST    RF,AIOA2                                                         
         LHI   RF,IOA3-WORKD                                                    
         LA    RF,WORKD(RF)                                                     
         ST    RF,AIOA3                                                         
         LHI   RF,IOA1SAVE-WORKD                                                
         LA    RF,WORKD(RF)                                                     
         ST    RF,SAVEAIO1                                                      
         MVI   SENUM,X'01'         SET CONNECTED TO SE1                         
*                                                                               
         GOTO1 VDATAMGR,DMCB,(0,=CL8'FILTAB'),0                                 
         MVC   AFILTAB,DMCB+4                                                   
*                                                                               
         EJECT                                                                  
***********************************************************************         
* Scan FACWRK for held recovery files for update pending              *         
***********************************************************************         
         USING UKRECD,R2                                                        
WKR001   LA    R2,FINDEX           READ FACWRK INDEX                            
         XC    UKINDEX,UKINDEX                                                  
         LA    RE,FACWRK           P1 - DMCB for FACWRK                         
         ST    RE,FWAFILE                                                       
         ST    R2,FWANDX           P2 - DMCB for FACWRK                         
                                                                                
         USING FWRECD,R3                                                        
         LHI   R3,IOA-WORKD        P3 - DMCB FOR FACWRK                         
         LA    R3,WORKD(R3)                                                     
         ST    R3,FWAREC                                                        
                                                                                
         LHI   R4,WBUFF-WORKD                                                   
         LA    R4,WORKD(R4)                                                     
         ST    R4,FWABUF           P4 - DMCB for FACWRK                         
         LAY   RF,PQBUFF           Buffer area for PQ work                      
         ST    RF,ACIREC                                                        
                                                                                
         USING WKRECD,R4                                                        
         GOTO1 VDATAMGR,FWDMCB,(X'00',=C'BUF')                                  
*                                                                               
WKR010   GOTO1 VDATAMGR,FWDMCB,(X'08',=C'IND')                                  
         CLI   8(R1),0                                                          
         BE    WKR020                                                           
         TM    8(R1),X'80'         TEST END OF INDEX                            
         BO    WKRX                                                             
         ICM   RF,15,UKSYSSUBPRG   SO RF IN E-MAIL SHOWS THE FACWRK KEY         
         DC    H'0'                DIE IF DISK ERROR                            
*                                                                               
WKR020   TM    UKSTAT,WKSTHO       FILE MUST BE HOLD                            
         BZ    WKR010                                                           
         TM    UKSTAT,WKSTKE       AND NOT KEEP                                 
         BO    WKR010                                                           
         CLI   UKCLASS,C'R'        MUST BE RECOVERY CLASS                       
         BNE   WKR010                                                           
         CLI   UKSUBPRG,C' '       TEST FACPAK ID PRESENT                       
         BNH   *+14                NO                                           
         CLC   UKSUBPRG,SSBSYSN1   ELSE MUST MATCH                              
         BNE   WKR010                                                           
*                                                                               
         MVC   WORK(2),UKUSRID     PUT USERID INTO WORK                         
         BAS   RE,GETALPH          GET AGYALPH IN WORK+2                        
*                                                                               
         USING UTLD,R1                                                          
         L     R1,AUTL                                                          
         MVC   TAGY,WORK+2         SAVE IT IN UTL0 FOR DATAMGR                  
         MVC   TUSER,WORK                                                       
         DROP  R1                                                               
*                                                                               
         GOTO1 VDATAMGR,FWDMCB,(X'00',RANDOM),,0                                
         BE    WKR022                                                           
         CLI   8(R1),X'90'                                                      
         BE    *+10                                                             
         ICM   RF,15,UKSYSSUBPRG   SO RF IN E-MAIL SHOWS THE FACWRK KEY         
         DC    H'0'                                                             
         GOTO1 VDATAMGR,FWDMCB,(X'00',=C'KEE')                                  
         B     WKRX                                                             
*                                                                               
WKR022   MVC   RECCOUNT,WKRECS     SAVE NUMBER OF RECORDS                       
         MVC   RECCOUNT,WKRECS     SAVE NUMBER OF RECORDS                       
         MVC   PQSYSPRG,UKSYSPRG   SAVE SYSTEM PROGRAM                          
         MVC   PQUSERID,WKPQUSER   USERID OF ASSOCIATED PQ REPORT               
         MVC   PQCINUM,WKPQCI      C/I # or report # if WKIREP# on              
         MVI   WKPQ#FLG,NO                                                      
         TM    WKIND1,WKIREP#      Report # instead of CI                       
         BZ    *+8                 No                                           
         MVI   WKPQ#FLG,YES        Yes                                          
         MVC   SVWKSPP,WKSYSPRG    Show what kind of FACWRK it is               
         LA    RF,FINDEX           RESTORE FACWRK INDEX DMCB PARAMETER          
         ST    RF,FWANDX                                                        
***********************************************************************         
* Note: PQCINUM AKA WKPQCI is also WKPQREP# if WKIREP# is on                    
***********************************************************************         
         OC    PQCINUM,PQCINUM     PQ information present?                      
         BNZ   WKR022#             No - don't read PQ info                      
         MVC   PQUSERID,WKUSRID    Id, who is this?                             
         B     WKR025                                                           
*                                                                               
WKR022#  L     R0,ACIREC                                                        
         LHI   R1,L'PQBUFF                                                      
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE               Clear PQBUFF                                 
*                                                                               
PKNDX    USING PKINDEX,NDX                                                      
         XC    NDX,NDX             Get PRTQ file id from WKPQ                   
         MVC   PKNDX.PKSRCID,PQUSERID                                           
         CLI   WKPQ#FLG,NO         CI OR REPORT #                               
         BE    WKR022A             CI                                           
         MVC   PKNDX.PKREPNO,PQREP#    Much easier (direct locate)              
         MVC   PRTQID,=CL8'PRTQUE'     Prime for DATAMGR call                   
         B     WKR022C                                                          
         DROP  R4                                                               
                                                                                
WKR022A  GOTO1 VDATAMGR,PQDMCB,(0,=C'GFILE'),=C'PRTQUE',NDX,0,ACIREC            
         CLI   8(R1),0                                                          
         BE    *+10                                                             
         ICM   RF,15,UKSYSSUBPRG   SO RF IN E-MAIL SHOWS THE FACWRK KEY         
         DC    H'0'                                                             
*                                                                               
         MVC   PRTQID,PKNDX.PKUSRINF   PRIME THE PQ BUFFER                      
         GOTO1 VDATAMGR,PQDMCB,(0,=C'BUFFER'),PRTQID,0,0,ACIREC                 
         CLI   8(R1),0                                                          
         BE    *+10                                                             
         ICM   RF,15,UKSYSSUBPRG   SO RF IN E-MAIL SHOWS THE FACWRK KEY         
         DC    H'0'                                                             
*                                                                               
         L     RE,ACIREC                                                        
         MVC   CIDATA,12(RE)       RETRIEVE CIDATA FROM BUFFER                  
         SR    RE,RE               PREPARE FOR DIVIDE                           
         LH    RF,PQCINUM          C/I NUMBER OF ASSOCIATED PQ REPORT           
         BCTR  RF,0                                                             
         LH    R1,CITRKS           NUMBER OF TRACKS PER PART 1 C/I              
         DR    RE,R1               RF = PQ REFERENCE NUMBER                     
         STCM  RF,3,PKNDX.PKREPNO  REF. NUMBER OF ASSOCIATED RPT                
         MVC   PKNDX.PKSRCID,PQUSERID USERID OF ASSOCIATED PQ REPORT            
*                                                                               
WKR022C  OI    PKNDX.PKFLAG,X'04'  DIRECT LOCATE BY REF. NUMBER                 
         GOTO1 VDATAMGR,PQDMCB,(0,=C'INDEX'),PRTQID,NDX,SAVE,ACIREC,0           
         CLI   8(R1),0                                                          
         BE    WKR023                                                           
         GOTOR MSGERR,C'1'         Report ERROR                                 
         B     WKR025              Just process that for now                    
***********************************************************************         
*THIS IS A BUG THAT WILL BE FIXED, 6/6/03, YYUN                                 
*        CLC   =C'SBY',UKSYSPRG    IS THIS A SPOT BUY COPY?                     
*        BE    WKR025              YES - PROCESS IT AS USUAL                    
*                                                                               
*SHOULD NOT HAPPEN, KEEP THE WKFILE, AND THEN ABEND.                            
*        GOTO1 VDATAMGR,FWDMCB,(X'00',=C'KEE')                                  
*        DC    H'0'                                                             
*                                                                               
*        B     WKR025              JUST PROCESS THAT FOR NOW, YYUN              
***********************************************************************         
                                                                                
***********************************************************************         
* Code added to SRTIM not to dispatch WRKFs that have bit on (HOLD)             
***********************************************************************         
WKR023   MVC   PRTQID,=CL8'PRTQ'            Prime the PQ buffer                 
         MVC   PRTQID+4(1),PKNDX.PKUSRINF+4 Specific PQ                         
         TM    PKNDX.PKATTB,QLATJOBI   Soon job still running?                  
         BZ    WKR025                  No, okay to run                          
         CLI   WKPQ#FLG,YES                                                     
         BNE   WKRX                Go away. This can be a problem               
*----------------------------------------------------------------------         
*  If we get stuck on entry patch out 'BNE   WKRX' and report it                
*  If WKPQ#FLG is YES then die since SRTIM should not of put on HOLD or         
*----------------------------------------------------------------------         
*NOP AH3 GOTO1 VDATAMGR,FWDMCB,(X'00',=C'KEE')                                  
*AH3     GOTO1 MSGERR,C'2'                                                      
         B     WKRX                                                             
*NOP AH3 ICM   RF,15,UKSYSSUBPRG   SO RF IN E-MAIL SHOWS THE FACWRK KEY         
*NOP AH3 DC    H'0'                                                             
*                                                                               
WKR025   GOTO1 VDATAMGR,FWDMCB,(X'00',=C'KEE')                                  
*                                                                               
WKR030   GOTO1 VDATAMGR,FWDMCB,(X'00',=C'REA')                                  
         CLI   8(R1),0                                                          
         BE    WKR040                                                           
         TM    8(R1),X'80'         TEST END OF FILE                             
         BO    WKR200                                                           
         ICM   RF,15,UKSYSSUBPRG   SO RF IN E-MAIL SHOWS THE FACWRK KEY         
         DC    H'0'                DIE IF DISK/FORMAT ERROR                     
*                                                                               
WKR040   CLC   =C'$UO',FWRHDR      TEST GO TO SRUPDXX OVERLAY                   
         BE    WKR300                                                           
         CLC   FWRHDR,=C'SOON'                                                  
         BE    WKR050                                                           
         CLC   FWRHDR,=C'TSO '                                                  
         BE    WKR050                                                           
         CLC   FWRHDR,=C'SMF='                                                  
         BE    WKR058                                                           
         CLC   FWRUSER,=C'USER='                                                
         BE    WKR052                                                           
         CLC   FWRUSER,=C'LAST='                                                
         BE    WKR053                                                           
         CLC   FWRUSER,=C'POST '                                                
         BE    WKR054                                                           
         CLC   FWRUSER,=C'POSTX'                                                
         BE    WKR055                                                           
         CLI   RFILTY,X'A1'        TEST FOR CTFILE                              
         BE    *+12                                                             
         CLI   RFILTY,X'AF'        TEST FOR GENFIL                              
         BNE   WKR042                                                           
         MVI   RSYS,X'0A'          FORCE SENUM TO 0A FOR CTFILE!                
         B     WKR043                                                           
WKR042   CLI   RFILTY,X'AD'        TEST CTUSER                                  
         BNE   WKR045                                                           
WKR043   CLI   IOA1,C'8'           CT8REC (LOCKET) TYPE                         
         BNE   WKR045                                                           
         CLI   IOA1+1,C'*'         TEST STEP2 RECORD                            
         BE    WKR070                                                           
         B     WKR060                                                           
*                                                                               
WKR045   TM    FLAG,FLFIRSTQ       TEST FIRST TIME FLAG                         
         BO    WRK046                                                           
         GOTOR FIRSTIME            ANYTHING TO DO BEFORE FIRST UPDATE           
WRK046   GOTO1 UPDATE              PROCESS RECORD IN IOA                        
         B     WKR030                                                           
*                                                                               
WKR050   MVC   HEADER,0(R3)        SAVE HEADER INFO                             
         B     WKR030                                                           
*                                                                               
WKR052   MVC   USID,FWRUSID        SAVE USER ID                                 
         MVC   LUID,FWRLUID        LUID                                         
         MVC   PQID,FWRPQID        PQ ID                                        
         MVC   WKID,FWRWKID        WORKER FILE ID                               
         B     WKR030                                                           
*                                                                               
WKR053   MVC   PQIDL,FWRPQID       LAST PQ ID                                   
         B     WKR055                                                           
*                                                                               
WKR054   OI    ACFLG,ACPOST        SET START OF POSTING FILE ENTRIES            
         B     WKR030                                                           
*                                                                               
WKR055   TM    ACFLG,ACTLAST       END OF POSTING FILE ENTRIES                  
         BZ    WKR030                                                           
         MVI   ACFLG,0                                                          
         ICM   RF,15,ADADDTRN                                                   
         BZ    WKR030                                                           
         LA    R1,ACTRNBLK                                                      
         USING TRNBLK,R1                                                        
         OI    TRNINDS,TRNILAST    SET 'LAST CALL'                              
         OI    TRNINDS2,TRNIUPDG   Update GL postings                           
         GOTOR (RF)                CALL ADDTRN                                  
         B     WKR030                                                           
         DROP  R1                                                               
*                                                                               
WKR058   ICM   RF,15,VSMFOUT                                                    
         BZ    WKR030              NEXT                                         
         L     R3,FWAREC                                                        
         CLI   FWRSMFTY,12         LIMITED TO TYPE 12 FOR NOW                   
         BNE   WKR030                                                           
         GOTOR VSMFOUT,SMFPARM,12,FWRSMFR                                       
         OC    SMFP1,SMFP1                                                      
         BZ    WKR030                                                           
         WTO   'SMF#ERR - SRUPD00 SMFOUT FACWRK balance problem'                
         B     WKR030                                                           
*                                                                               
WKR060   CLI   RRECTY,3            IF CT8REC ADD (UNLOCK)                       
         BNE   WKR030                                                           
         MVC   WORK(15),IOA1+4                                                  
         GOTO1 VLOCKET,DMCB,(C'U',WORK),SRPAR4                                  
         B     WKR030                                                           
         EJECT                                                                  
*=====================================================================          
* FOR SPT TRF, ADD A SOON JOB FOR AMS/GEN NOW THAT AUTO/GEN IS DONE             
*=====================================================================          
                                                                                
WKR070   BRAS  RE,AMSADD                                                        
         B     WKR030                                                           
*                                                                               
WKR200   GOTO1 EXHOOK                                                           
         GOTO1 WFSTCHG             TAKE WORKER FILE OFF KEEP                    
         GOTOR PQSTCHG,DMCB,0      MAKE PQ REPORT VISIBLE                       
         GOTOR PQST2CHG                                                         
         GOTOR LASTIME             ANYTHING TO DO BEFORE I GO                   
         BRAS  RE,UNLOCK                                                        
*                                                                               
         GOTO1 VDATAMGR,FWDMCB,(X'00',=C'PUR')                                  
         B     WKR400                                                           
*                                  GO TO FACWRK HANDLER OVERLAY                 
WKR300   GOTOR VCALLOVL,DMCB,(FWRHDR+3,0),0,0                                   
         CLI   4(R1),X'FF'         TEST OVERLAY FOUND                           
         BNE   *+10                                                             
         ICM   RF,15,UKSYSSUBPRG   SO RF IN E-MAIL SHOWS THE FACWRK KEY         
         DC    H'0'                                                             
         L     RF,0(R1)                                                         
         GOTOR (RF),DMCB,SRPARS,FWDMCB,AIOA2                                    
*                                                                               
WKR400   DS    0H                                                               
*                                                                               
*&&US*&& BRAS  RE,BACKUP           SEE IF ANY MORE UNPROCESSED AROUND           
*                                                                               
WKRX     XIT1                                                                   
*                                                                               
SYSNOP   GOTO1 VSWITCH,DMCB,X'01FFFFFF',0                                       
         ICM   RF,15,UKSYSSUBPRG   SO RF IN E-MAIL SHOWS THE FACWRK KEY         
         DC    H'0'                FORCE BACK TO SERVICE                        
         EJECT                                                                  
***********************************************************************         
* FACWK recovery record is in IOA. Attempt to dup changes in file     *         
***********************************************************************         
UPDATE   NTR1                                                                   
         MVC   RACTN,RRECTY        SAVE RECORD TYPE                             
*                                                                               
         CLI   RACTN,CHANGEQ       FOR CHANGE ACTION                            
         BNE   UPD005                                                           
         CLI   LASTACTN,COPYQ      LAST ACTION MUST BE COPY                     
         BNE   UPD001                                                           
         CLC   SENUM,RSYS          SAME SYSTEM                                  
         BNE   UPD001                                                           
         CLC   LASTFILE,RFILTY     SAME FILE                                    
         BE    UPD005                                                           
UPD001   DS    0H                  ELSE SEQUENCE ERROR                          
         ICM   RF,15,UKSYSSUBPRG   SO RF IN E-MAIL SHOWS THE FACWRK KEY         
         DC    H'0'                                                             
*                                                                               
UPD005   CLC   SENUM,RSYS          IS SWITCH REQUIRED                           
         BE    UPD010                                                           
         GOTO1 VSWITCH,DMCB,(RSYS,X'FFFFFFFF'),0                                
         MVC   SENUM,RSYS                                                       
         CLI   4(R1),0             SWITCHED OK ?                                
         BE    *+18                                                             
         CLI   4(R1),2             TEST FOR SYSTEM NOP                          
         BE    SYSNOP                                                           
         ICM   RF,15,UKSYSSUBPRG   SO RF IN E-MAIL SHOWS THE FACWRK KEY         
         DC    H'0'                UNKNOWN SWITCH ERROR                         
*                                                                               
         L     R5,VSELIST          LOCATE SELIST ENTRY                          
         LH    R0,0(R5)                                                         
         L     R1,2(R5)                                                         
         LA    R5,6(R5)                                                         
         USING SELISTD,R5                                                       
         CLC   SESYS,SENUM         MATCH SE NUMBER TO SELIST                    
         BE    *+14                                                             
         BXLE  R5,R0,*-10                                                       
         ICM   RF,15,UKSYSSUBPRG   SO RF IN E-MAIL SHOWS THE FACWRK KEY         
         DC    H'0'                INVALID SWITCH SYSTEM NUMBER                 
         TM    SEIND,SEISETRO+SEIRONLY                                          
         BZ    UPD010              SYSTEM IS UP AND UPDATEABLE                  
         ICM   RF,15,UKSYSSUBPRG   SO RF IN E-MAIL SHOWS THE FACWRK KEY         
         DC    H'0'                SYSTEM IS UP, BUT IN READ-ONLY MODE          
         DROP  R5                                                               
*                                                                               
UPD010   SR    R1,R1                                                            
         IC    R1,RFILTY           USE EXTERNAL NUMBER                          
         SLL   R1,5                TO INDEX INTO FILTAB                         
*        LARL  R5,FILTAB                                                        
         L     R5,AFILTAB                                                       
         AR    R5,R1                                                            
         USING FILTABD,R5                                                       
         MVC   LASTFILE,DMFLNUM    SAVE FILE NUMBER                             
         MVC   FILENAME,DMFLNAME   AND FILE NAME                                
*                                                                               
UPD020   MVC   LASTACTN,RACTN      SAVE ACTION VALUE                            
         SR    R1,R1                                                            
         ICM   R1,1,RACTN                                                       
         BNZ   *+10                                                             
         ICM   RF,15,UKSYSSUBPRG   SO RF IN E-MAIL SHOWS THE FACWRK KEY         
         DC    H'0'                RACTN MUST BE BETWEEN 1 - 3                  
         CLI   RACTN,3                                                          
         BNH   *+10                                                             
         ICM   RF,15,UKSYSSUBPRG   SO RF IN E-MAIL SHOWS THE FACWRK KEY         
         DC    H'0'                                                             
         BCTR  R1,0                                                             
         SLL   R1,2                                                             
         EX    0,ACTTAB(R1)        PROCESS RECOVERY RECORD                      
*                                                                               
ACTTAB   B     COPY                                                             
         B     CHANGE                                                           
         B     ADD                                                              
         EJECT                                                                  
***********************************************************************         
* Copy records must be identical to records on file. If recovery copy *         
* is different then protection during update must have failed         *         
***********************************************************************         
                                                                                
COPY     MVI   TRFLAG,0            CLEAR TRAFFIC FLAG                           
         MVI   SOAFLAG,0           CLEAR SOA FLAG                               
         TM    ACFLG,ACPOST        TEST ACC POSTING MODE                        
         BO    COPYX               YES, SKIP COPY/CHANGE                        
         CLI   DMFLTYP,DMFLIS      TEST INDEX SEQUENTIAL                        
         BNE   COPY10                                                           
         GOTO1 VDATAMGR,DMCB,(X'88',DMREAD),FILENAME,IOA1,AIOA2                 
         TM    8(R1),X'FD'                                                      
         BZ    COPY20                                                           
         ICM   RF,15,UKSYSSUBPRG   SO RF IN E-MAIL SHOWS THE FACWRK KEY         
         DC    H'0'                                                             
*                                                                               
COPY10   CLI   DMFLTYP,DMFLDA      TEST DIRECT ACCESS                           
         BE    *+14                                                             
         BRAS  RE,DEATH                                                         
         ICM   RF,15,UKSYSSUBPRG   SO RF IN E-MAIL SHOWS THE FACWRK KEY         
         DC    H'0'                UNKNOWN FILE TYPE                            
         MVC   DDA,RVCHR                                                        
         GOTO1 VDATAMGR,DMCB,(X'88',GETREC),FILENAME,DDA,AIOA2,IOWORK           
         TM    8(R1),X'FD'                                                      
         BZ    COPY20                                                           
         ICM   RF,15,UKSYSSUBPRG   SO RF IN E-MAIL SHOWS THE FACWRK KEY         
         DC    H'0'                                                             
*                                                                               
COPY20   TM    DMFLSTYP,DMFLFIX    TEST FIXED LENGTH RECORD                     
         BNO   COPY21                                                           
         MVC   RECLEN,DMFLMINI     IF SO MINIMUM LEN IS LEN                     
         B     COPY25                                                           
COPY21   TM    DMFLFLG1,DMFLRLEN   V/L RECORDS LEN MUST BE IN RECORD            
         BO    *+14                                                             
         BRAS  RE,DEATH                                                         
         ICM   RF,15,UKSYSSUBPRG   SO RF IN E-MAIL SHOWS THE FACWRK KEY         
         DC    H'0'                                                             
         SR    R1,R1                                                            
         IC    R1,DMFLLEND                                                      
         L     R0,AIOA2                                                         
         AR    R1,R0               INDEX TO RECORD LENGTH                       
         MVC   RECLEN,0(R1)                                                     
*                                                                               
COPY25   L     R0,AIOA3            MOVE FACWK RECORD TO IOA3                    
         LA    RE,IOA1                                                          
         SR    R1,R1                                                            
         ICM   R1,3,RECLEN                                                      
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
*                                                                               
         L     R0,AIOA3            USE NEW BUFFER FOR COMPARE                   
         L     RE,AIOA2                                                         
         CLI   DMFLTYP,DMFLDA      TEST DIRECT ACCESS                           
         BNE   COPY25F                                                          
         CLC   =C'UNTFIL',DMFLNAME                                              
         BNE   COPY25F                                                          
         SR    RF,RF                                                            
         ICM   RF,1,DMFLDAD        LINKAGE AREA                                 
         BNZ   *+6                                                              
         DC    H'00'                                                            
         AR    RF,RE                                                            
         CLC   =X'00000001',0(RF)                                               
         BNE   *+8                                                              
         MVI   3(RF),00                                                         
         SR    RF,RF                                                            
         ICM   RF,1,DMFLDAD                                                     
         AR    RF,R0                                                            
         CLC   =X'00000001',0(RF)                                               
         BNE   *+8                                                              
         MVI   3(RF),00                                                         
                                                                                
COPY25F  SR    R1,R1                                                            
         ICM   R1,3,RECLEN                                                      
         LR    RF,R1                                                            
         CLCL  R0,RE                                                            
         BE    COPYX                                                            
*                              *** CODE FOR EXTENDING PUTRECS                   
         CHI   R1,4                                                             
         BNH   COPY26              MAKE SURE NOT PAST END                       
*                                                                               
         OC    0(4,RE),0(RE)       FILE  MUST HAVE ZEROS                        
         BNZ   COPY26                                                           
         LR    R1,R0                                                            
         CLC   =XL3'FFFFFF',0(R1)                                               
         BNE   COPY26              FACWK MUST HAVE FFS                          
*                                                                               
         LR    R1,RF               RESTORE LENGTH                               
         AHI   R0,4                                                             
         AHI   RE,4                GO PAST OFFENDING FFS                        
         AHI   R1,-4                                                            
         AHI   RF,-4                                                            
         CLCL  R0,RE               COMPARE REST OF RECORD                       
         BE    COPYX                                                            
*                                                                               
COPY26   BAS   RE,CPYHOOK          NOT A MATCH                                  
         BE    COPYX               IF HOOK RETURNS EQU THEN OK                  
*                                                                               
         CLI   RFILTY,X'32'        TEST TRAFFIC FILE                            
         BNE   COPY28                                                           
         L     RE,AIOA2            POINT TO FILE RECORD                         
         CLC   =X'0A2E',0(RE)      IS IT A TBA RECORD                           
         BNE   COPY28                                                           
         MVI   TRFLAG,C'Y'         SET TO SKIP CHANGE                           
         B     COPYX                                                            
*                                                                               
COPY28   CLC   =C'GZ',WORK+2       IS THIS GM                                   
         BNE   COPY30                                                           
*                                                                               
         CLC   =C'SCG',FINDEX+2    IS THIS CABLE GEN & NET 5                    
         BE    COPYX                                                            
*                                                                               
         CLC   =C'SNI',FINDEX+2    IS THIS NINS GEN                             
         BE    COPYX                                                            
*                                                                               
COPY30   DS    0H                                                               
         BRAS  RE,DEATH                                                         
         ICM   RF,15,UKSYSSUBPRG   SO RF IN E-MAIL SHOWS THE FACWRK KEY         
         DC    H'0'                COPY RECORD DID NOT MATCH                    
*                                                                               
COPYX    B     UPDATEX                                                          
         EJECT                                                                  
***********************************************************************         
* Copy record has been read for update now replace with change record *         
***********************************************************************         
CHANGE   TM    ACFLG,ACPOST        TEST ACC POSTING MODE                        
         BO    COPYX               YES, SKIP COPY/CHANGE                        
*                                                                               
         BAS   RE,CHGHOOK          SEE IF WE HAVE A HOOK                        
*                                                                               
         BAS   RE,SETTSYS                                                       
*                                                                               
         CLI   DMFLTYP,DMFLIS      TEST INDEX SEQUENTIAL                        
         BNE   CHNG10                                                           
         GOTO1 VDATAMGR,DMCB,DMWRT,FILENAME,IOA1,IOA1                           
         CLI   8(R1),0                                                          
         BE    CHNG20                                                           
         ICM   RF,15,UKSYSSUBPRG   SO RF IN E-MAIL SHOWS THE FACWRK KEY         
         DC    H'0'                                                             
*                                                                               
CHNG10   CLI   DMFLTYP,DMFLDA      TEST DIRECT ACCESS                           
         BE    *+10                                                             
         ICM   RF,15,UKSYSSUBPRG   SO RF IN E-MAIL SHOWS THE FACWRK KEY         
         DC    H'0'                UNKNOWN FILE TYPE                            
*                                                                               
         CLI   SOAFLAG,C'S'        FOR SOA, TEST SKIP CHG                       
         BE    UPDATEX              YES, SKIP IT                                
*                                                                               
         CLI   TRFLAG,C'Y'         FOR TRAFFIC ONLY, TEST IGNORE CHG            
         BNE   CHNG12                                                           
         CLI   RFILTY,X'32'                                                     
         BE    *+10                                                             
         ICM   RF,15,UKSYSSUBPRG   SO RF IN E-MAIL SHOWS THE FACWRK KEY         
         DC    H'0'                                                             
         LA    RE,IOA1                                                          
         CLC   =X'0A2E',0(RE)                                                   
         BE    UPDATEX                                                          
         ICM   RF,15,UKSYSSUBPRG   SO RF IN E-MAIL SHOWS THE FACWRK KEY         
         DC    H'0'                                                             
*                                                                               
CHNG12   GOTO1 VDATAMGR,DMCB,PUTREC,FILENAME,DDA,IOA1,IOWORK                    
         CLI   8(R1),0                                                          
         BE    *+10                                                             
         ICM   RF,15,UKSYSSUBPRG   SO RF IN E-MAIL SHOWS THE FACWRK KEY         
         DC    H'0'                                                             
*                                                                               
CHNG20   BAS   RE,RSTTSYS                     RESTORE TSYS                      
         GOTO1 VDATAMGR,DMCB,DMUNLK,FILENAME  UNLOCK BLOCK                      
         CLI   8(R1),0                                                          
         BE    UPDATEX                                                          
         ICM   RF,15,UKSYSSUBPRG   SO RF IN E-MAIL SHOWS THE FACWRK KEY         
         DC    H'0'                                                             
         EJECT                                                                  
***********************************************************************         
* TO ADD DA RECORDS DO ADDREC                                         *         
* TO ADD IS RECORDS FIRST CHECK IF IS/DA PAIR IF NOT DO DMADD         *         
* IF SO CHECK DISK ADDR. IF NON ZERO USE IT AS IT MUST BE A PASSIVE   *         
* IF IT IS ZERO TEST KEY WITH PREVIOUS ADDREC KEY AND IF ITS IDENTICAL*         
* IGNORE THE RECORD AS ADDREC HAS ALREADY ADDED IT                    *         
* IF NOT INSERT LAST DA KEY AND ADD AS IT MUST BE A NEW PASSIVE       *         
***********************************************************************         
ADD      BAS   RE,ADDHOOK          SEE IF WE HAVE A HOOK                        
                                                                                
         TM    FLAG,FLZ5BYP        BYPASS THIS RECORD IN ADD ROUTINE            
         BO    UPDATEX                                                          
         TM    FLAG,FLB1BYP        BYPASS THIS RECORD IN ADD ROUTINE            
         BO    UPDATEX                                                          
         TM    FLAG,FLBUBYP        BYPASS THIS RECORD IN ADD ROUTINE            
         BO    UPDATEX                                                          
         TM    FLAG,FLTABYP        BYPASS THIS RECORD IN ADD ROUTINE            
         BO    UPDATEX                                                          
         TM    FLAG,FLNIBYP        BYPASS THIS RECORD IN ADD ROUTINE            
         BO    UPDATEX                                                          
                                                                                
         CLI   DMFLTYP,DMFLIS      TEST INDEX SEQUENTIAL                        
         BNE   ADD10                                                            
         TM    DMFLFLG1,DMFLISDA   DO WE NEED A DISK ADDRESS                    
         BNO   ADD1                                                             
         SR    R1,R1                                                            
         IC    R1,DMFLDAD                                                       
         LA    R1,IOA1(R1)                                                      
         OC    0(4,R1),0(R1)       DISK ADDRESS MUST BE A PASSIVE               
         BNZ   ADD1                                                             
         CLC   DMFLPAIR,DALAST     LAST DA ADD MUST BE PAIRED DA FILE           
         BE    *+10                                                             
         ICM   RF,15,UKSYSSUBPRG   SO RF IN E-MAIL SHOWS THE FACWRK KEY         
         DC    H'0'                                                             
         MVC   0(4,R1),DALINK      USE DA FROM LAST ADDREC                      
         SR    R1,R1                                                            
         IC    R1,DMFLKEYL         GET KEY LENGTH                               
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   DKEY(0),IOA1        TEST WITH LAST DA KEY ADDED                  
         BE    ADD20               ADDREC HAS ALREADY DONE THIS                 
*                                                                               
ADD1     BAS   RE,SETTSYS                                                       
*                                                                               
         MVC   KEY,IOA1                                                         
         GOTO1 VDATAMGR,DMCB,DMADD,FILENAME,KEY,IOA1                            
         CLI   8(R1),0                                                          
         BE    ADD20                                                            
*&&US                                                                           
         CLI   DMFLNUM,X'33'       TEST TRFDIR                                  
         BNE   ADD2                                                             
         CLI   IOA1,X'8A'          TEST ACTIVITY POINTER                        
         BNE   ADD5                                                             
         B     ADD4                                                             
*                                                                               
ADD2     CLI   DMFLNUM,X'23'       TEST SPTDIR                                  
         BNE   ADD5                                                             
         CLI   IOA1,X'10'          TEST BUYREC                                  
         BNH   ADD5                                                             
ADD4     TM    8(R1),X'DF'         IGNORE DUP KEY ERROR                         
         BZ    ADD20                                                            
         B     ADD8                                                             
*                                                                               
ADD5     CLI   DMFLNUM,X'36'       TEST XSPDIR                                  
         BNE   ADD8                                                             
         CLC   =X'0AE2',IOA1       TEST NET RECAP PASSIVE                       
         BE    ADD20                                                            
*&&                                                                             
ADD8     BRAS  RE,DEATH                                                         
         ICM   RF,15,UKSYSSUBPRG   SO RF IN E-MAIL SHOWS THE FACWRK KEY         
         DC    H'0'                                                             
*                                                                               
ADD10    CLI   DMFLTYP,DMFLDA      TEST DIRECT ACCESS                           
         BE    *+10                                                             
         ICM   RF,15,UKSYSSUBPRG   SO RF IN E-MAIL SHOWS THE FACWRK KEY         
         DC    H'0'                UNKNOWN FILE TYPE                            
         CLI   DMFLSTYP,DMFLREQ    TEST REQUEST FILE                            
         BE    ADD080                                                           
*                                                                               
         BAS   RE,SETTSYS                                                       
*                                                                               
         MVC   DALAST,DMFLNUM                                                   
         MVC   DKEY,IOA1                                                        
         GOTO1 VDATAMGR,DMCB,ADDREC,FILENAME,DKEY,IOA1,IOWORK                   
         CLI   8(R1),0                                                          
         BE    ADD15                                                            
         BRAS  RE,DEATH                                                         
         ICM   RF,15,UKSYSSUBPRG   SO RF IN E-MAIL SHOWS THE FACWRK KEY         
         DC    H'0'                                                             
*                                                                               
ADD15    DS    0H                                                               
         MVC   DALINK,DKEY         SAVE DISK ADDRESS                            
         MVC   DKEY,IOA1           SAVE KEY                                     
         B     ADD20                                                            
ADD20    BAS   RE,RSTTSYS                                                       
*                                                                               
         GOTO1 VDATAMGR,DMCB,DMUNLK,FILENAME  UNLOCK BLOCK                      
         CLI   8(R1),0                                                          
         BE    UPDATEX                                                          
         ICM   RF,15,UKSYSSUBPRG   SO RF IN E-MAIL SHOWS THE FACWRK KEY         
         DC    H'0'                                                             
         EJECT                                                                  
SETTSYS  L     R1,AUTL                                                          
         USING UTLD,R1                                                          
         MVC   HALF,TSYS        SAVE SYS/PRG                                    
         CLI   RSYS,0                                                           
         BE    *+10                                                             
         MVC   TSYS,RSYS        SET TSYS/TPRG FROM WORKER FILE                  
         CLI   RPRG,0                                                           
         BE    *+10                                                             
         MVC   TPRG,RPRG        IF NZ                                           
         BR    RE                                                               
         DROP  R1                                                               
*                                                                               
RSTTSYS  L     R1,AUTL                                                          
         USING UTLD,R1                                                          
         MVC   TSYS(2),HALF                                                     
         BR    RE                                                               
         DROP  R1                                                               
         EJECT                                                                  
***********************************************************************         
* ADD REQUEST RECORDS                                                 *         
***********************************************************************         
ADD080   DS    0H                                                               
         LH    R1,IOA              GET RECORD LENGTH                            
         TM    RTIME,RTIMEEXT      Has extention?                               
         BZ    ADD082              No                                           
         LA    RE,IOA-1(R1)        Point to end of record - 1                   
         LLC   RF,0(RE)            Get length of extention                      
         SR    R1,RF               Adjust for extention length                  
*                                                                               
ADD082   SHI   R1,L'RECVHDR+4      ADJUST FOR LEN/RECOVERY HDR                  
         SR    R0,R0                                                            
         D     R0,=F'80'           GIVES NUMBER OF CARDS IN R1                  
         LTR   R0,R0               TEST FOR REMAINDER                           
         BZ    *+10                                                             
         ICM   RF,15,UKSYSSUBPRG   SO RF IN E-MAIL SHOWS THE FACWRK KEY         
         DC    H'0'                                                             
*                                                                               
         LA    RE,IOA1                                                          
         USING RQHHDRD,RE                                                       
         BCTR  R1,0                ADJUST FOR 80 BYTE OVERHEAD                  
         BCTR  R1,0                AND NEED TO SET N'CARDS-1                    
         SLL   R1,4                NUMBER TO LEFT NIBBLE                        
         ZIC   R0,RQHFLAG          IN CASE ANY BITS ARE ALREADY SET...          
         N     R0,=X'0000000F'     ...LEAVE THEM ON                             
         OR    R1,R0               'OR' N'CARDS-1 INTO RQHFLAG                  
         STC   R1,RQHFLAG          SET NUMBER IN REQHDR                         
         SRL   R1,4                RESTORE COUNT                                
         DROP  RE                                                               
*                                                                               
* MUST NOT ADD DUPLICATE SPOOL-TYPE REQUESTS WITH                               
* SAME SIN BECAUSE IT CAUSES PROBLEMS IN END OF DAY.                            
*                                                                               
         OC    NEXTSIN,NEXTSIN     TEST FIRST TIME THIS WKFILE                  
         BNZ   ADD102                                                           
         MVC   SAVESIN,RSIN                                                     
         MVC   NEXTSIN,RSIN                                                     
*                                                                               
* NO EASY WAY TO KNOW IF IT IS A SPOOL REQUEST, SO                              
* LOOK FOR SYSTEM INPUT NUMBER IN ALL REQUEST CARDS                             
*                                                                               
ADD102   L     RF,RSIN             GET SIN IN REQUEST CARD FORMAT               
         CVD   RF,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  QSIN,DUB                                                         
*                                                                               
         LA    R0,1(R1)            SET TO NUMBER OF CARDS - 1                   
         LA    RE,IOA1+80                                                       
*                                                                               
ADD104   CLC   QSIN,5(RE)                                                       
         BNE   ADD110                                                           
         LA    RE,80(RE)                                                        
         BCT   R0,ADD104                                                        
*                                                                               
* ALL MATCH - REPLACE SIN IN REQUESTS WITH NEXTSIN                              
*                                                                               
         L     RF,NEXTSIN          GET SIN IN REQUEST CARD FORMAT               
         LA    RF,1(RF)            BUMP FOR NEXT TIME                           
         ST    RF,NEXTSIN                                                       
         BCTR  RF,0                                                             
         CVD   RF,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  QSIN,DUB                                                         
*                                                                               
         LA    R0,1(R1)                                                         
         LA    RE,IOA1+80                                                       
*                                                                               
ADD106   MVC   5(6,RE),QSIN                                                     
         LA    RE,80(RE)                                                        
         BCT   R0,ADD106                                                        
*                                                                               
ADD110   DS    0H                                                               
         GOTO1 VDATAMGR,DMCB,(X'20',DMADD),=C'REQUEST',DKEY,IOA1                
         CLI   8(R1),0                                                          
         BE    UPDATEX                                                          
         ICM   RF,15,UKSYSSUBPRG   SO RF IN E-MAIL SHOWS THE FACWRK KEY         
         DC    H'0'                                                             
*                                                                               
UPDATEX  XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
* DEATH OF A UPDATE                                                   *         
* IF YOU WANT TO SEND AN E-MAIL DUMP NOTIFICATION YOU HAVE TO         *         
* BEGIN THE MESSAGE WITH '*SOON DUMP*' ELSE IT WILL NOT BE RECOGNIZED *         
***********************************************************************         
DEATH    ST    RE,DUB              SAVE WHERE WE CAME FROM                      
         NTR1  ,                                                                
         OC    USID,USID           DID WE FIND ID YET?                          
         BNZ   DEATH2              YES                                          
         BAS   RE,FINDID           NO, LOOK FOR ONE                             
         OC    USID,USID           DID WE FIND AN ID?                           
         BZ    DEATHX              NO                                           
*                                                                               
DEATH2   MVI   OPMSG,C' '                                                       
         MVC   OPMSG+1(L'OPMSG-1),OPMSG                                         
         MVC   OPSOON,SOONMSG                                                   
         MVC   OPUSID,USID         USER                                         
         MVC   OPPRGM,WKID+2       PROGRAM AND SUB/PROGRAM                      
         MVC   OPLUID,LUID                                                      
         SR    RE,RE                                                            
         ICM   RE,7,DUB+1          GET RE                                       
         LA    RF,0(RB)                                                         
         SR    RE,RF               GET DISPLACEMENT                             
         ST    RE,DUB                                                           
         GOTO1 VHEXOUT,PARML,DUB+1,OPDLOC,3,0,0                                 
         BRAS  RE,WTO              WRITE THE MESSAGE                            
         EJECT                                                                  
***********************************************************************         
* Now read to end of FACWRK file                                                
***********************************************************************         
DEATH10  TM    FWDMCB+8,X'80'      TEST ALREADY THERE                           
         BO    DEATHX                                                           
*                                                                               
         GOTO1 VDATAMGR,FWDMCB,(X'00',=C'REA')                                  
         CLI   8(R1),0                                                          
         BE    DEATH12                                                          
         TM    8(R1),X'80'         TEST END OF FILE                             
         BO    DEATHX                                                           
*                                                                               
DEATH12  CLI   RFILTY,X'A1'        TEST FOR CTFILE                              
         BNE   DEATH10                                                          
         CLI   IOA1,C'8'           CT8REC (LOCKET) TYPE                         
         BNE   DEATH10                                                          
*                                                                               
         MVC   WORK(15),IOA1+4                                                  
         GOTO1 VLOCKET,DMCB,(C'U',WORK),SRPAR4                                  
         B     DEATH10                                                          
*                                                                               
DEATHX   DS    0H                                                               
         GOTOR PQSTCHG,DMCB,1      MARK PQ REPORT VISIBLE AND IN ERROR          
         B     UPDATEX                                                          
         EJECT                                                                  
**********************************************************************          
* READ TO END OF FACWK FILE TO FIND USID                             *          
**********************************************************************          
FINDID   NTR1                                                                   
         TM    FWDMCB+8,X'80'      ARE WE ALREADY AT END?                       
         BO    FINDX               YES, JUST GO BACK                            
*                                                                               
FIND02   GOTO1 VDATAMGR,FWDMCB,(X'00',=C'REA')                                  
         CLI   8(R1),0                                                          
         BE    FIND04                                                           
         TM    8(R1),X'80'         TEST END OF FILE                             
         BO    FINDX                                                            
         ICM   RF,15,UKSYSSUBPRG   SO RF IN E-MAIL SHOWS THE FACWRK KEY         
         DC    H'0'                DIE IF DISK/FORMAT ERROR                     
*                                                                               
FIND04   CLC   FWRUSER,=C'USER='                                                
         BE    FIND06                                                           
         CLC   FWRUSER,=C'LAST='                                                
         BE    FIND08                                                           
*                                                                               
         CLI   RFILTY,X'A1'        TEST FOR CTFILE                              
         BNE   FIND02                                                           
         CLI   IOA1,C'8'           CT8REC (LOCKET) TYPE                         
         BNE   FIND02                                                           
*                                                                               
****     CLI   RRECTY,2            IF CT8REC CHANGE (UNLOCK)                    
         CLI   RRECTY,3            IF CT8REC ADD (UNLOCK)                       
         BNE   FIND02                                                           
*                                                                               
         MVC   WORK(15),IOA1+4                                                  
         GOTO1 VLOCKET,DMCB,(C'U',WORK),SRPAR4                                  
         B     FIND02                                                           
*                                                                               
FIND06   MVC   USID,FWRUSID        SAVE USER ID                                 
         MVC   LUID,FWRLUID        LUID                                         
         MVC   PQID,FWRPQID        PQ ID                                        
         MVC   WKID,FWRWKID        WORKER FILE ID                               
         B     FIND02                                                           
*                                                                               
FIND08   MVC   PQIDL,FWRPQID       LAST PQ ID                                   
         B     FIND02                                                           
*                                                                               
FINDX    XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
* Get AGYALPH into work+2 from USERID in work                         *         
***********************************************************************         
GETALPH  NTR1                                                                   
         LA    R5,KEY              READ ID RECORD                               
         USING CTIREC,R5                                                        
         XC    CTIKEY,CTIKEY                                                    
         MVI   CTIKTYP,CTIKTYPQ                                                 
         MVC   CTIKNUM,WORK                                                     
         GOTO1 VDATAMGR,DMCB,DMREAD,CTFILE,KEY,IOA1                             
         CLI   0(R1),0                                                          
         BE    *+10                                                             
         ICM   RF,15,UKSYSSUBPRG   SO RF IN E-MAIL SHOWS THE FACWRK KEY         
         DC    H'0'                                                             
         LA    R5,IOA1+CTIDATA-CTIREC                                           
         USING CTAGYD,R5                                                        
         XC    WORK+2(2),WORK+2    CLEAR OUTPUT AREA                            
GETAL01  CLI   CTAGYEL,CTAGYELQ    SCAN FOR ALPHA ELEMENT                       
         BE    GETAL02                                                          
         CLI   CTAGYEL,0                                                        
         BE    GETALX              NOT FOUND                                    
         SR    R0,R0                                                            
         IC    R0,CTAGYLEN         TRY NEXT ELEMENT                             
         AR    R5,R0                                                            
         B     GETAL01                                                          
*                                                                               
GETAL02  MVC   WORK+2(2),CTAGYID   SAVE ALPHAID IN WORK+2                       
GETALX   XIT1                                                                   
         LTORG                                                                  
***********************************************************************         
*        Report this to MF FACIL team                                           
*        R4 = FACWRK area                                                       
***********************************************************************         
         USING WKRECD,R4                                                        
MSGERR   NTR1                                                                   
         STC   R1,MSGFWER#                                                      
         LLH   RF,WKUSRID          *** FACWRK info ***                          
         CVD   RF,DUB                                                           
         OI    DUB+L'DUB-1,X'0F'                                                
         UNPK  MSGFWCID,DUB                                                     
         MVC   MSGFWSUB,WKSYSPRG                                                
         LLH   RF,WKFILNO                                                       
         CVD   RF,DUB                                                           
         OI    DUB+L'DUB-1,X'0F'                                                
         UNPK  MSGFWRP#,DUB                                                     
         DROP  R4                                                               
                                                                                
PKNDX    USING PKINDEX,NDX                                                      
         LLH   RF,PQUSERID         *** PQ RPT info ***                          
         CVD   RF,DUB                                                           
         OI    DUB+L'DUB-1,X'0F'                                                
         UNPK  MSGFPCID,DUB                                                     
         MVC   MSGFPSUB,PKNDX.PKSUBID                                           
         LLH   RF,PQREP#                                                        
         CVD   RF,DUB                                                           
         OI    DUB+L'DUB-1,X'0F'                                                
         UNPK  MSGFPRP#,DUB                                                     
         GOTO1 VDATAMGR,DMCB,=C'OPMSG',('MSGFWLNQ',MSGFW1)                      
         XIT1                                                                   
         DROP  PKNDX                                                            
                                                                                
***********************************************************************         
* Constant - may move to routine                                                
***********************************************************************         
MSGFW1   DS    0C                                                               
******&& DC    C'AUTONOTE*AHYDN:'                                               
*&&US*&& DC    C'AUTONOTE*US-MF_Fac_Team_NY:'                                   
*&&UK*&& DC    C'AUTONOTE*US-MF_Fac_Team_UK:'                                   
         DC    C'ERR#='                                                         
MSGFWER# DC    C' '                                                             
MSGFWER1 EQU   C'1'                PQ entry not found                           
MSGFWER2 EQU   C'2'                Job still set to running                     
         DC    C' '                                                             
         DC    C'WK='                                                           
MSGFWCID DC    C'NNNNN'                                                         
         DC    C','                                                             
MSGFWSUB DC    C'XXX'                                                           
         DC    C','                                                             
MSGFWRP# DC    C'NNNNN'                                                         
         DC    C' '                                                             
         DC    C'PQ='                                                           
MSGFPCID DC    C'NNNNN'                                                         
         DC    C','                                                             
MSGFPSUB DC    C'XXX'                                                           
         DC    C','                                                             
MSGFPRP# DC    C'NNNNN'                                                         
MSGFWLNQ EQU   *-MSGFW1                                                         
         EJECT                                                                  
***********************************************************************         
* Change the status of the associate worker file                      *         
*   1. Take the worker file off keep                                  *         
***********************************************************************         
WFSTCHG  NTR1                                                                   
         OC    WKID,WKID           TEST WORKER FILE KEY                         
         BZ    WFSTCHGX            NONE SO NOTHING TO UNKEEP                    
                                                                                
         USING UKRECD,R2                                                        
         LA    R2,FINDEX           READ WORKER INDEX                            
         XC    FINDEX,FINDEX                                                    
         MVC   UKINDEX,WKID                                                     
*        L     R0,ATIA             USE TIA FOR WKFILE BUFFER                    
*        L     R0,FWABUFF          USE WORKER FILE BUFFER                       
*        LHI   R1,L'WBUFF                                                       
         L     R0,ACIREC           CLEAR BUFFER                                 
         LHI   R1,L'PQBUFF                                                      
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
WFSTCH10 GOTO1 VDATAMGR,DMCB,(X'08',INDEX),=C'WKFILE',FINDEX,AIOA1,    X        
               ACIREC                                                           
         CLI   8(R1),0                                                          
         BE    WFSTCH20                                                         
         BRAS  RE,DEATH                                                         
         ICM   RF,15,UKSYSSUBPRG   SO RF IN E-MAIL SHOWS THE FACWRK KEY         
         DC    H'0'                CAN'T FIND WORKER FILE                       
*                                                                               
         USING WKRECD,R4                                                        
WFSTCH20 LA    R4,WKID             SEARCH ID                                    
         CLC   UKKEY,WKINDEX       MUST BE SAME KEY                             
         BNE   WFSTCH10            TRY NEXT                                     
         CLC   UKFILNO,WKFILNO     FILE NUMBER                                  
         BNE   WFSTCH10            TRY NEXT                                     
                                                                                
         GOTO1 VDATAMGR,DMCB,(X'00',UNKEEP)                                     
*                                                                               
WFSTCHGX XIT1                                                                   
         DROP  R2,R4                                                            
         EJECT                                                                  
***********************************************************************         
* CALL EXIT ROUTINE                                                   *         
* NOTE: UTL IS SWITCHED TO THE LAST SYSTEM UPDATED                    *         
* R9=SYSFACS R8=SSB R2=UKRECD                                         *         
***********************************************************************         
         USING UKRECD,R2                                                        
EXHOOK   NTR1                                                                   
         LA    R1,KEYTAB           TABLE OF EXITS                               
EXHK1    L     RF,4(R1)                                                         
         A     RF,RELO                                                          
         CLC   UKSYSPRG,0(R1)                                                   
         BER   RF                  EXECUTE USER EXIT                            
         LA    R1,8(R1)                                                         
         CLI   0(R1),0                                                          
         BNE   EXHK1                                                            
EXHOOKX  XIT1                                                                   
         DROP  R2                                                               
*                                                                               
KEYTAB   DS    0F                                                               
*&&US*&& DC    C'TCK',X'00',A(TALEXIT)                                          
*&&US*&& DC    C'TDC',X'00',A(TALEXIT)                                          
*&&US*&& DC    C'A21',X'00',A(ACCEXIT)                                          
*&&US*&& DC    C'A23',X'00',A(ACCEXIT)                                          
*&&US*&& DC    C'A27',X'00',A(ACCEXIT)                                          
*&&US*&& DC    C'A29',X'00',A(ACCEXIT)                                          
*&&US*&& DC    C'A55',X'00',A(ACCEXIT9)                                         
*&&US*&& DC    C'A56',X'00',A(ACCEXIT9)                                         
*&&US*&& DC    C'AAA',X'00',A(ACCEXIT9)                                         
*&&US*&& DC    C'AAB',X'00',A(ACCEXIT9)                                         
         DC    H'0'                                                             
         EJECT                                                                  
***********************************************************************         
* CALL EXIT ROUTINE FOR COPY NOT EQUAL                                *         
* NOTE: UTL IS SWITCHED TO THE LAST SYSTEM UPDATED                    *         
* R9=SYSFACS R8=SSB R2=UKRECD                                         *         
* IOA1=COPY RECORD FROM FACWRK                                        *         
* IOA2=CURRENT FILE RECORD                                            *         
***********************************************************************         
         USING UKRECD,R2                                                        
CPYHOOK  NTR1                                                                   
         LA    R1,KEYTAB1          TABLE OF COPY EXITS                          
CPYHK1   L     RF,4(R1)                                                         
         A     RF,RELO                                                          
         CLC   UKSYSPRG,0(R1)                                                   
         BNE   CPYHK1A                                                          
         BASR  RE,RF               EXECUTE USER EXIT                            
         B     CPYHK1X                                                          
*                                                                               
CPYHK1A  LA    R1,8(R1)                                                         
         CLI   0(R1),0                                                          
         BNE   CPYHK1                                                           
         LTR   RB,RB               SET NEQ TO CAUSE DUMP                        
*                                                                               
CPYHK1X  XIT1                                                                   
         DROP  R2                                                               
*                                                                               
KEYTAB1  DS    0F                                                               
*&&UK*&& DC    C'DEM',X'00',A(DEMEXIT)                                          
*&&US*&& DC    C'SAI',X'00',A(SAIEXIT)  TRAFFIC PATTERNS                        
*&&US*&& DC    C'SAT',X'00',A(SAIEXIT)  TRAFFIC PATTERNS                        
*&&US*&& DC    C'SSH',X'00',A(SAIEXIT)  TRAFFIC SHIPPING RECAP PATTERNS         
*&&US*&& DC    C'A21',X'00',A(BILEXIT)  BILLING EXCEPTIONS                      
*&&US*&& DC    C'A23',X'00',A(BILEXIT)  BILLING EXCEPTIONS                      
*&&US*&& DC    C'A27',X'00',A(BILEXIT)  BILLING EXCEPTIONS                      
*&&US*&& DC    C'A29',X'00',A(BILEXIT)  BILLING EXCEPTIONS                      
*&&US*&& DC    C'AAA',X'00',A(APPEXIT)  ACC AUTO APPROVE                        
*&&US*&& DC    C'SSE',X'00',A(NETEXIT)                                          
*&&US*&& DC    C'SNI',X'00',A(NETEXIT)                                          
*&&US*&& DC    C'TCK',X'00',A(TCKEXIT)  TALENT CHECKS                           
*&&US*&& DC    C'SI2',X'00',A(SI2EXIT)  INVOICE MATCH                           
*&&US*&& DC    C'SOA',X'00',A(SOAEXIT)  SPOT ORDER ACTIVITY - SPOBA***          
         DC    H'0'                                                             
         EJECT                                                                  
***********************************************************************         
* CALL EXIT ROUTINE FOR CHANGE                                        *         
* NOTE: UTL IS SWITCHED TO THE LAST SYSTEM UPDATED                    *         
* R9=SYSFACS R8=SSB R2=UKRECD                                         *         
* IOA1=CURRENT RECORD TO BE WRITTEN                                   *         
***********************************************************************         
         USING UKRECD,R2                                                        
CHGHOOK  NTR1                                                                   
         LA    R1,KEYTAB2          TABLE OF CHANGE EXITS                        
CHGHK1   L     RF,4(R1)                                                         
         A     RF,RELO                                                          
         CLC   UKSYSPRG,0(R1)                                                   
         BNE   CHGHK1A                                                          
         BASR  RE,RF               EXECUTE USER EXIT                            
         B     CHGHK1X             EXIT WITH CC PRESERVED                       
*                                                                               
CHGHK1A  LA    R1,8(R1)                                                         
         CLI   0(R1),0                                                          
         BNE   CHGHK1                                                           
CHGHK1X  XIT1                                                                   
         DROP  R2                                                               
*                                                                               
KEYTAB2  DS    0F                                                               
*&&UK*&& DC    C'DEM',X'00',A(DEMEXIT2)                                         
*&&US*&& DC    C'A21',X'00',A(BILEXIT2)                                         
*&&US*&& DC    C'A23',X'00',A(BILEXIT2)                                         
*&&US*&& DC    C'A27',X'00',A(BILEXIT2)                                         
*&&US*&& DC    C'A29',X'00',A(BILEXIT2)                                         
*&&US*&& DC    C'AAA',X'00',A(APPEXIT2)                                         
*&&US*&& DC    C'SSE',X'00',A(NETEXIT2)                                         
*&&US*&& DC    C'SNI',X'00',A(NETEXIT2)                                         
*&&US*&& DC    C'SI2',X'00',A(SI2EXIT2)                                         
*&&US*&& DC    C'SOA',X'00',A(SOAEXIT2)                                         
         DC    H'0'                                                             
         EJECT                                                                  
***********************************************************************         
* CALL EXIT ROUTINE FOR ADD                                           *         
* NOTE: UTL IS SWITCHED TO THE LAST SYSTEM UPDATED                    *         
* R9=SYSFACS R8=SSB R2=UKRECD                                         *         
* IOA1=CURRENT RECORD TO BE ADDED                                     *         
***********************************************************************         
         USING UKRECD,R2                                                        
ADDHOOK  NTR1                                                                   
         LA    R1,KEYTAB3          TABLE OF ADD EXITS                           
ADDHK1   L     RF,4(R1)                                                         
         A     RF,RELO                                                          
         CLC   UKSYSPRG,0(R1)                                                   
         BNE   ADDHK1A                                                          
         BASR  RE,RF               EXECUTE USER EXIT                            
         B     ADDHK1X                                                          
*                                                                               
ADDHK1A  LA    R1,8(R1)                                                         
         CLI   0(R1),0                                                          
         BNE   ADDHK1                                                           
ADDHK1X  XIT1                                                                   
         DROP  R2                                                               
*                                                                               
KEYTAB3  DS    0F                                                               
*&&UK*&& DC    C'DEM',X'00',A(DEMEXIT3)                                         
*&&US*&& DC    C'SZ5',X'00',A(SZ5EXIT)  EASI CONVERSION                         
*&&US*&& DC    C'SB1',X'00',A(SB1EXIT3) SPOT BILLING                            
*&&US*&& DC    C'SBU',X'00',A(SBUEXIT3) NET BILLING                             
*&&US*&& DC    C'S07',X'00',A(S07EXIT3) SPOT UNBILLING                          
*&&US*&& DC    C'S7U',X'00',A(S07EXIT3) NET UNBILLING                           
*&&US*&& DC    C'A21',X'00',A(BILEXIT3) ACC BILLING                             
*&&US*&& DC    C'A23',X'00',A(BILEXIT3) ACC UNBILLING                           
*&&US*&& DC    C'A27',X'00',A(BILEXIT3) ACC 27 BILLING                          
*&&US*&& DC    C'A29',X'00',A(BILEXIT3) ACC 27 UNBILLING                        
*&&US*&& DC    C'SAI',X'00',A(SAIEXIT3) AUTO GEN                                
*&&US*&& DC    C'SNI',X'00',A(SNIEXIT3) NINS GEN                                
         DC    H'0'                                                             
         EJECT                                                                  
***********************************************************************         
* EXIT ROUTINES                                                       *         
***********************************************************************         
         USING FWRECD,R3                                                        
*&&UK                                                                           
DEMEXIT  EQU   *                                                                
         LA    R4,IOA1             COPY RECORD                                  
         L     R5,AIOA2            CURRENT RECORD                               
         USING DEMFILED,R4                                                      
         CLI   DEKCNTR,DEKCNTRQ    IS IT A CONTROL REC                          
         BNE   DEMEXDIE                                                         
         XC    DEMSAVE,DEMSAVE                                                  
*                                  COPY CHANGED DATA                            
         CLC   DECNTPRO(6),DECNTPRO-DEMFILED(R5)                                
         BE    *+16                                                             
         MVC   DEMSAVE,DECNTPRO-DEMFILED(R5)                                    
         MVC   DECNTPRO(6),DEMSAVE                                              
         MVC   DECNTUCI(8),DECNTUCI-DEMFILED(R5)                                
*                                                                               
         LA    R0,IOA1             THEN RECOMPARE THEM                          
         L     RE,AIOA2                                                         
         SR    R1,R1                                                            
         ICM   R1,3,RECLEN         RECLEN WAS SET PREVIOUSLY                    
         LR    RF,R1                                                            
         CLCL  R0,RE               ARE RECORDS NOW THE SAME ?                   
         BNE   DEMEXDIE                                                         
DEMEXOK  CR    RB,RB               SET EQU AND RETURN                           
         B     CPYHK1X                                                          
DEMEXDIE LTR   RB,RB               SET NEQ FOR DUMP                             
         B     CPYHK1X                                                          
*                                                                               
DEMEXIT2 EQU   *                                                                
         LA    R4,IOA1             COPY RECORD                                  
         USING DEMFILED,R4                                                      
         CLI   DEKCNTR,DEKCNTRQ    IS IT A CONTROL REC                          
         BNE   DEMXX2                                                           
         OC    DEMSAVE,DEMSAVE                                                  
         BZ    *+10                                                             
         MVC   DECNTPRO(6),DEMSAVE                                              
DEMXX2   B     CHGHK1X             RETURN                                       
         SPACE 1                                                                
DEMEXIT3 EQU   *                                                                
         LA    R4,IOA1             ADD RECORD                                   
         BAS   RE,DEMUNIV                                                       
DEMXX3   B     ADDHK1X             RETURN                                       
         SPACE 1                                                                
DEMUNIV  EQU   *                                                                
         CLI   RSYS,X'14'          TEST MEDZ                                    
         BNER  RE                                                               
         CLI   RFILTY,X'42'        TEST MEDFILE                                 
         BNER  RE                                                               
         CLI   UNVKTYP-DUNIV(R4),UNVKTYPQ IS IT A UNIVERSE RECORD               
         BNER  RE                                                               
         OI    FLAG,FLUNVZQ        NOTE UNIVERSES ADDED/CHANGED                 
         BR    RE                                                               
*&&                                                                             
         SPACE 2                                                                
*&&US                                                                           
TALEXIT  EQU   *                   TALENT URGENT CHECK RUN                      
*&&DO                                                                           
         GOTO1 VCALLOVL,DUB,0,X'D9000A88'  GET A(TAL SYSTEM TABLES)             
         L     R1,0(R1)                                                         
         A     R1,TGACKLK-TGTABLES(R1)     R1=A(LOCKOUT STATUS BYTE)            
         MVI   0(R1),CKLKOK                SET OK STATUS                        
         B     EXHOOKX                                                          
*&&                                                                             
         XC    WORK(15),WORK                                                    
         MVC   WORK(1),RSYS                SYSTEM NUMBER                        
         MVC   WORK+5(10),=CL10'TAL_CHECKS'                                     
*                                                                               
TALEXIT3 GOTO1 VLOCKET,DMCB,(C'U',WORK),SRPAR4                                  
         CLI   4(R1),2             TEST TABLE BUSY                              
         BE    TALEXIT3            YES - TRY AGAIN                              
         CLI   4(R1),0             ANY ERRORS?                                  
         BE    EXHOOKX             NO, LEAVE                                    
*                                                                               
         BRAS  RE,TALX_ERR                                                      
*                                                                               
         ICM   RF,15,=C'TALC'                                                   
         DC    H'0'                                                             
*                                                                               
*&&US                                                                           
ACCEXIT  EQU   *                ACCPAK - BILLING                                
         L     R2,AIOA2            TEST LAST RECORD IS LEDGER                   
         USING LDGRECD,R2                                                       
         CLC   LDGKUNT(2),=C'SJ'   TEST SJ LEDGER                               
         BNE   EXHOOKX             NO, DON'T CALL SETLOCK                       
         CLC   LDGKLDG+1(LDGKSTA-LDGKLDG-1),SPACES                              
         BNE   EXHOOKX             NOT A LEDGER RECORD                          
         LA    R2,LDGRFST                                                       
         XR    R0,R0                                                            
ACCEXIT3 CLI   0(R2),0                                                          
         BE    EXHOOKX             EXIT - WITHOUT CALL TO SETLOCK               
         CLI   0(R2),LGLELQ        GET LOCK ELEMENT                             
         BE    ACCEXIT9            CALL SETLOCK TO UNLOCK                       
         IC    R0,1(R2)                                                         
         AR    R2,R0                                                            
         B     ACCEXIT3                                                         
         DROP  R2                                                               
*                                                                               
ACCEXIT9 EQU   *                ACCPAK - CHECKS, AUTO APPROVE                   
         GOTO1 VCALLOVL,DUB,0,X'D9000A66'  GET A(ACSETLOCK)                     
         L     RF,0(R1)                                                         
         GOTO1 (RF),DUB,(C'U',AIOA2),SRPAR4 ISSUE UNLOCK                        
         CLI   4(R1),0                                                          
         BE    EXHOOKX                                                          
         DC    H'0'                ERROR TRYING TO UNLOCK                       
*&&                                                                             
         SPACE 2                                                                
*&&US                                                                           
         USING FILTABD,R5                                                       
SZ5EXIT  DS    0H                                                               
         NI    FLAG,X'FF'-FLZ5BYP                                               
         SPACE                                                                  
         CLC   =X'0EA3',IOA1       IF REP INVOICE PROCESS IT                    
         BE    SZ5050                                                           
         SPACE                                                                  
         CLC   =X'0E03',IOA1       IF NOT INVOICE, DO NOT TOUCH                 
         BNE   SZ5EXITE                                                         
         SPACE                                                                  
SZ5050   DS    0H                                                               
         L     R4,AIOA3                                                         
         CLC   IOA1(32),0(R4)      SAME KEY AS PREVIOUS?                        
         BE    SZ5100               NEEDS TO BE BYPASSED (KEY ADD)              
*                                   OR CHANGE (RECORD)                          
         CLI   RFILTY,X'37'        THIS XSPFIL                                  
         BE    SZ5EXITE                                                         
         SPACE                                                                  
* WAIT FOR SECOND RECORD (DIRECTORY ADD) TO SAVE KEY                            
         SPACE                                                                  
         MVC   0(32,R4),IOA1       SAVE KEY, BUT DO NORMAL PROCESSING           
         B     SZ5EXITE                                                         
         SPACE                                                                  
SZ5100   DS    0H                                                               
         OI    FLAG,FLZ5BYP        BYPASS THIS RECORD IN ADD ROUTINE            
         SPACE                                                                  
* THIS WILL PROCESS THE RECORDS AS CHANGES, AND BYPASS KEY ADDS                 
         SPACE                                                                  
         CLI   DMFLTYP,DMFLIS      TEST INDEX SEQUENTIAL                        
         BE    SZ5EXITE             BYPASS                                      
         SPACE                                                                  
* SIMULATE COPY TO READ BEFORE WRITE - THIS 'ADD' IS REALLY A CHANGE            
         SPACE                                                                  
         GOTO1 VDATAMGR,DMCB,(X'88',GETREC),FILENAME,DALINK,AIOA2,     C        
               IOWORK                                                           
         TM    8(R1),X'FD'                                                      
         BZ    SZ5120                                                           
         DC    H'0'                                                             
*                                                                               
SZ5120   DS    0H                                                               
         LA    R4,IOA1             GET ADDR OF NEW REC                          
         LA    R4,42(,R4)          1ST ELEM                                     
         L     R7,AIOA2            OLD - EXISTING RECORD                        
         SPACE                                                                  
SZ5140   DS    0H                                                               
         GOTO1 VHELLO,DMCB,(C'P',FILENAME),(R7),(R4),0                          
         CLI   12(R1),0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         ZIC   R0,1(R4)                                                         
         AR    R4,R0                                                            
         CLI   0(R4),0                                                          
         BNE   SZ5140                                                           
         SPACE                                                                  
         GOTO1 VDATAMGR,DMCB,PUTREC,FILENAME,DALINK,AIOA2,IOWORK                
         CLI   8(R1),0                                                          
         BE    SZ5160                                                           
         DC    H'0'                                                             
SZ5160   DS    0H                                                               
         GOTO1 VDATAMGR,DMCB,DMUNLK,FILENAME  UNLOCK BLOCK                      
         CLI   8(R1),0                                                          
         BE    SZ5EXITE                                                         
         DC    H'0'                                                             
         SPACE                                                                  
SZ5EXITE CR    RB,RB               SET EQU AND RETURN                           
         B     CPYHK1X                                                          
         DROP  R5                                                               
*&&                                                                             
         SPACE 2                                                                
*&&US                                                                           
         USING FILTABD,R5                                                       
SB1EXIT3 DS    0H                                                               
         NI    FLAG,X'FF'-FLB1BYP                                               
*                                                                               
         CLC   =X'0E01',IOA1       IF NOT STABUCK, DO NOT TOUCH                 
         BNE   SB1EXITE                                                         
*                                                                               
         MVC   SB1KEY,IOA1         SAVE KEY FROM FACKWORK                       
*                                                                               
*  CHECK IF THE RECORD WITH THE SAME KEY HAS ALREADY BEEN ADDED                 
*                                                                               
         GOTO1 VDATAMGR,DMCB,(X'88',DMRDHI),=C'SPTDIR',SB1KEY,AIOA2             
         TM    8(R1),X'FF'-X'02'                                                
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R1,AIOA2                                                         
         CLC   SB1KEY,0(R1)                                                     
         BNE   SB1EXITE           IF REC NOT THERE YET, DO NORMAL PROC          
*                                                                               
         OI    FLAG,FLB1BYP       ELSE BYPASS THIS REC IN ADD ROUTINE           
*                                                                               
*   THIS WILL PROCESS THE RECORDS AS CHANGES, AND BYPASS KEY ADDS               
*                                                                               
         CLI   DMFLTYP,DMFLIS     TEST INDEX SEQUENTIAL                         
         BE    SB1EXITE           BYPASS                                        
*                                                                               
*    SIMULATE COPY - THIS 'ADD' IS REALLY A CHANGE                              
*                                                                               
         MVC   SB1DA,14(R1)        SAVE DISK ADDRESS                            
         GOTO1 VDATAMGR,DMCB,(X'88',GETREC),FILENAME,SB1DA,AIOA2,IOWORK         
         TM    8(R1),X'FF'-X'02'                                                
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    R4,IOA1             GET ADDR OF NEW REC (FACWRK)                 
         LA    R4,24(R4)           1ST ELEM                                     
         L     R7,AIOA2            OLD - EXISTING RECORD ON DISK                
         LA    R6,24(R7)           1ST ELEM                                     
*                                                                               
SB1140   DS    0H                                                               
         CLI   0(R4),X'0E'         LOOK FOR BILLING ELEMS                       
         BNE   SB1145                                                           
*                                                                               
SB1141   CLI   0(R6),0             EOR                                          
         BE    SB1143                                                           
         CLC   0(6,R4),0(R6)       FIND WHERE TO PUT ELEM                       
         BL    SB1143              (SAME WAY IT DOES IN SPOT BILLING)           
         ZIC   R0,1(R6)                                                         
         AR    R6,R0                                                            
         B     SB1141                                                           
*                                                                               
SB1143   GOTO1 VRECUP,DMCB,(R7),(R4),(R6)                                       
         LA    R6,24(R7)           REPOINT TO FIRST ELEMENT                     
*                                                                               
SB1145   ZIC   R0,1(R4)                                                         
         AR    R4,R0                                                            
         CLI   0(R4),0                                                          
         BNE   SB1140                                                           
*                                                                               
         GOTO1 VDATAMGR,DMCB,PUTREC,FILENAME,SB1DA,AIOA2,IOWORK                 
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         GOTO1 VDATAMGR,DMCB,DMUNLK,FILENAME  UNLOCK BLOCK                      
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
SB1EXITE DS    0H                                                               
         B     ADDHK1X                                                          
*&&                                                                             
         SPACE 2                                                                
*&&US                                                                           
SBUEXIT3 DS    0H                                                               
         BRAS  RE,SBUEXIT                                                       
         B     ADDHK1X                                                          
*&&                                                                             
                                                                                
*&&US                                                                           
SNIEXIT3 DS    0H                                                               
         BRAS  RE,SNIEXIT                                                       
         B     ADDHK1X                                                          
*&&                                                                             
         SPACE 2                                                                
*&&US                                                                           
S07EXIT3 DS    0H                                                               
         CLI   DMFLSTYP,DMFLREQ    REQUEST FILE?                                
         BNE   S07EXITE                                                         
         CLI   RFILTY,X'25'        SPOT REQUEST FILE?                           
         BNE   S07EXITE                                                         
         LA    RE,IOA1                                                          
         USING RQHHDRD,RE                                                       
         CLC   =C'07',RQHCARD      IS THIS A SPOT UNBILLING REQUEST?            
         BE    S07EX10                                                          
         CLC   =C'7U',RQHCARD      IS THIS A NET UNBILLING REQUEST?             
         BE    S07EX10                                                          
         CLC   =C'MY',RQHCARD      IS THIS AN UNPOSTING REQUEST?                
         BNE   S07EXITE                                                         
*                                                                               
S07EX10  DS    0H                                                               
         OI    RQHFLAG,RQHFLNK     TREAT AS LINKED REQUEST                      
         DROP  RE                                                               
*                                                                               
S07EXITE DS    0H                                                               
         B     ADDHK1X                                                          
*&&                                                                             
         SPACE 2                                                                
*&&US                                                                           
SAIEXIT  DS    0H                                                               
         CLC   =X'0A22',IOA1       THIS A PATTERN RECORD                        
         BNE   SAIEXITE                                                         
         LA    R4,IOA1+24          COPY                                         
         L     R5,AIOA2            CURRENT REC                                  
         LA    R5,24(R5)                                                        
         MVC   PATUSED-PATDTAEL(3,R5),PATUSED-PATDTAEL(R4)                      
         SPACE                                                                  
         LA    R5,IOA1+24          COPY                                         
         SPACE                                                                  
         MVI   ELCODE,X'F1'                                                     
         BAS   RE,NEXTEL                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         LR    R4,R5                                                            
         SPACE                                                                  
         L     R5,AIOA2            CURRENT                                      
         LA    R5,24(R5)                                                        
         BAS   RE,NEXTEL                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         SR    RF,RF                                                            
         IC    RF,1(R4)                                                         
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R5),0(R4)       MOVE TO CURRENT FROM COPY                    
         SPACE                                                                  
         LA    R0,IOA1             CHECK COPY RECORD IS SAME                    
         L     RE,AIOA2                                                         
         SR    R1,R1                                                            
         ICM   R1,3,RECLEN                                                      
         LR    RF,R1                                                            
         CLCL  R0,RE                                                            
         BE    SAIEXITE                                                         
         DC    H'0'                                                             
         SPACE                                                                  
SAIEXITE CR    RB,RB               SET EQU AND RETURN                           
         B     CPYHK1X                                                          
*&&                                                                             
         EJECT                                                                  
*&&US                                                                           
**********************************************************************          
* ACC COPY HOOK                                                      *          
**********************************************************************          
         USING FILTABD,R5                                                       
BILEXIT  DS    0H                                                               
         OI    ACPGM,COPBILQ       SET BILLING FLAG                             
         B     ACCOPYH                                                          
*                                                                               
APPEXIT  DS    0H                                                               
         OI    ACPGM,COPAPPQ       SET APPROVE FLAG                             
*                                                                               
ACCOPYH  GOTOR CPYIT               COPY FROM CURRENT RECORD                     
         L     R0,SAVEAIO1         SAVED AND CAN BE SEEN IN DUMP                
         LA    RE,IOA1                                                          
         SR    R1,R1                                                            
         ICM   R1,3,RECLEN                                                      
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
*                                                                               
         XC    SAVELEN,SAVELEN                                                  
         LA    R0,IOA1             CHECK COPY RECORD IS SAME                    
         L     RE,AIOA2                                                         
         ST    RE,SAVEAIO2                                                      
         SR    R1,R1                                                            
         ICM   R1,3,RECLEN                                                      
         LR    RF,R1                                                            
         CLCL  R0,RE                                                            
         BE    CPYHK1X                                                          
*                                                                               
         STCM  R1,15,SAVELEN                                                    
         LTR   RB,RB               SET NOT EQUAL & RETURN                       
         B     CPYHK1X                                                          
         EJECT                                                                  
**********************************************************************          
* ACC CHANGE HOOK                                                    *          
**********************************************************************          
BILEXIT2 EQU   *                                                                
APPEXIT2 EQU   *                                                                
         GOTOR CPYIT               COPY FROM CURRENT RECORD                     
         B     CHGHK1X             RETURN                                       
*&&                                                                             
         EJECT                                                                  
***********************************************************************         
* NET/TRAFFIC SHARED RECORDS - IF NET MERGE TRAFFIC CHANGES           *         
***********************************************************************         
         SPACE 1                                                                
*&&US                                                                           
NETEXIT  CLI   RSYS,X'2B'          ONLY NETW                                    
         BNE   CHGHK1X                                                          
         CLI   DMFLTYP,DMFLDA      TEST DIRECT ACCESS (FILE NOT DIR)            
         BNE   CHGHK1X                                                          
         L     R4,AIOA1            R4 = A(CHANGE) RECORD                        
C        USING NURECD,R4                                                        
         CLI   C.NUKTYPE,X'04'     ONLY UNIT RECORDS                            
         BNE   NETEX10                                                          
         MVC   BYTE1,C.NUKAM                                                    
         NI    BYTE1,X'F0'                                                      
         CLI   BYTE1,X'E0'         ONLY AGENCY GZ                               
         BNE   CHGHK1X                                                          
         CR    RB,RB                                                            
         B     CHGHK1X                                                          
         DROP  C                                                                
         SPACE                                                                  
NETEX10  DS    0H                                                               
C        USING REVKEY,R4                                                        
         CLI   C.REVKID,X'21'      ONLY REVISION RECORDS                        
         BNE   CHGHK1X                                                          
         SPACE                                                                  
         MVC   BYTE1,C.REVKAM                                                   
         NI    BYTE1,X'F0'                                                      
         CLI   BYTE1,X'E0'         ONLY AGENCY GZ                               
         BNE   CHGHK1X                                                          
         SPACE                                                                  
         MVI   TRFLAG,0            INIT MODIFY TRAFFIC RECORD                   
         SPACE                                                                  
         GOTO1 VDATCON,DMCB,(5,0),(3,WORK+50)  TODAY'S DATE                     
         SPACE                                                                  
         SR    R1,R1                                                            
         IC    R1,DMFLLEND         KEY LENGTH                                   
         LR    RF,R1                                                            
         SPACE                                                                  
         L     R0,AIOA2                                                         
         AR    R1,R0               INDEX TO RECORD LENGTH ON FILE               
         MVC   RECLEN,0(R1)                                                     
         SPACE                                                                  
         L     R0,AIOA1            A(COPY) RECORD                               
         L     RE,AIOA2            A(FILE) RECORD                               
         LA    R1,7(RF)            L'KEY + 2 REC LEN/1 STATUS/4 LINK            
         LR    RF,R1                                                            
         CLCL  R0,RE               IS KEY/RECORD LEN/STATUS/LINK SAME           
         BE    NETEX20                                                          
         DC    H'0'                NO, DIE                                      
         SPACE                                                                  
NETEX20  DS    0H                                                               
         ZICM  RF,RECLEN,2                                                      
         SR    RF,R1               REC LENGTH MINUS ELEM LENGTH                 
         LTR   RF,RF                                                            
         BP    *+6                                                              
         DC    H'0'                PAST END OF RECORD ?                         
         STCM  RF,3,RECLEN         SAVE REMAINING RECRORD LENGTH                
         SPACE                                                                  
         CLI   0(RE),X'10'         IS THIS 10 ELEMENT                           
         BE    *+6                                                              
         DC    H'0'                OOPS, WHAT'S WRONG                           
         SPACE                                                                  
         ZIC   R1,1(RE)            GET ELEMENT LENGTH                           
         ZICM  RF,RECLEN,2                                                      
         SR    RF,R1               REC LENGTH MINUS ELEM LENGTH                 
         LTR   RF,RF                                                            
         BP    *+6                                                              
         DC    H'0'                PAST END OF RECORD ?                         
         STCM  RF,3,RECLEN         SAVE REMAINING RECRORD LENGTH                
         SPACE                                                                  
         LR    R1,R0                                                            
         CLC   0(REVIDATE-REVREVEL,R1),0(RE) ELCODE/LEN/ASGN & REV DTE          
         BE    *+6                                                              
         DC    H'0'                                                             
         LA    RF,REVNNUM-REVREVEL(RE)                                          
         CLC   REVNNUM-REVREVEL(REVREVEQ-REVNNUM,R1),0(RF) NET REV NUM          
         BE    *+6                                                              
         DC    H'0'                                                             
         LA    RF,REVIDATE-REVREVEL(RE)                                         
         CLC   REVIDATE-REVREVEL(L'REVIDATE,R1),0(RF) INSTRUCTION DATE          
         BNH   *+6                                                              
         DC    H'0'                DIE, LATER DATE                              
         SPACE                                                                  
* INSTRUCTION DATE MUST BE BEFORE TODAY'S DATE                                  
         SPACE                                                                  
         CLC   REVIDATE-REVREVEL(L'REVIDATE,R1),WORK+50                         
         BNH   *+6                 SMUR 12/3/01 (USED TO BE BL)                 
         DC    H'0'                DIE, DATE IS NOT BEFORE TODAY'S              
         SPACE                                                                  
* SEE IF FAX BIT IS ON, ON FILE                                                 
         SPACE                                                                  
         CLC   REVFLAG-REVREVEL(1,R1),REVFLAG-REVREVEL(RE)                      
         BE    NETEX25                                                          
         TM    REVFLAG-REVREVEL(R1),REVFAX  IS FAX ON IN FILE                   
         BZ    NETEX25                                                          
         TM    REVFLAG-REVREVEL(RE),REVFAX  BUT NOT IN COPY                     
         BO    NETEX25                                                          
         MVI   TRFLAG,1            TURN FAX BIT ON                              
         SPACE                                                                  
NETEX25  ZIC   R1,1(RE)            ELEMENT LENGTH                               
         AR    R0,R1               BUMP TO NEXT ELEMENT                         
         AR    RE,R1               BUMP TO NEXT ELEMENT                         
         SPACE                                                                  
NETEX30  ZIC   R1,1(RE)            GET ELEMENT LENGTH                           
         ZICM  RF,RECLEN,2                                                      
         SR    RF,R1               REC LENGTH MINUS ELEM LENGTH                 
         LTR   RF,RF                                                            
         BP    *+6                                                              
         DC    H'0'                PAST END OF RECORD ?                         
         STCM  RF,3,RECLEN         SAVE REMAINING RECRORD LENGTH                
         SPACE                                                                  
         CLI   0(RE),X'F1'         F1 ELEMENT                                   
         BE    NETEXITX            YES, DONE                                    
         SPACE                                                                  
         LR    RF,R1                                                            
         CLCL  R0,RE               COMPARES AND BUMPS TO NEXT ELEMENT           
         BE    NETEX30                                                          
         DC    H'0'                DIE, ELEMENTS NOT EQUAL                      
         SPACE                                                                  
NETEXITX CR    RB,RB                                                            
         B     CHGHK1X                                                          
*&&                                                                             
*&&US                                                                           
NETEXIT2 CLI   RSYS,X'2B'          ONLY NETW                                    
         BNE   CHGHK1X                                                          
         CLI   DMFLTYP,DMFLDA      TEST DIRECT ACCESS (FILE NOT DIR)            
         BNE   CHGHK1X                                                          
         L     R4,AIOA1            R4 = A(CHANGE) RECORD                        
C        USING NURECD,R4                                                        
         CLI   C.NUKTYPE,X'04'     ONLY UNIT RECORDS                            
         BNE   NETX40                                                           
         MVC   BYTE1,C.NUKAM                                                    
         NI    BYTE1,X'F0'                                                      
         CLI   BYTE1,X'E0'         ONLY AGENCY GZ                               
         BNE   CHGHK1X                                                          
*                                                                               
         L     R5,AIOA2            R5 = A(CURRENT FILE RECORD)                  
D        USING NURECD,R5                                                        
*                                  MOVE 23 ELEMENT FROM CHANGE TO FILE          
         GOTO1 VHELLO,DMCB,(C'D',FILENAME),(X'23',(R5)),0,0                     
         GOTO1 (RF),(R1),(C'G',FILENAME),(X'23',(R4)),0,0                       
         SR    R7,R7                                                            
         ICM   R7,7,13(R1)                                                      
         BZ    NETX02                                                           
         SPACE                                                                  
         CLI   0(R7),X'23'         THIS HAD BETTER BE A '23' ELEM               
         BE    *+6                                                              
         DC    H'0'                                                             
         SPACE                                                                  
         GOTO1 (RF),(R1),(C'P',FILENAME),(R5),(R7),0                            
*                                                                               
NETX02   XR    R6,R6               X'21' ELEMENT IS REQUIRED                    
         XR    R7,R7                                                            
         GOTO1 VHELLO,DMCB,(C'G',FILENAME),(X'21',(R5)),0,0                     
         ICM   R6,7,13(R1)                                                      
         GOTO1 (RF),(R1),,(X'21',(R4)),0,0                                      
         ICM   R7,7,13(R1)                                                      
         STM   R6,R7,DUB           SAVE THE ADDRESSES OF THESE ELEMENTS         
*                                                                               
         LTR   R6,R6                                                            
         BZ    NETX10              NO X'21' ON FILE - JUST COPY IN              
         LTR   R7,R7                                                            
         BZ    NETX10              NO X'21' ON CHANGE - DELETE IN FILE          
*                                                                               
DE       USING NUCMLEL,R6          FILE RECORD ELEMENT                          
CE       USING NUCMLEL,R7          CHANGE RECORD ELEMENT                        
         TM    DE.NUCMLFLG,X'E0'   FILE HAS FLAGS ON?                           
         BZ    NETX10              NO - STRAIGHT REPLACE                        
*                                                                               
         CLC   D.NUPRD,C.NUPRD                                                  
         BNE   NETX08                                                           
         CLC   D.NUPRD2,C.NUPRD2                                                
         BNE   NETX08                                                           
         CLC   D.NULEN,C.NULEN                                                  
         BNE   NETX08                                                           
*                                                                               
         GOTO1 VHELLO,DMCB,(C'G',FILENAME),(X'14',(R4)),0,0                     
         SR    R7,R7                                                            
         ICM   R7,7,13(R1)                                                      
*                                                                               
         GOTO1 VHELLO,DMCB,(C'G',FILENAME),(X'14',(R5)),0,0                     
         SR    R6,R6                                                            
         ICM   R6,7,13(R1)                                                      
*                                                                               
         LTR   R6,R6               COMPARE X'14' ELEMENTS                       
         BNZ   *+14                                                             
         LTR   R7,R7                                                            
         BZ    NETX10              X'14' NOT FOUND ON EITHER RECORD             
         B     NETX08              X'14' ONLY ON FILE                           
         LTR   R7,R7                                                            
         BZ    NETX08              X'14' ONLY ON CHANGE                         
*                                                                               
         CLC   1(1,R6),1(R7)                                                    
         BNE   NETX08              LENGTH CHANGE                                
         XR    RF,RF                                                            
         IC    RF,1(R6)                                                         
         BCTR  RF,0                                                             
         EX    RF,*+8              COMPARE THE TWO X'14' ELEMENTS               
         BE    NETX10                                                           
         CLC   0(0,R6),0(R7)                                                    
*                                                                               
NETX08   LM    R6,R7,DUB           NEED TO SET BITS BACK ON                     
         NI    DE.NUCMLFLG,X'E0'   PRESERVE WHAT WAS ON IN THE FILE             
         NI    CE.NUCMLFLG,255-X'E0'                                            
         OC    CE.NUCMLFLG,DE.NUCMLFLG                                          
*                                                                               
NETX10   LM    R6,R7,DUB           REPLACE FILE WITH CHANGE                     
         GOTO1 VHELLO,DMCB,(C'D',FILENAME),(X'21',(R5)),0,0                     
         LTR   R7,R7                                                            
         BZ    NETX12                                                           
         GOTO1 (RF),(R1),(C'P',FILENAME),(R5),(R7),0                            
*                                                                               
NETX12   L     R0,AIOA1            NOW MOVE FILE OVER CHANGE FOR WRITE          
         LHI   R1,4096                                                          
         L     RE,AIOA2                                                         
         LHI   RF,4096                                                          
         MVCL  R0,RE                                                            
*                                                                               
NETXX    B     CHGHK1X                                                          
         DROP  C,D,CE,DE                                                        
*                                                                               
NETX40   DS    0H                                                               
C        USING REVKEY,R4                                                        
         CLI   C.REVKID,X'21'      ONLY REVISION RECORDS                        
         BNE   CHGHK1X                                                          
         SPACE                                                                  
         MVC   BYTE1,C.REVKAM                                                   
         NI    BYTE1,X'F0'                                                      
         CLI   BYTE1,X'E0'         ONLY AGENCY GZ                               
         BNE   CHGHK1X                                                          
         SPACE                                                                  
         CLI   TRFLAG,1            MODIFY TRAFFIC RECORD                        
         BNE   CHGHK1X              NO DONE                                     
         SPACE                                                                  
         SR    R1,R1                                                            
         IC    R1,DMFLLEND         KEY LENGTH                                   
         SPACE                                                                  
         L     RE,AIOA1            A(CHANGE) RECORD                             
         LA    R1,7(R1)            L'KEY + 2 REC LEN/1 STATUS/4 LINK            
         AR    RE,R1               BUMP TO X'10' ELEMENT                        
         CLI   0(RE),X'10'         IS THIS 10 ELEMENT                           
         BE    *+6                                                              
         DC    H'0'                OOPS, MUST BE 10 ELEMENT                     
         SPACE                                                                  
         TM    REVFLAG-REVREVEL(RE),REVFAX  IF FAX IS ON                        
         BZ    *+6                                                              
         DC    H'0'                DIE, FAX SHOULD NOT BE ON                    
         SPACE                                                                  
         OI    REVFLAG-REVREVEL(RE),REVFAX                                      
         MVI   TRFLAG,0            RESET MODIFY TRAFFIC RECORD                  
         SPACE                                                                  
         B     CHGHK1X                                                          
         SPACE                                                                  
         DROP  C                                                                
*&&                                                                             
         EJECT                                                                  
***********************************************************************         
* TALENT CHECK                                                        *         
***********************************************************************         
TCKEXIT  DS    0H                                                               
         CLI   IOA1,TLSYCDQ        ONLY SYSTEM RECORD                           
         BNE   TCKEXITN                                                         
*                                                                               
         LA    R4,IOA1             COPY RECORD                                  
         L     R5,AIOA2            CURRENT RECORD                               
         LA    R1,TLRCELEM-TLRCD                                                
*                                                                               
         AR    R4,R1               BOTH POINT TO FIRST ELEMENT                  
         AR    R5,R1               ASSUMING SYSTEM ELEMENT                      
*                                                                               
TCKEXIT3 SR    R1,R1               COMPARE ELEMENTS                             
         IC    R1,1(R4)            LENGTH OF ELEMENT                            
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   0(0,R4),0(R5)                                                    
         BNE   TCKEXITN            NOT EQUAL, ERROR OUT                         
*                                                                               
         IC    R1,1(R4)            NEXT ELEMENT IN COPY                         
         AR    R4,R1                                                            
         AR    R5,R1               NEXT ELEMENT IN CURRENT                      
         LR    RF,R4                                                            
         BAS   RE,TCKEXIT8         BUMP TO NEXT ELEMENT                         
         LR    R4,RF                                                            
*                                                                               
         LR    RF,R5                                                            
         BAS   RE,TCKEXIT8         BUMP TO NEXT ELEMENT                         
         LR    R5,RF                                                            
*                                                                               
         CLI   0(R4),0             REACHED EOR FOR COPY RECORD                  
         BNE   TCKEXIT5                                                         
         CLI   0(R5),0                                                          
         BE    TCKEXITE            EOR FOR CURRENT TOO                          
         B     TCKEXITN            OTHERWISE ERROR OUT                          
*                                                                               
TCKEXIT5 CLI   0(R5),0             NOT EOR FOR COPY RECORD                      
         BE    TCKEXITN            EOR FOR CURRENT RECORD, ERROR OUT            
         B     TCKEXIT3            OTHERWISE, COMPARE IT                        
*                                                                               
TCKEXITE CR    RB,RB                                                            
         B     CPYHK1X                                                          
TCKEXITN LTR   RB,RB                                                            
         B     CPYHK1X                                                          
*                                                                               
*----------------------------------------------------------------------         
* BUMP TO NEXT ELEMENT, SKIP TAAVEL AND TAACEL                                  
*----------------------------------------------------------------------         
TCKEXIT8 CLI   0(RF),0             EOR                                          
         BER   RE                                                               
         CLI   0(RF),TAAVELQ       SKIP ACCESS VIOLATION                        
         BE    *+10                                                             
         CLI   0(RF),TAACELQ       AND ACTIVITY ELEMENT                         
         BNER  RE                                                               
         IC    R1,1(RF)            BUMP TO NEXT ONE                             
         AR    RF,R1                                                            
         B     TCKEXIT8                                                         
         EJECT                                                                  
***********************************************************************         
* ELEMENT TRAVERSAL ROUTINES                                          *         
***********************************************************************         
FIRSTEL  CLI   0(R5),0                                                          
         BNE   *+10                                                             
         CLI   0(R5),1                                                          
         BR    RE                                                               
         CLI   ELCODE,0                                                         
         BCR   8,RE                                                             
         CLC   ELCODE,0(R5)                                                     
         BCR   8,RE                                                             
NEXTEL   SR    RF,RF                                                            
         IC    RF,1(R5)                                                         
         LTR   RF,RF                                                            
         BNZ   *+6                                                              
         DC    H'0'                                                             
         AR    R5,RF                                                            
         B     FIRSTEL                                                          
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*&&UK                                                                           
ENQZMSG  DC    CL40'+ENQDEQ+ MEDZ LONG UPDATE     (FACPAK)'                     
ENQXMSG  DC    CL40'+ENQDEQ+ MEDZ UPDATE ENDED    (FACPAK)'                     
ENQUMSG  DC    CL40'+ENQDEQ+ MEDZ UNIVERSE UPDATE (FACPAK)'                     
*&&                                                                             
SOONMSG  DC    C'*SOON DUMP* IN SRUPD00 AT'                                     
INVSMSG  DC    C'*SOON DUMP* INVISIBLE REPORT '                                 
VSBLMSG  DC    C'*VISIBLE  U='                                                  
SPACES   DC    CL60' '                                                          
         SPACE 1                                                                
DMREAD   DC    C'DMREAD '                                                       
DMWRT    DC    C'DMWRT  '                                                       
DMADD    DC    C'DMADD  '                                                       
GETREC   DC    C'GETREC '                                                       
PUTREC   DC    C'PUTREC '                                                       
ADDREC   DC    C'ADDREC '                                                       
*                                                                               
BUFFER   DC    CL8'BUFFER'                                                      
GFILE    DC    CL8'GFILE'                                                       
INDEX    DC    CL8'INDEX'                                                       
RANDOM   DC    CL8'RANDOM'                                                      
READ     DC    CL8'READ'                                                        
PRTQUE   DC    CL8'PRTQUE'                                                      
UNKEEP   DC    CL8'UNKEEP'                                                      
VISIBL   DC    CL8'VISIBLE'                                                     
ERROR    DC    CL8'ERROR'                                                       
*                                                                               
GENDIR   DC    C'GENDIR '                                                       
GENFILE  DC    C'GENFILE'                                                       
CTFILE   DC    C'CTFILE '                                                       
FACWRK   DC    C'FACWRK '                                                       
DMRDHI   DC    C'DMRDHI '                                                       
DMRSEQ   DC    C'DMRSEQ '                                                       
DMUNLK   DC    C'DMUNLK '                                                       
*                                                                               
PGMLIST  DC    (PGMLSTX-PGMLSTD)X'00'                                           
         ORG   PGMLIST                                                          
MYPGMLST DC    CL7'SRUPD00',X'14',X'0D',X'00',AL1(000),9X'00'                   
         ORG                                                                    
*                                                                               
* FORCE AN ASSEMBLY ERROR IF WE NEED ANOTHER BASE REGISTER                      
         DC    (2*4096-(*-UPD))X'00'  ROUND UP TO 8K BOUNDARY                   
         EJECT                                                                  
***********************************************************************         
* FIRSTIME (CALLED BEFORE 1ST UPDATE)                                 *         
* R2 = KEY OF WORKER FILE                                                       
***********************************************************************         
FIRSTIME NTR1  BASE=*,LABEL=*                                                   
*&&US                                                                           
         USING UKRECD,R2                                                        
         LA    R1,DDUMPTAB         SEE TO DUMP WHEN DUPLICATE                   
FIRST10  CLC   UKSYSPRG,0(R1)                                                   
         BNE   FIRST15             EXECUTE USER EXIT                            
         GOTO1 VGETFACT,DMCB,(X'80',0),F#DDUMP                                  
         B     FIRST20                                                          
*                                                                               
FIRST15  LA    R1,3(R1)                                                         
         CLI   0(R1),0                                                          
         BNE   FIRST10                                                          
         DROP  R2                                                               
*                                                                               
FIRST20  DS    0H                                                               
*&&                                                                             
*&&UK                                                                           
         CLI   RSYS,X'14'          IS THIS MEDZ UPDATE                          
         BNE   FIRSTX                                                           
         CLC   RECCOUNT,=F'1000'   IS IT > 1000 RECS                            
         BL    FIRSTX                                                           
         MVC   OPMSG(L'ENQZMSG),ENQZMSG     TELL OPS MVS ABOUT IT               
         OC    OPMSG,SPACES                                                     
         MVC   OPFACID+4(3),SYSNAME                                             
         BAS   RE,WTO                                                           
         OI    FLAG,FLENQZQ        FLAG MESSAGE OUTPUT                          
*&&                                                                             
FIRSTX   J     UPDATEX                                                          
*                                                                               
DDUMPTAB DS    0F                                                               
         DC    C'A21'                                                           
         DC    C'A23'                                                           
         DC    C'A27'                                                           
         DC    C'A29'                                                           
         DC    C'A55'                                                           
         DC    C'A56'                                                           
         DC    H'0'                                                             
         EJECT                                                                  
***********************************************************************         
* LASTIME (CALLED AFTER LAST UPDATE)                                  *         
***********************************************************************         
LASTIME  NTR1  BASE=*,LABEL=*                                                   
*&&UK                                                                           
         TM    FLAG,FLENQZQ        DID WE OUTPUT A MESSAGE                      
         BNO   LASTIME2                                                         
         MVC   OPMSG,ENQXMSG       TELL OPS MVS WE'VE FINISHED                  
         BAS   RE,WTO                                                           
LASTIME2 TM    FLAG,FLUNVZQ        WERE UNIVERSES ADDED/CHANGED                 
         BNO   UPDATEX                                                          
         MVC   OPMSG,ENQUMSG       TELL OPS MVS ABOUT IT                        
         BAS   RE,WTO                                                           
*&&                                                                             
LASTX    J     UPDATEX                                                          
         EJECT                                                                  
***********************************************************************         
* WRITE TO OPERATOR                                                   *         
***********************************************************************         
WTO      NTR1  BASE=*,LABEL=*                                                   
         OC    OPMSG,SPACES                                                     
         MVC   OPFACID+4(3),SYSNAME                                             
         GOTO1 VTICTOC,DUB,C'SSET' SUSPEND TIMERS                               
         GOTO1 VDMOD000,PARML,VWCTYPE,OPMSG,OPMSGL,C'LVL1'                      
         GOTO1 VTICTOC,DUB,C'RSET' RESET TIMERS                                 
         J     UPDATEX                                                          
         EJECT                                                                  
***********************************************************************         
* INVOICE MATCHING CHECK IF INVOICE ON FILE WAS MARKED PAID                     
*         IF YES - TURN ON PAID BIT IN FWK COPY AND RECOMPARE TO FILE           
***********************************************************************         
                                                                                
SI2EXIT  NTR1  BASE=*,LABEL=*      COPY HOOK                                    
*                                                                               
         CLI   DMFLTYP,DMFLDA      TEST DIRECT ACCESS (FILE NOT DIR)            
         BNE   SI2EXITN            RETURN NEQ                                   
*                                                                               
         L     R4,AIOA2            IF INVOICE ON FILE IS MKD PAID               
         CLC   0(2,R4),=X'0E03'    MAKE SURE IT'S AN INVOICE RECORD             
         BNE   SI2EXITN            RETURN NEQ                                   
*                                                                               
         LA    R4,42(R4)           FIRST ELEM                                   
SI2X10   CLI   0(R4),X'10'         LOOK FOR HEADER ELEM                         
         BE    SI2X20                                                           
         ZIC   R1,1(R4)                                                         
         AR    R4,R1                                                            
         CLI   0(R4),0                                                          
         BE    SI2EXITN                                                         
         B     SI2X10                                                           
*                                                                               
         USING SNVHDELD,R4                                                      
SI2X20   TM    SNVHDCTL,SNVHDPDQ   TEST MARKED PAID                             
         BNO   SI2EXITN            EXIT NEQ                                     
*                                                                               
         L     R4,AIOA3            CHECK IF FWK COPY IS PAID                    
         CLC   0(2,R4),=X'0E03'    MAKE SURE IT'S AN INVOICE RECORD             
         BNE   SI2EXITN            RETURN NEQ                                   
*                                                                               
         LA    R4,42(R4)           FIRST ELEM                                   
SI2X30   CLI   0(R4),X'10'         LOOK FOR HEADER ELEM                         
         BE    SI2X40                                                           
         ZIC   R1,1(R4)                                                         
         AR    R4,R1                                                            
         CLI   0(R4),0                                                          
         BE    SI2EXITN                                                         
         B     SI2X30                                                           
*                                                                               
         USING SNVHDELD,R4                                                      
SI2X40   TM    SNVHDCTL,SNVHDPDQ   TEST MARKED PAID                             
         BO    SI2EXITN            ALREADY ON - EXIT NEQ                        
*                                                                               
         OI    SNVHDCTL,SNVHDPDQ   TURN ON IN FWK AND RECOMPARE                 
         DROP  R4                                                               
*                                                                               
         L     R0,AIOA3            COMPARE FILE TO FWK COPY                     
         L     RE,AIOA2                                                         
         SR    R1,R1                                                            
         ICM   R1,3,RECLEN                                                      
         LR    RF,R1                                                            
         CLCL  R0,RE                                                            
         BE    SI2EXITE                                                         
*                              *** CODE FOR EXTENDING PUTRECS                   
         CHI   R1,4                                                             
         BNH   SI2EXITN            MAKE SURE NOT PAST END                       
*                                                                               
         OC    0(4,RE),0(RE)       FILE  MUST HAVE ZEROS                        
         BNZ   SI2EXITN                                                         
         LR    R1,R0                                                            
         CLC   =XL3'FFFFFF',0(R1)                                               
         BNE   SI2EXITN            FACWK MUST HAVE FFS                          
*                                                                               
         LR    R1,RF               RESTORE LENGTH                               
         AHI   R0,4                                                             
         AHI   RE,4                GO PAST OFFENDING FFS                        
         AHI   R1,-4                                                            
         AHI   RF,-4                                                            
         CLCL  R0,RE               COMPARE REST OF RECORD                       
         BE    SI2EXITE                                                         
         B     SI2EXITN                                                         
*                                                                               
SI2EXITE CR    RB,RB                                                            
         B     *+6                                                              
SI2EXITN LTR   RB,RB                                                            
         XIT1                                                                   
         LTORG                                                                  
*                                                                               
***********************************************************************         
* INVOICE MATCHING CHECK IF INVOICE ON FILE WAS MARKED PAID                     
*         IF YES - TURN ON PAID BIT IN FWK COPY AND RECOMPARE TO FILE           
***********************************************************************         
                                                                                
SI2EXIT2 NTR1  BASE=*,LABEL=*      CHANGE HOOK                                  
*                                                                               
         CLI   DMFLTYP,DMFLDA      TEST DIRECT ACCESS (FILE NOT DIR)            
         BNE   SI2EXT2X            RETURN                                       
*                                                                               
         L     R4,AIOA2            IF INVOICE ON FILE IS MKD PAID               
         CLC   0(2,R4),=X'0E03'    MAKE SURE IT'S AN INVOICE RECORD             
         BNE   SI2EXT2X            RETURN                                       
*                                                                               
         LA    R4,42(R4)           FIRST ELEM                                   
SI2X100  CLI   0(R4),X'10'         LOOK FOR HEADER ELEM                         
         BE    SI2X200                                                          
         ZIC   R1,1(R4)                                                         
         AR    R4,R1                                                            
         CLI   0(R4),0                                                          
         BE    SI2EXT2X                                                         
         B     SI2X100                                                          
*                                                                               
         USING SNVHDELD,R4                                                      
SI2X200  TM    SNVHDCTL,SNVHDPDQ   TEST MARKED PAID                             
         BNO   SI2EXT2X                                                         
*                                                                               
         L     R4,AIOA1            THEN TURN ON BIT IN FWK CHG                  
         CLC   0(2,R4),=X'0E03'    MAKE SURE IT'S AN INVOICE RECORD             
         BNE   SI2EXT2X            RETURN                                       
*                                                                               
         LA    R4,42(R4)           FIRST ELEM                                   
SI2X300  CLI   0(R4),X'10'         LOOK FOR HEADER ELEM                         
         BE    SI2X400                                                          
         ZIC   R1,1(R4)                                                         
         AR    R4,R1                                                            
         CLI   0(R4),0                                                          
         BE    SI2EXT2X                                                         
         B     SI2X300                                                          
*                                                                               
         USING SNVHDELD,R4                                                      
SI2X400  TM    SNVHDCTL,SNVHDPDQ   TEST MARKED PAID                             
         BO    SI2EXT2X            ALREADY ON - EXIT                            
*                                                                               
         OI    SNVHDCTL,SNVHDPDQ   TURN ON IN FWK CHANGE TO WRITE OUT           
         DROP  R4                                                               
*                                                                               
SI2EXT2X XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* ADD ACC TRANSACTIONS TO FILE VIA ADDTRN TO ENSURE UPDATING OF       *         
*         BALANCE AND HISTORY BUCKETS                                 *         
***********************************************************************         
*&&US                                                                           
BILEXIT3 NTR1  BASE=*,LABEL=*                                                   
         LA    R6,ACTRNBLK                                                      
         USING TRNBLK,R6                                                        
         NI    FLAG,X'FF'-(FLACBYP)                                             
         TM    ACFLG,ACPOST        TEST POSTING ENTRIES BEING PROCESSED         
         BO    ACTRN3              YES, MSUT BE USING AU UPDATE                 
         CLI   RFILTY,X'6A'        TEST ACCMST                                  
         BNE   ACTRNNO             NO, SKIP IT                                  
         L     R2,AIOA1                                                         
         USING TRNRECD,R2                                                       
         CLI   TRNRFST,TRNELQ      TEST TRANSACTION RECORD                      
         BE    ACTRN9              NO - SKIP BALANCES, HISTORIES, ETC.          
         B     ACTRNYES            PROCESS AS USUAL                             
*                                                                               
ACTRN3   CLI   RFILTY,X'6A'        TEST ACCMST                                  
         BNE   ACTRNNO             NO, SKIP IT                                  
         L     R2,AIOA1                                                         
         USING TRNRECD,R2                                                       
         CLI   TRNRFST,TRNELQ      TEST TRANSACTION RECORD                      
         BNE   ACTRNNO             NO - SKIP BALANCES, HISTORIES, ETC.          
         MVI   TRNKSBR,0           ADDTRN WILL SET SUB-REF                      
         NI    TRNRSTAT,X'FF'-(TRNSREVS)                                        
*                                                                               
         LA    R2,TRNRFST                                                       
         USING TRNELD,R2                                                        
         MVI   TRNSUB,0                                                         
         NI    TRNSTAT,X'FF'-(TRNSREV)                                          
*                                                                               
         USING TRSELD,R2                                                        
ACTRN5   SR    R1,R1                                                            
         IC    R1,TRSLN                                                         
         AR    R2,R1                                                            
         CLI   TRSEL,TRSELQ                                                     
         BE    ACTRN7                                                           
         CLI   TRSEL,0                                                          
         BNE   ACTRN5                                                           
         DC    H'0'                                                             
*                                                                               
ACTRN7   XC    TRSREVD,TRSREVD     CLEAR REVERSED INFO                          
         XC    TRSRMOS,TRSRMOS                                                  
*                                                                               
ACTRN9   MVC   TRNCACNM,SPACES                                                  
         L     R2,AIOA1                                                         
         USING TRNRECD,R2                                                       
         LA    R2,TRNRFST                                                       
         USING FFTELD,R2                                                        
ACTRN11  SR    R1,R1                                                            
         IC    R1,FFTLN                                                         
         AR    R2,R1                                                            
         CLI   FFTEL,FFTELQ                                                     
         BE    ACTRN13                                                          
         CLI   FFTEL,0                                                          
         BNE   ACTRN11                                                          
         B     ACTRN15                                                          
*                                                                               
ACTRN13  CLI   FFTTYPE,FFTTPNAM    FIND CONTRA NAME ELEMENT                     
         BNE   ACTRN11                                                          
         IC    R1,FFTDLEN                                                       
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   TRNCACNM(0),FFTDATA                                              
         MVI   FFTEL,X'FF'         DELETE THE ELEMENT                           
         L     R5,AIOA1                                                         
         GOTO1 VHELLO,DMCB,(C'D',FILENAME),(X'FF',(R5)),0,0                     
*                                                                               
ACTRN15  OC    ADADDTRN,ADADDTRN   TEST ADDTRN INITIALIZED                      
         BNZ   ACTRN19             YES,                                         
*                                                                               
         GOTO1 VCALLOVL,DUB,0,X'D9000A63'  GET A(ADDTRN)                        
         L     RF,0(R1)                                                         
         STCM  RF,15,ADADDTRN                                                   
*                                                                               
         LA    R2,KEY              GET COMPANY RECORD                           
         USING CPYRECD,R2                                                       
         MVI   CPYKEY,C' '                                                      
         MVC   CPYKEY+1(L'CPYKEY-1),CPYKEY                                      
         L     RF,AIOA1                                                         
         MVC   CPYKCPY,0(RF)       SET COMPANY KEY                              
         GOTO1 VDATAMGR,DMCB,DMREAD,ACCDIR,KEY,DKEY                             
         CLI   0(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    R2,DKEY                                                          
         GOTO1 VDATAMGR,DMCB,GETREC,ACCMST,CPYKDA,AIOA2,IOWORK                  
         CLI   0(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
                                                                                
         L     R2,AIOA2                                                         
         LA    R2,CPYRFST                                                       
         USING CPYELD,R2                                                        
         XR    R0,R0                                                            
ACTRN17  CLI   CPYEL,CPYELQ        GET COMPANY ELEMENT                          
         BE    ACTRN18                                                          
         CLI   CPYEL,0                                                          
         BNE   *+6                                                              
         DC    H'0'                                                             
         IC    R0,CPYLN                                                         
         AR    R2,R0                                                            
         B     ACTRN17                                                          
*                                                                               
ACTRN18  MVC   TRNCPYS1,CPYSTAT1   SET COMPANY STATUS                           
         MVC   TRNCPYS2,CPYSTAT2                                                
         MVC   TRNCPYS3,CPYSTAT3                                                
         MVC   TRNCPYS4,CPYSTAT4                                                
         MVC   TRNCPYS5,CPYSTAT5                                                
         MVC   TRNCPYS6,CPYSTAT6                                                
         MVC   TRNCPYS7,CPYSTAT7                                                
         MVC   TRNCPYS8,CPYSTAT8                                                
         MVI   TRNCPYS9,0                                                       
         MVI   TRNCPYSA,0                                                       
         CLI   CPYLN,CPYLN3Q                                                    
         BL    ACTRN18A                                                         
         MVC   TRNCPYS9,CPYSTAT9                                                
         MVC   TRNCPYSA,CPYSTATA                                                
*                                                                               
ACTRN18A CLI   CPYLN,CPYLN4Q                                                    
         BL    *+10                                                             
         MVC   TRNGLMOA,CPYGLMOA                                                
         MVC   TRNREC,AIOA1        A(TRANSACTION RECORD)                        
         GOTO1 VDATCON,DMCB,(5,0),(1,TRNPDAT1)  TODAY'S DATE                    
         GOTO1 VDATCON,DMCB,(5,0),(2,TRNPDAT2)                                  
         MVI   TRNCTRY,CTRYUSA     COUNTRY                                      
         MVI   TRNMODE,TRNMONLN    ONLINE                                       
         MVC   TRNCOMF,SRPAR4      A(COMFACS)                                   
         OI    TRNINDS,TRNICONV+TRNIDUCN                                        
         LA    R0,PALAREA                                                       
         STCM  R0,15,TRNPAL                                                     
*                                                                               
         USING TRNRECD,R2                                                       
ACTRN19  L     R2,AIOA1                                                         
         LA    R2,TRNRFST                                                       
                                                                                
         USING TRSELD,R2                                                        
ACTRN20  SR    R1,R1                                                            
         IC    R1,TRSLN                                                         
         AR    R2,R1                                                            
         CLI   TRSEL,TRSELQ                                                     
         BE    ACTRN21                                                          
         CLI   TRSEL,0                                                          
         BNE   ACTRN20                                                          
         B     ACTRN22                                                          
*                                                                               
ACTRN21  MVC   TRNPUSER,TRSUSER    SAVE USERID                                  
*                                                                               
ACTRN22  OI    ACFLG,ACTLAST       SET LAST TIME FLAG                           
         OI    TRNINDS2,TRNIADDG                                                
         GOTOR ADADDTRN,TRNBLK                                                  
         BE    ACTRNNO                                                          
         LLC   RF,TRNERRS          RF = ERROR CODE                              
         LLC   R0,TRNGLER#         R0 = GL ERROR#, IF RF=6                      
         DC    H'00'               ERROR HAPPENED                               
*                                                                               
ACTRNNO  OI    FLAG,FLACBYP        SET 'BYPASS RECORD'                          
ACTRNYES DS    0H                                                               
ACTRNX   XIT1                                                                   
*                                                                               
         DROP  R2,R6                                                            
*                                                                               
ACCDIR   DC    CL8'ACCDIR'                                                      
ACCMST   DC    CL8'ACCMST'                                                      
*                                                                               
         LTORG                                                                  
         DROP  RB                                                               
*&&                                                                             
         EJECT                                                                  
***********************************************************************         
* Change the status of the associate print queue report               *         
*   1. Make the report visible                                        *         
*   2. If the update failed, turn the error status bit on             *         
***********************************************************************         
PQSTCHG  NTR1  BASE=*,LABEL=*                                                   
         L     R3,0(R1)            R3 = ACTION PARAMETER                        
         SR    R4,R4                                                            
         CLC   =C'TBA',PQSYSPRG    Special SPOT traffic                         
         BNE   PQSTCH02                                                         
         OC    PQCINUM,PQCINUM     IS PQ C/I INFORMATION PRESENT?               
         BNZ   PQSTCH08                                                         
         B     PQSTCHGX            NO: EXIT                                     
*                                                                               
PQSTCH02 CLC   =C'SB1',PQSYSPRG    SPOT BILLING?                                
         BE    *+14                                                             
         CLC   =C'SBU',PQSYSPRG    NET BILLING (NBU)?                           
         BNE   PQSTCH08                                                         
         LA    R4,3                LOOP 3 TIMES                                 
         XC    SMFREC,SMFREC       CLEAR SMF RECORD                             
SMF      USING TRAKD,SMFREC                                                     
*                                                                               
PQSTCH08 MVC   PQUSRID2,PQUSERID   SAVE USERID                                  
         XC    PQREPRNO,PQREPRNO   CLEAR 2ND REP REF NUMBER                     
*                                                                               
PQSTCH10 OC    PQCINUM,PQCINUM     IS PQ C/I INFORMATION PRESENT?               
         BZ    PQSTCHE1            NO: EXIT                                     
*                                                                               
         L     R0,ACIREC                                                        
         LHI   R1,L'PQBUFF                                                      
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE               CLEAR PQBUFF                                 
*                                                                               
PKNDX    USING PKINDEX,NDX                                                      
         XC    NDX,NDX             GET PRTQ FILE ID FROM WKPQ                   
         MVC   PKNDX.PKSRCID,PQUSERID                                           
         CLI   WKPQ#FLG,NO         REPORT # or CI                               
         BE    PQSTCH12            CI                                           
         MVC   PKNDX.PKREPNO,PQREP#    Much easier (direct locate)              
         MVC   PRTQID,=CL8'PRTQUE'     Prime for DATAMGR                        
         OC    PQREPRNO,PQREPRNO       Any 2nd rep ref number?                  
         BZ    PQSTCH20                                                         
         MVC   PKNDX.PKREPNO,PQREPRNO  Yes - use it instead                     
         B     PQSTCH20                                                         
                                                                                
PQSTCH12 GOTO1 VDATAMGR,PQDMCB,(0,=C'GFILE'),=C'PRTQUE',NDX,0,ACIREC            
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   PRTQID,PKNDX.PKUSRINF   PRIME THE PQ BUFFER                      
         GOTO1 VDATAMGR,PQDMCB,(0,=C'BUFFER'),PRTQID,0,0,ACIREC                 
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         XC    NDX,NDX                                                          
         OC    PQREPRNO,PQREPRNO       Any 2nd rep ref number?                  
         BZ    PQSTCH18                No - Get it from C/I number              
         MVC   PKNDX.PKREPNO,PQREPRNO  Yes - use it directly                    
         B     PQSTCH20                                                         
*                                                                               
PQSTCH18 L     RE,ACIREC                                                        
         MVC   CIDATA,12(RE)       RETRIEVE CIDATA FROM BUFFER                  
         SR    RE,RE               PREPARE FOR DIVIDE                           
         LH    RF,PQCINUM          C/I NUMBER OF ASSOCIATED PQ REPORT           
         BCTR  RF,0                                                             
         LH    R1,CITRKS           NUMBER OF TRACKS PER PART 1 C/I              
         DR    RE,R1               RF = PQ REFERENCE NUMBER                     
         STCM  RF,3,PKNDX.PKREPNO  REF. NUMBER OF ASSOCIATED RPT                
*                                                                               
PQSTCH20 MVC   PKNDX.PKSRCID,PQUSERID USERID OF ASSOCIATED PQ REPORT            
         OI    PKNDX.PKFLAG,X'04'  DIRECT LOCATE BY REF. NUMBER                 
         GOTO1 VDATAMGR,PQDMCB,(0,=C'INDEX'),PRTQID,NDX,SAVE,ACIREC,0           
         CLI   8(R1),0                                                          
         BNE   PQSTCHE2            REPORT MIGHT HAVE BEEN PURGED                
***********************************************************************         
*  Extract 2nd rep's key from 1st rep'S description filed                       
***********************************************************************         
         CHI   R4,3                                                             
         BNE   PQSTCH30                                                         
         XC    SAVE(SAVELQ),SAVE                                                
         MVI   SAVE+4,C'L'                                                      
         GOTO1 VDATAMGR,PQDMCB,(0,=C'RANDOM'),PRTQID,NDX,SAVE,ACIREC            
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                             1ST SUBID = 2ND SUBID?            
         CLC   QLSUBID-PQPLD+SAVE,QLDESC+4-PQPLD+SAVE                           
         BE    *+10                           YES - THERE IS 2ND REP            
         SR    R4,R4                          NO  - NO 2ND REPORT               
         B     PQSTCH50                                                         
*                                                                               
         GOTO1 VHEXIN,DMCB,QLDESC-PQPLD+SAVE,PQUSERID,4                         
         ICM   RE,15,DMCB+12                                                    
         BNZ   *+10                                                             
         SR    R4,R4               NO 2ND REPORT                                
         B     PQSTCH50                                                         
*                                                                               
         GOTO1 VHEXIN,DMCB,QLDESC+7-PQPLD+SAVE,PQREPRNO,4                       
         ICM   RE,15,DMCB+12                                                    
         BNZ   *+6                                                              
         SR    R4,R4               NO 2ND REPORT                                
         B     PQSTCH50                                                         
***********************************************************************         
*Read 2nd rep's description field and switch back to 1st PQ report key          
***********************************************************************         
PQSTCH30 DS    0H                                                               
         CHI   R4,2                                                             
         BNE   PQSTCH40                                                         
         XC    SAVE(SAVELQ),SAVE                                                
         MVI   SAVE+4,C'L'                                                      
         GOTO1 VDATAMGR,PQDMCB,(0,=C'RANDOM'),PRTQID,NDX,SAVE,ACIREC            
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   PQDESCR,QLDESC-PQPLD+SAVE                                        
***********************************************************************         
* Need to use PQUSERID to read the id name for the SMF record                   
***********************************************************************         
         USING CTIREC,R5                                                        
         LA    R5,KEY              READ ID RECORD                               
         XC    CTIKEY,CTIKEY                                                    
         MVI   CTIKTYP,CTIKTYPQ                                                 
         MVC   CTIKNUM,PQUSERID                                                 
         GOTO1 VDATAMGR,DMCB,DMREAD,CTFILE,KEY,IOA1                             
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    R5,IOA1+CTIDATA-CTIREC                                           
         SR    R0,R0                                                            
*                                                                               
         USING CTDSCD,R5                                                        
PQSTCH35 CLI   CTDSCEL,0           END OF RECORD?                               
         BNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLI   CTDSCEL,CTDSCELQ                                                 
         BE    *+14                                                             
         IC    R0,CTDSCLEN         TRY NEXT ELEMENT                             
         AR    R5,R0                                                            
         B     PQSTCH35                                                         
*                                                                               
         MVC   SMF.TRAKUID,CTDSC   USERID NAME                                  
         DROP  R5                                                               
*                                                                               
         MVC   PQUSERID,PQUSRID2                                                
         XC    PQREPRNO,PQREPRNO   CLEAR 2ND REP REF NUMBER                     
         B     PQSTCH50                                                         
*                                                                               
PQSTCH40 DS    0H                                                               
         CHI   R4,1                CHANGE DESC                                  
         BNE   PQSTCH50                                                         
         XC    SAVE(SAVELQ),SAVE                                                
         MVC   QLDESC-PQPLD+SAVE,PQDESCR                                        
         GOTO1 VDATAMGR,PQDMCB,(0,=C'DESC'),PRTQID,NDX,SAVE,ACIREC              
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LHI   RE,L'SMFREC                                                      
         STCM  RE,3,SMF.TRAKLN       SFM RECORD LENGTH                          
         MVI   SMF.TRAKTYP,TRAKTYPB  SFM REC TYPE = SP BILL SOON CDROM          
         MVC   SMF.TRAKJIB,PQDESCR                                              
         GOTO1 VDATCON,DMCB,(5,0),(10,SMF.TRAKDAT)   TODAY'S DATE               
*                                                                               
         TIME  DEC                                                              
         ST    R0,FULL                                                          
         MVI   FULL+3,X'0F'                                                     
         UNPK  DUB(7),FULL                                                      
         MVC   SMF.TRAKTIM(2),DUB                                               
         MVI   SMF.TRAKTIM+2,C'.'                                               
         MVC   SMF.TRAKTIM+3(2),DUB+2                                           
         MVI   SMF.TRAKTIM+2,C'.'                                               
         MVC   SMF.TRAKTIM+6(2),DUB+4                                           
*                                                                               
         GOTOR VSMFOUT,SMFPARM,7,SMF.TRAKD                                      
         OC    SMFP1,SMFP1                                                      
         BZ    PQSTCH90                                                         
         WTO   'SMF#ERR - SRUPD00 SMFOUT ERROR track postings'                  
         B     PQSTCH90            DON'T NEED TO MARK VISI/ERROR - DONE         
         DROP  SMF                                                              
*                                                                               
PQSTCH50 DS    0H                                                               
         GOTO1 VDATAMGR,PQDMCB,(0,=C'VISIBLE'),PRTQID,NDX,SAVE,ACIREC           
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTOR CHKVISB             SEE IF REALLY VISIBLE                        
*                                                                               
PQSTCH55 CHI   R3,1                UPDATE FAILED?                               
         BNE   PQSTCH60            NO                                           
         GOTO1 VDATAMGR,PQDMCB,(0,=C'ERROR'),PRTQID,NDX,SAVE,ACIREC             
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
PQSTCH60 DS    0H                                                               
         LTR   R4,R4                                                            
         BZ    PQSTCH90                                                         
         BCT   R4,PQSTCH10                                                      
*                                                                               
PQSTCH90 DS    0H                                                               
*&&DO                                                                           
*Deactivate this for now per ahyd, apr08/2014, yyun                             
***********************************************************************         
* Reports that don't set the updative bit on in the pq report.                  
* Send a warning e-mail to team for each FACWRK file that is associated         
* with a print queue report that doesn't have the updative bit on.              
***********************************************************************         
         TM    PKNDX.PKTYPE,PKTYPUPQ   PQ RPT MARKED OF TYPE UPDATIVE?          
         BO    PQSTCHGX                YES: OKAY                                
         SR    RF,RF                                                            
         ICM   RF,3,PKNDX.PKSRCID                                               
         CVD   RF,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  MSGPQCID,DUB                                                     
         MVC   MSGPQSUB,PKNDX.PKSUBID                                           
         SR    RF,RF                                                            
         ICM   RF,3,PKNDX.PKREPNO                                               
         CVD   RF,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  MSGPQRP#,DUB                                                     
         GOTO1 VDATAMGR,DMCB,=C'OPMSG',(L'MSGPQ1,MSGPQ1)                        
         DROP  PKNDX                                                            
*&&                                                                             
         B     PQSTCHGX                                                         
                                                                                
PQSTCHE1 LA    R1,1                ERROR 1, MISSING PQ CI NUMBER                
         B     PQSTCHER                                                         
PQSTCHE2 LA    R1,2                ERROR 2, REPORT NOT FOUND ON QUEUE           
                                                                                
PQSTCHER GOTOR OPMSGV                                                           
*                                                                               
PQSTCHGX XIT1  ,                                                                
*                                                                               
MSGPQ1   DC    CL66' '                                                          
         ORG   MSGPQ1                                                           
*&&US*&& DC    C'AUTONOTE*US-MF_Fac_Team_NY:Warn-up SOON. '                     
*&&UK*&& DC    C'AUTONOTE*US-MF_Fac_Team_UK:Warn-up SOON. '                     
         DC    C'SEE PQ '                                                       
MSGPQCID DC    C'NNNNN'                                                         
         DC    C','                                                             
MSGPQSUB DC    C'XXX'                                                           
         DC    C','                                                             
MSGPQRP# DC    C'NNNNN'                                                         
         ORG                                                                    
         LTORG                                                                  
         DC    C'SEE FWK '                                                      
         EJECT                                                                  
***********************************************************************         
* Change the status of the associate print queue report               *         
*   1. Make the report of 2nd report visible - laser checks register  *         
***********************************************************************         
PQST2CHG NTR1  BASE=*,LABEL=*                                                   
         OC    PQIDL,PQIDL         TEST PRTQUE KEY                              
         BZ    PQST2CHX                                                         
         L     R0,ACIREC           CLEAR BUFFER                                 
         LHI   R1,L'PQBUFF                                                      
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
         XC    NDX,NDX             FIND PRTQ ID FOR USER REPORT                 
         L     R5,ACIREC                                                        
         MVC   NDX(2),PQIDL                                                     
         GOTO1 VDATAMGR,PQDMCB,(X'00',GFILE),PRTQUE,NDX,SAVE,(R5)               
         MVC   PRTQID,NDX+(PKUSRINF-PKINDEX)                                    
*                                                                               
         GOTO1 VDATAMGR,PQDMCB,(X'00',BUFFER),PRTQID,NDX,SAVE,(R5),0            
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   SAVEBUFF,0(R5)      SAVE RETURN DATA IN BUFFER                   
*                                                                               
         USING PKRECD,R5                                                        
         LA    R5,NDX              SEARCH PRTQUE INDEX FOR REPORT               
         XC    NDX,NDX             FIND PRTQ ID FOR USER REPORT                 
         MVC   PKKEY,PQIDL         SET REPORT ID IN USER INDEX                  
         OI    PKFLAG,X'0C'        PASS TEMPS/DIRECT LOCATE                     
*                                                                               
         GOTO1 VDATAMGR,PQDMCB,(X'08',INDEX),PRTQID,NDX,SAVE,ACIREC,0           
         CLI   8(R1),0                                                          
         BE    PQST2C10                                                         
         BRAS  RE,DEATH                                                         
         DC    H'0'                ERR - DIE ON INDEX READ DISK ERROR           
*                                                                               
*        MAKE REPORT VISIBLE                                                    
*                                                                               
PQST2C10 GOTO1 VDATAMGR,PQDMCB,(X'00',VISIBL),PRTQID,NDX,SAVE,ACIREC,0          
         CLI   8(R1),0                                                          
         BE    PQST2C20                                                         
         BRAS  RE,DEATH                                                         
         DC    H'0'                DIE IF CANT READ                             
                                                                                
PQST2C20 SR    R1,R1                                                            
         GOTOR CHKVISB                                                          
                                                                                
PQST2CHX XIT1                                                                   
         DROP  R5                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* CHECK TO SEE IF REPORT WAS ACTUALLY MADE VISABLE                    *         
***********************************************************************         
                                                                                
CHKVISB  NTR1  BASE=*,LABEL=*                                                   
         MVC   SVNDX,NDX           SAVE PRIOR NDX OF PQ REPORT                  
                                                                                
         XC    KEY,KEY             READ ANY RECORD TO FLUSH PQ BUFFERS          
         MVI   KEY,X'01'                                                        
         GOTOR VDATAMGR,DMCB,DMRDHI,GENDIR,KEY,GDIR                             
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         MVC   GDA,GDIR+36                                                      
         GOTO1 VDATAMGR,DMCB,GETREC,GENFILE,GDA,IOA1,WORK                       
                                                                                
         L     R0,ACIREC           CLEAR BUFFER                                 
         LHI   R1,L'PQBUFF                                                      
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
                                                                                
         GOTO1 VGETFACT,DMCB,(X'80',=F'7680'),F#WAIT 1/5 SECOND                 
                                                                                
PRIOR    USING PKINDEX,SVNDX                                                    
PKNDX    USING PKINDEX,NDX                                                      
         L     R5,ACIREC                                                        
         XC    NDX,NDX             FIND PRTQ# BASED ON USER ID                  
         MVC   PKNDX.PKSRCID,PRIOR.PKSRCID                                      
         GOTO1 VDATAMGR,PQDMCB,(X'00',GFILE),PRTQUE,NDX,SAVE,(R5)               
*                                                                               
         MVC   PRTQID,PKNDX.PKUSRINF                                            
         GOTO1 VDATAMGR,PQDMCB,(X'00',BUFFER),PRTQID,NDX,SAVE,(R5),0            
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   SAVEBUFF,0(R5)      SAVE RETURN DATA IN BUFFER                   
*                                                                               
         XC    NDX,NDX             FIND PRTQ ID FOR USER REPORT                 
         MVC   PKNDX.PKREPNO,PRIOR.PKREPNO   REPORT NUMBER                      
         MVC   PKNDX.PKSRCID,PRIOR.PKSRCID   PQ USER ID                         
         OI    PKNDX.PKFLAG,X'04'  DIRECT LOCATE BY REF. NUMBER                 
*                                                                               
         GOTO1 VDATAMGR,PQDMCB,(X'08',INDEX),PRTQID,NDX,SAVE,ACIREC,0           
         CLI   8(R1),0                                                          
         BNE   CHKVISBX               WHO KNOWS                                 
         TM    PKNDX.PKSTAT,PKSINVB   IS IT MARK INVISIBLE                      
         BZ    CHKVISBX               NO SO THIS IS OK                          
         SR    R1,R1               SET TO ERROR 0, STILL INVISIBLE              
         GOTOR OPMSGV              SEND TO CONSOLE                              
*                                  TRY MAKING VISIBLE AGAIN                     
                                                                                
         GOTO1 VDATAMGR,PQDMCB,(0,=C'VISIBLE'),PRTQID,NDX,SAVE,ACIREC           
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
CHKVISBX XIT1                                                                   
         DROP  PKNDX,PRIOR                                                      
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* WRITE TO OPERATOR THE U=IIIII,CCC,##### OF REPORT JUST MADE VISIBLE *         
***********************************************************************         
                                                                                
OPMSGV   NTR1  BASE=*,LABEL=*                                                   
         LA    R5,SVNDX                                                         
         USING PKRECD,R5                                                        
         MVC   OPMSG,SPACES                                                     
         MVC   OPVTEXT,VSBLMSG                                                  
         MVC   OPVSPP,SVWKSPP                                                   
         MVC   OPVXTXT,=CL16'NOT VISIBLE'                                       
         STC   R1,OPVERR#          Error is 0 to 9 only                         
         OI    OPVERR#,X'F0'                                                    
         CHI   R1,1                                                             
         BL    OPMSGV08                                                         
         MVC   OPVXTXT,=CL16'MISSING PQCINUM'                                   
         BE    OPMSGV08                                                         
         MVC   OPVXTXT,=CL16'REPORT NOT FOUND'                                  
                                                                                
OPMSGV08 MVC   OPVPRTQ#,PRTQID                                                  
*                                                                               
         SR    R1,R1                                                            
         ICM   R1,3,PKSRCID        USER ID                                      
         CVD   R1,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  OPVUSRID(6),DUB                                                  
         MVI   OPVUSRID+L'OPVUSRID,C','                                         
*                                                                               
         SR    R1,R1                                                            
         ICM   R1,3,PKREPNO        REPORT NUMBER                                
         CVD   R1,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  OPVRPTID,DUB                                                     
*                                                                               
OPMSGV30 MVC   OPFACADV(3),SYSNAME                                              
         GOTO1 VTICTOC,DUB,C'SSET' SUSPEND TIMERS                               
         GOTO1 VDMOD000,PARML,VWCTYPE,OPMSG,OPMSGL,C'LVL1'                      
         GOTO1 VTICTOC,DUB,C'RSET' RESET TIMERS                                 
         XIT1  ,                                                                
         DROP  R5                                                               
                                                                                
         LTORG                                                                  
         EJECT                                                                  
**********************************************************************          
         USING FILTABD,R5                                                       
SBUEXIT  NTR1  BASE=*,LABEL=*                                                   
         NI    FLAG,X'FF'-FLBUBYP                                               
*                                                                               
         CLC   =X'0E0A',IOA1       IF NOT UBILL OR PASSIVE                      
         BE    *+14                                                             
         CLC   =X'0E06',IOA1       DO NOT TOUCH                                 
         BNE   SBUEXITE                                                         
*                                                                               
         MVC   XSPKEY,IOA1         SAVE KEY FROM FACKWORK                       
*                                                                               
*  CHECK IF THE RECORD WITH THE SAME KEY HAS ALREADY BEEN ADDED                 
*                                                                               
         GOTO1 VDATAMGR,DMCB,(X'88',DMRDHI),=C'XSPDIR',XSPKEY,AIOA2             
         TM    8(R1),X'FF'-X'02'                                                
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R1,AIOA2                                                         
         CLC   XSPKEY,0(R1)                                                     
         BNE   SBUEXITE           IF REC NOT THERE YET, DO NORMAL PROC          
*                                                                               
         OI    FLAG,FLBUBYP       BYPASS THIS REC IN ADD ROUTINE                
*                                                                               
*   THIS WILL PROCESS THE RECORDS AS CHANGES, AND BYPASS KEY ADDS               
*                                                                               
         CLI   DMFLTYP,DMFLIS     TEST INDEX SEQUENTIAL                         
         BE    SBUEXITE           BYPASS                                        
*                                                                               
*    SIMULATE COPY - THIS 'ADD' IS REALLY A CHANGE                              
*                                                                               
         MVC   SB1DA,36(R1)       SAVE D/A (USE SB1DA SINCE DEFINED)            
         GOTO1 VDATAMGR,DMCB,(X'88',GETREC),FILENAME,SB1DA,AIOA2,IOWORK         
         TM    8(R1),X'FF'-X'02'                                                
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    R4,IOA1+NUBELDQ     GET ADDR OF NEW REC'S 1ST ELEM               
*        LA    R4,42(,R4)          1ST ELEM                                     
         L     R7,AIOA2            OLD - EXISTING RECORD                        
         USING NBILD,R4                                                         
*                                                                               
SBU140   DS    0H                                                               
         CLI   NBILEL,NBILELQ      IS IT X'10' ELEM ?                           
         BNE   SBU150                                                           
         CLI   NBILCHGT,C'T'       IS IT TIME CHARGE ?                          
         BNE   SBU150              MIGHT SKIP THIS ELEM                         
         OC    NBILGRS(L'NBILGRS+L'NBILNET),NBILGRS                             
         BNZ   SBU150                                                           
         TM    NBILST,NBILRLQ      IF NOT REVERSAL                              
         BZ    SBU155              DON'T PUT THIS ZERO TIME ELEM                
         DROP  R4                  IT WAS ADDED 1ST TIME THROUGH                
SBU150   GOTO1 VHELLO,DMCB,(C'P',FILENAME),(R7),(R4),0                          
         CLI   12(R1),0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
SBU155   ZIC   R0,1(R4)                                                         
         AR    R4,R0                                                            
         CLI   0(R4),0                                                          
         BNE   SBU140                                                           
*                                                                               
         GOTO1 VDATAMGR,DMCB,PUTREC,FILENAME,SB1DA,AIOA2,IOWORK                 
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         GOTO1 VDATAMGR,DMCB,DMUNLK,FILENAME  UNLOCK BLOCK                      
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
SBUEXITE XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
**********************************************************************          
* SPOT ORDER ACTIVITY - JOB NAME SPOBA***                                       
* SUMMARY: ALLOW CHANGE IF ONLY X'13' AND X'14' ARE DIFFERENT                   
*                                                                               
* ON ENTRY :  IOA1      COPY RECORD FROM FACWRK                                 
*             IOA2      CURRENT FILE RECORD                                     
*                                                                               
* ON EXIT  :  SOAFLAG   C'S' - SKIP CHANGE                                      
*             SOAFLAG   C'R' - REPLACE                                          
*             DOSPFLG2  IF SOAFLAG IS R, DOSPRSND TURNED ON IN CURRENT          
*                                                                               
**********************************************************************          
         USING UKRECD,R2                                                        
SOAEXIT  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         L     R4,AIOA1            ADDRESS OF COPY IN FWK                       
         CLC   =X'0D34',0(R4)      IF NOT SPOT ORDER RECORD                     
         BNE   SOAEXITN                                                         
         L     RF,AIOA2            ADDRESS OF CURRENT                           
         CLC   0(L'DOKEY,R4),0(RF) MATCH ON KEY?                                
         BNE   SOAEXITN                                                         
*                                                                               
* OUTER LOOP [START] - TO GO THRU ELEMS IN COPY RECORD FROM FACWRK              
*                                                                               
         MVI   SOAFLAG,C'S'        SET (S)KIP CHANGE -                          
         LA    R4,DORFRST-DOKEY(R4)                                             
SOA010   CLI   0(R4),0             E-O-R?                                       
         BE    SOAEXITR             REPLACE IT IN SOAEXIT2                      
         CLI   0(R4),COLELQ        IGNORE X'13' ORDER COLOR ELEMENT             
         BE    SOA050                                                           
         CLI   0(R4),MGCOLELQ      IGNORE X'14' MKGD COLOR ELEMENT              
         BE    SOA050                                                           
*                                                                               
* INNER LOOP [START] - TO GO THRU ELEMS IN CURRENT RECORD                       
*                                                                               
         L     RF,AIOA2            ADDRESS OF CURRENT                           
         LA    RF,DORFRST-DOKEY(RF)                                             
SOA020   CLI   0(RF),0             E-O-R?                                       
         BE    SOAEXITY             SKIP IT IN SOAEXIT2                         
         CLI   0(RF),COLELQ        IGNORE X'13' ORDER COLOR ELEMENT             
         BE    SOA030               SKIP                                        
         CLI   0(RF),MGCOLELQ      IGNORE X'14' MKGD COLOR ELEMENT              
         BE    SOA030               SKIP                                        
*                                                                               
         CLC   0(2,R4),0(RF)       FIND MATCHING ELEM CODE & LEN?               
         BE    SOA040               YES                                         
SOA030   SR    RE,RE                NO, GET THE NEXT ELEM IN CURRENT            
         ICM   RE,1,1(RF)          GET LENTH OF ELEMENT                         
         BZ    SOADUMP              EXIT WITH DUMP                              
         LA    RF,0(RE,RF)         BUMP TO NEXT ELEM                            
         B     SOA020                                                           
*                                                                               
* IF HERE, SEE IF ELEM FROM FWK COPY MATCHES ELEM FROM CURRENT                  
*                                                                               
SOA040   SR    RE,RE               CHECK IF ENTIRE ELEM MATCHES                 
         ICM   RE,1,1(R4)          GET LENGTH OF ELEMENT IN COPY                
         BZ    SOADUMP              EXIT WITH DUMP                              
         BCTR  RE,0                SETUP FOR EX INSTR                           
         CLC   0(0,R4),0(RF)                                                    
         EX    RE,*-6              ELEM IN COPY MATCH ELEM IN CURRENT           
         BNE   SOA030               NO, CHK NEXT ELEM IN CURRENT REC            
*                                                                               
         CLI   0(RF),DOSPELQ       PROC X'03' SUPP ELEM?                        
         BNE   SOA050                NO, CHK NEXT ELEM IN COPY REC              
         OI    DOSPFLG2-DOSPELD(RF),DOSPRSND  UPDATE RESEND FLAG                
*                                                                               
SOA050   SR    RE,RE                                                            
         ICM   RE,1,1(R4)                                                       
         BZ    SOADUMP              EXIT WITH DUMP                              
         LA    R4,0(RE,R4)         BUMP TO NEXT ELEM                            
         B     SOA010                THEN CHK NEXT ELEM IN COPY REC             
*                                                                               
SOADUMP  ICM   RF,15,UKSYSSUBPRG   SO RF IN E-MAIL SHOWS THE FACWRK KEY         
         DC    H'0'                DIE IF DISK ERROR                            
*                                                                               
SOAEXITR MVI   SOAFLAG,C'R'        SET TO (R)EPLACE CHANGE W/CURRENT            
SOAEXITY CR    RB,RB                                                            
         B     *+6                                                              
SOAEXITN LTR   RB,RB                                                            
         XIT1                                                                   
         LTORG                                                                  
         DROP  R2                                                               
         EJECT                                                                  
**********************************************************************          
* SPOT ORDER ACTIVITY - JOB NAME SPOBA***                                       
* SUMMARY: ALLOW CHANGE IF ONLY X'13' AND X'14' ARE DIFFERENT                   
*                                                                               
* ON ENTRY :  IOA1      CHANGE RECORD FROM FACWRK                               
*             IOA2      CURRENT FILE RECORD                                     
*             SOAFLAG   C'S' - SKIP CHANGE                                      
*             SOAFLAG   C'R' - REPLACE                                          
*                                                                               
* ON EXIT  :  CURRENT RECORD IN IOA2 COPIED TO IOA1 FOR WRITE                   
*                                                                               
**********************************************************************          
         USING UKRECD,R2                                                        
SOAEXIT2 NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         CLI   RFILTY,X'21'        SPOTFILE                                     
         BNE   *+10                                                             
         CLC   =X'0D34',IOA1       SPOT ORDER RECORD                            
         BE    *+10                                                             
         ICM   RF,15,UKSYSSUBPRG   SO RF IN E-MAIL SHOWS THE FACWRK KEY         
         DC    H'0'                DIE IF DISK ERROR                            
*                                                                               
         CLI   SOAFLAG,C'R'        REPLACE?                                     
         BNE   SOAEXT2X             NO                                          
*                                                                               
         L     R0,AIOA1            MOVE COPY RECORD TO IOA1 FOR WRITE           
         LHI   R1,L'IOA1                                                        
         L     RE,AIOA2                                                         
         LHI   RF,L'IOA2                                                        
         MVCL  R0,RE                                                            
*                                                                               
SOAEXT2X XIT1                                                                   
         LTORG                                                                  
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* SEE IF THERE ARE ANY MORE WKR FILES THAT REQUIRE $UPD TO BE RE-RUN  *         
***********************************************************************         
         SPACE 1                                                                
BACKUP   NTR1  BASE=*,LABEL=*                                                   
         GOTO1 VDATAMGR,DMCB,(X'00',=C'DTFAD'),=C'FACWRK'                       
         L     RE,DMCB+12          A(DTF)                                       
         TM    36(RE),X'40'        IS FACWRK FILE NO-OP?                        
         BO    BACKUPX             YES -- CAN'T READ IT                         
*                                                                               
         LA    R2,FINDEX           READ FACWRK INDEX                            
         USING UKRECD,R2                                                        
         XC    UKINDEX,UKINDEX                                                  
         LA    RE,FACWRK           SET SPECIAL DMCB FOR FACWRK I/OS             
         ST    RE,FWAFILE                                                       
         ST    R2,FWANDX                                                        
         LHI   R3,IOA-WORKD                                                     
         LA    R3,WORKD(R3)                                                     
         ST    R3,FWAREC                                                        
         USING FWRECD,R3                                                        
         LHI   R4,WBUFF-WORKD                                                   
         LA    R4,WORKD(R4)                                                     
         ST    R4,FWABUF                                                        
         USING WKRECD,R4                                                        
*                                                                               
         GOTO1 VDATAMGR,FWDMCB,(X'00',=C'BUF')                                  
*                                                                               
BACK02   GOTO1 VDATAMGR,FWDMCB,(X'08',=C'IND')                                  
         CLI   8(R1),0                                                          
         BE    BACK04                                                           
         TM    8(R1),X'80'         TEST END OF INDEX                            
         BO    BACKUPX                                                          
         DC    H'0'                DIE IF DISK ERROR                            
*                                                                               
BACK04   TM    UKSTAT,WKSTAC       FILE MUST BE ACTIVE                          
         BZ    BACK06                                                           
         TM    UKSTAT,WKSTKE       AND NOT KEEP                                 
         BO    BACK02                                                           
         CLI   UKCLASS,C'R'        FILE MUST HAVE CORRECT CLASS                 
         BNE   BACK02                                                           
         CLI   UKSUBPRG,C' '       TEST FACPAK ID IN KEY                        
         BNH   BACK08              NO                                           
         CLC   UKSUBPRG,SSBSYSN1                                                
         BNE   BACK02                                                           
         B     BACK08                                                           
*                                                                               
BACK06   TM    UKSTAT,WKSTHO       LOOK FOR HOLD FILES THAT DIDNT GET           
         BZ    BACK02              PROCESSED. WE DONT KNOW WHY !!               
         TM    UKSTAT,WKSTKE       TEST IF HOLD AND NOT KEEP                    
         BO    BACK02                                                           
         CLI   UKCLASS,C'R'        FILE MUST HAVE CORRECT CLASS                 
         BNE   BACK02                                                           
         CLI   UKSUBPRG,C' '       TEST FACPAK ID IN KEY                        
         BNH   BACK02              NO                                           
         CLC   UKSUBPRG,SSBSYSN1                                                
         BNE   BACK02                                                           
         OI    SSBJFLAG,SSBJFWKR                                                
         B     BACKUPX             EXIT WITH FLAG SET TO GET GOING              
*                                                                               
BACK08   GOTO1 VDATAMGR,FWDMCB,(X'00',=C'HOL')                                  
         OI    SSBJFLAG,SSBJFWKR                                                
*                                                                               
BACKUPX  XIT1  ,                                                                
*                                                                               
         LTORG                                                                  
         DROP  RB                                                               
         EJECT                                                                  
**********************************************************************          
         USING FILTABD,R5                                                       
SNIEXIT  NTR1  BASE=*,LABEL=*                                                   
         NI    FLAG,X'FF'-FLNIBYP                                               
*                                                                               
         CLC   =X'0A20',IOA1       IF NOT NET RECAP                             
         BNE   SNIEXITE            BYPASS                                       
*                                                                               
         MVC   XSPKEY,IOA1         SAVE KEY FROM FACKWORK                       
*                                                                               
*  CHECK IF THE RECORD WITH THE SAME KEY HAS ALREADY BEEN ADDED                 
*                                                                               
         GOTO1 VDATAMGR,DMCB,(X'88',DMRDHI),=C'XSPDIR',XSPKEY,AIOA2             
         TM    8(R1),X'FF'-X'02'                                                
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R1,AIOA2                                                         
         CLC   XSPKEY,0(R1)                                                     
         BNE   SNIEXITE           IF REC NOT THERE YET, DO NORMAL PROC          
*                                                                               
         OI    FLAG,FLNIBYP       BYPASS THIS REC IN ADD ROUTINE                
*                                                                               
*   THIS WILL PROCESS THE RECORDS AS CHANGES, AND BYPASS KEY ADDS               
*                                                                               
         CLI   DMFLTYP,DMFLIS     TEST INDEX SEQUENTIAL (DIR)                   
         BE    SNIEXITE           BYPASS                                        
*                                                                               
*    SIMULATE COPY - THIS 'ADD' IS REALLY A CHANGE                              
*                                                                               
         MVC   SB1DA,36(R1)       SAVE D/A (USE SB1DA SINCE DEFINED)            
         GOTO1 VDATAMGR,DMCB,(X'88',GETREC),FILENAME,SB1DA,AIOA2,IOWORK         
         TM    8(R1),X'FF'-X'02'                                                
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    R4,IOA1             GET ADDR OF NEW REC'S 1ST ELEM               
         LA    R4,42(R4)           1ST ELEM                                     
         L     R7,AIOA2            OLD - EXISTING RECORD                        
         LA    R6,42(R7)                                                        
         USING NRCPDTEL,R4                                                      
O        USING NRCPDTEL,R6                                                      
*                                                                               
SNI010   CLI   NRCPDTEL,X'10'      IS IT COMMERCIAL DATA ELEM?                  
         BNE   SNI100                                                           
         CLI   O.NRCPDTEL,X'10'                                                 
         BNE   SNI050                                                           
         CLC   NRCPFTD,O.NRCPFTD   SAME FTD                                     
         BNE   SNI060                                                           
         CLC   NRCPLTD,O.NRCPLTD   AND LTD                                      
         BNE   SNI060              NO, CHECK NEXT ELEMENT                       
         DROP  R4                                                               
*                                                                               
* BUMP TO NEXT ELEM IN NEW                                                      
* RESET TO START OF ELEMS IN OLD                                                
*                                                                               
SNI020   LLC   R0,1(R4)            BUMP TO NEXT ELEM IN NEW                     
         AR    R4,R0                                                            
         CLI   0(R4),0                                                          
         BNE   *+8                                                              
         B     SNI100              DONE WITH ALL NEW ELEMENTS                   
                                                                                
         L     R6,AIOA2            OLD - EXISTING RECORD                        
         LA    R6,42(R6)                                                        
         B     SNI010                                                           
*                                                                               
SNI050   GOTO1 VHELLO,DMCB,(C'P',FILENAME),(R7),(R4),0                          
         CLI   12(R1),0                                                         
         BE    SNI020                                                           
         DC    H'0'                                                             
*                                                                               
SNI060   LLC   R0,1(R6)            BUMP TO NEXT ELEM IN OLD                     
         AR    R6,R0                                                            
         CLI   0(R6),0                                                          
         BNE   SNI010                                                           
*                                                                               
SNI100   GOTO1 VDATAMGR,DMCB,PUTREC,FILENAME,SB1DA,AIOA2,IOWORK                 
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         GOTO1 VDATAMGR,DMCB,DMUNLK,FILENAME  UNLOCK BLOCK                      
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
SNIEXITE XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
*&&US                                                                           
**********************************************************************          
* ROUTINE TO COPY DATA - FROM CURRENT FILE RECORD TO FACWRK RECORD.  *          
*  DATA TO BE COPIED IS DEFINED IN ACCPYT.                           *          
* IN CPYHOOK DATA IS COPIED FROM CURRENT FILE RECORD(IOA2) TO THE    *          
*  FACWRK COPY RECORD(IAO1) TO ENSURE A MATCH ON THE COMPARE.        *          
* IN CHGHOOK DATA IS COPIED FROM CURRENT FILE RECORD(IOA2) TO THE    *          
*  FACWRK CHANGE RECORD(IAO1) TO ENSURE LATEST CHANGES ARE PRESERVED.*          
**********************************************************************          
CPYIT    NTR1  BASE=*,LABEL=*                                                   
         L     R2,AIOA2            R2=A(CURRENT FILE RECORD)                    
CUR      USING ACCRECD,R2                                                       
         L     R4,AIOA1            R3=A(COPY/CHANGE FACWRK RECORD)              
FAC      USING ACCRECD,R4                                                       
*                                                                               
         LA    R7,ACCPYT                                                        
         USING COPD,R7                                                          
CPYIT3   XR    R1,R1                                                            
         IC    R1,COPPGM           PROGRAM CODES                                
         EX    R1,*+8                                                           
         B     *+8                                                              
         TM    ACPGM,0             TEST ENTRY APPLIES TO THIS PROGRAM           
         BZ    CPYIT99             NO, SKIP ENTRY                               
*                                                                               
         CLI   RFILTY,X'69'        TEST ACCDIR                                  
         BNE   CPYIT4                                                           
         TM    COPDEF,COPSTAQ      TEST DIRECTORY STATUS CHANGE                 
         BNO   CPYIT99             NO, SKIP ITEM                                
         B     CPYIT5                                                           
*                                                                               
CPYIT4   CLI   RFILTY,X'6A'        TEST ACCMST                                  
         BNE   CPYITX                                                           
*                                                                               
CPYIT5   TM    COPDEF,COPELMQ      TEST ELEMENT DATA CHANGE                     
         BNO   CPYIT9                                                           
         MVC   ELCODE,COPCDE       SET ELEMENT CODE                             
         LA    R5,CUR.ACCRECD+(ACCRFST-ACCRECD)                                 
         BRAS  RE,FIRSTEL                                                       
         BNE   CPYIT99             NO ELEMENT, SKIP IT                          
         LR    R6,R5                                                            
         LA    R5,FAC.ACCRECD+(ACCRFST-ACCRECD)                                 
         BRAS  RE,FIRSTEL                                                       
         BNE   CPYIT8              ELEMENT NOT FOUND                            
         CLC   1(1,R5),1(R6)       TEST ELEMENT LENGTHS                         
         BNE   CPYIT6              NO,                                          
         XR    R1,R1                                                            
         IC    R1,1(R6)                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8              TEST DATA CHANGED                            
         B     *+10                                                             
         CLC   0(0,R5),0(R6)                                                    
         BE    CPYIT99             NO CHANGE, SKIP IT                           
         CLI   COPDSP,COPALLQ      KEEP ENTIRE CURRENT ELEMENT                  
         BNE   CPYIT6              NO                                           
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R5),0(R6)       MOVE CURRENT ELEMENT TO FACWRK               
         B     CPYIT99                                                          
*                                                                               
CPYIT6   TM    COPSTA,COPNARQ      TEST NARRATIVE CHANGE                        
         BNO   CPYIT7              NO,                                          
         BAS   RE,NARCOP           COPY THE LATEST NARRATIVE                    
         B     CPYIT99                                                          
*                                                                               
CPYIT7   CLC   1(1,R5),COPDSP      ELEMENT LONG ENOUGH TO HOLD DATA?            
         BNH   CPYIT99             NO, SKIP IT                                  
         CLC   1(1,R6),COPDSP                                                   
         BNH   CPYIT99                                                          
         B     CPYIT11                                                          
*                                                                               
CPYIT8   TM    COPSTA,COPADDQ      TEST ELEMENT MAY BE ADDED                    
         BNO   CPYIT99             NO ELEMENT, SKIP IT                          
         BAS   RE,ADDIT            ADD ELEMENT TO FACWRK                        
         B     CPYIT99                                                          
*                                                                               
CPYIT9   TM    COPDEF,COPSTAQ      CHANGE IN 'STATUS' AREA                      
         BNO   CPYIT99                                                          
         LA    R6,CUR.ACCRECD+(ACCKSTA-ACCRECD)                                 
         LA    R5,FAC.ACCRECD+(ACCKSTA-ACCRECD)                                 
         CLI   RFILTY,X'69'        TEST ACCDIR                                  
         BE    CPYIT11                                                          
*                                                                               
         LA    R6,CUR.ACCRECD+(ACCRSTA-ACCRECD)                                 
         LA    R5,FAC.ACCRECD+(ACCRSTA-ACCRECD)                                 
*                                                                               
CPYIT11  XR    R1,R1                                                            
         IC    R1,COPDSP           DISPLACEMENT TO DATA                         
         AR    R6,R1               R6=A(CUR DATA)                               
         AR    R5,R1               R5=A(FAC DATA)                               
*                                                                               
CPYIT13  TM    COPDEF,COPOIQ       TEST A BIT CHANGE-ON                         
         BO    CPYIT15                                                          
         TM    COPDEF,COPNIQ       TEST A BIT CHANGE-OFF                        
         BO    CPYIT17                                                          
         XR    R1,R1                                                            
         IC    R1,COPLEN           R1=LENGTH OF DATA                            
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R5),0(R6)       MOVE CURRENT DATA TO FACWRK                  
         B     CPYIT99                                                          
*                                                                               
CPYIT15  XR    R1,R1                                                            
         IC    R1,COPBIT           BIT SETTING                                  
         EX    R1,*+8                                                           
         B     *+8                                                              
         TM    0(R6),0             TEST BIT WENT ON                             
         BNO   CPYIT99             NO, DON'T CHANGE FACWRK                      
         EX    R1,*+8                                                           
         B     *+8                                                              
         OI    0(R5),0             TURN ON IN FACWRK                            
         B     CPYIT99                                                          
*                                                                               
CPYIT17  XR    R1,R1                                                            
         IC    R1,COPBIT           BIT SETTING                                  
         EX    R1,*+8                                                           
         B     *+8                                                              
         TM    0(R6),0             TEST BIT WENT OFF                            
         BO    CPYIT99             NO, DON'T CHANGE FACWRK                      
         LA    R1,255                                                           
         XR    R0,R0                                                            
         IC    R0,COPBIT                                                        
         SR    R1,R0                                                            
         EX    R1,*+8                                                           
         B     *+8                                                              
         NI    0(R5),0             TURN OFF IN FACWRK                           
*                                                                               
CPYIT99  LA    R7,COPLNQ(R7)       NEXT TABLE ENTRY                             
         CLI   0(R7),X'FF'                                                      
         BNE   CPYIT3                                                           
*                                                                               
CPYITX   XIT1                                                                   
         EJECT                                                                  
**********************************************************************          
* ADD AN ELEMENT FROM THE CURRENT RECORD TO THE FACWRK RECORD        *          
*  AT ENTRY R6=A(ELEMENT IN THE CURRENT RECORD)                      *          
*  WILL ATTEMPT TO PUT ELEMENT IN SAME RELATIVE POSITION IN FACWRK   *          
**********************************************************************          
ADDIT    NTR1  ,                                                                
         XR    R0,R0               COUNT ELEMENTS                               
         XR    R1,R1                                                            
         LA    RE,CUR.ACCRECD+(ACCRFST-ACCRECD)                                 
ADDIT3   CR    RE,R6               TEST LOCATION OF ELEMENT                     
         BE    ADDIT5              THIS IS IT                                   
         IC    R1,1(RE)            BUMP TO NEXT ELEMENT                         
         AR    RE,R1                                                            
         AHI   R0,1                COUNT ELEMENTS                               
         B     ADDIT3                                                           
*                                                                               
ADDIT5   LA    RE,FAC.ACCRECD+(ACCRFST-ACCRECD)                                 
ADDIT7   LTR   R0,R0               TEST CORRECT POSITION                        
         BZ    ADDIT9              YES,                                         
         CLI   0(RE),0             TEST EOR ON FACWRK                           
         BE    ADDIT9              YES,                                         
         IC    R1,1(RE)            BUMP TO NEXT FACWRK ELEMENT                  
         AR    RE,R1                                                            
         SHI   R0,1                                                             
         B     ADDIT7                                                           
*                                                                               
ADDIT9   STCM  RE,15,DUB           SAVE START OF DATA TO MOVE                   
         XR    RF,RF               GET LENGTH TO END OF FACWRK                  
ADDIT11  CLI   0(RE),0                                                          
         BE    ADDIT13                                                          
         IC    R1,1(RE)                                                         
         AR    RE,R1                                                            
         AR    RF,R1                                                            
         B     ADDIT11                                                          
*                                                                               
ADDIT13  AHI   RF,1                SAVE END OF RECORD +1                        
         ST    RF,FULL             SAVE LENGTH OF MOVE                          
         L     R0,AIOA3            SAVE PART OF FACWRK IN IO3                   
         LR    R1,RF               SET LENGTHS FOR MOVE                         
         ICM   RE,15,DUB           RESTORE START OF DATA TO MOVE                
         MVCL  R0,RE                                                            
*                                                                               
         XR    R1,R1                                                            
         IC    R1,1(R6)            LENGTH OF CURRENT ELEMENT                    
         BCTR  R1,0                                                             
         ICM   RE,15,DUB           RESTORE START OF DATA JUST MOVED             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RE),0(R6)       ADD NEW ELEMENT TO FACWRK                    
*                                                                               
         LA    RE,1(R1,RE)         RE TO NEXT ELEMENT POSITION                  
         ICM   R1,15,FULL          LENGTH OF SAVED DATA                         
         LR    RF,R1                                                            
         L     R0,AIOA3            RESTORE END OF FACWRK                        
         MVCL  RE,R0                                                            
*                                                                               
         XR    R1,R1               FIX LENGTH OF FACWRK                         
         IC    R1,1(R6)            LENGTH OF ELEMENT JUST ADDED                 
         XR    RF,RF                                                            
         ICM   RF,3,FAC.ACCRECD+(ACCRLEN-ACCRECD)                               
         AR    RF,R1                                                            
         STCM  RF,3,FAC.ACCRECD+(ACCRLEN-ACCRECD)                               
         B     CPYITX                                                           
         EJECT                                                                  
**********************************************************************          
* COPY TRANSACTION NARRATIVE FROM CURRENT RECORD TO FACWRK RECORD    *          
*  AT ENTRY R6=A(TRANSACTION ELEMENT IN CURRENT RECORD)              *          
*           R5=A(TRANSACTION ELEMENT IN FACWRK RECORD)               *          
**********************************************************************          
C        USING TRNELD,R6                                                        
F        USING TRNELD,R5                                                        
NARCOP   NTR1  ,                                                                
         MVC   WORK(TRNLN1Q),F.TRNELD SAVE FAC DATA - EXCEPT NARRATIVE          
         LA    RF,FAC.ACCRECD         DELETE FAC 44 ELEMENT                     
         GOTO1 VHELLO,DMCB,(C'D',FILENAME),(X'44',(RF)),0,0                     
         BAS   RE,ADDIT            ADD NEW TRNELD TO FACWRK                     
         MVC   F.TRNDATE(TRNLN1Q-2),WORK+(TRNDATE-TRNELD)                       
         B     CPYITX                                                           
*                                                                               
         DROP  R7,CUR,FAC,F,C                                                   
         EJECT                                                                  
*                                                                               
ACCPYT   DS    0H                                                               
*                                                                               
* A21, A23, A27, A29                                                            
*                                                                               
         DC    AL1(COPBILQ,COPSTAQ,0)            ACCOUNT RECORD STATUS          
         DC    AL1(0,ACTRSTAT-ACTRECD,L'ACTRSTAT)                               
*                                                                               
         DC    AL1(COPBILQ,COPELMQ,0)            RECORD ACTIVITY ELEMNT         
         DC    AL1(RACELQ,COPALLQ,0)                                            
*                                                                               
         DC    AL1(COPBILQ,COPELMQ,0)            DATE LAST TRAN POSTED          
         DC    AL1(RSTELQ,RSTTDATE-RSTELD,L'RSTTDATE)                           
*                                                                               
         DC    AL1(COPBILQ,COPELMQ,0)            STATUS BYTE ONE                
         DC    AL1(RSTELQ,RSTSTAT1-RSTELD,L'RSTSTAT1)                           
*                                                                               
         DC    AL1(COPBILQ,COPELMQ,0)            ADDITIONAL STATUS              
         DC    AL1(RSTELQ,RSTSTAT1-RSTELD,L'RSTSTAT2)                           
*                                                                               
         DC    AL1(COPBILQ,COPELMQ,0)            STATUS BYTE TWO                
         DC    AL1(RSTELQ,RSTSTAT3-RSTELD,L'RSTSTAT3)                           
*                                                                               
         DC    AL1(COPBILQ,COPELMQ,0)            ESTIMATED CLOSING DATE         
         DC    AL1(JOBELQ,JOBCDATE-JOBELD,L'JOBCDATE)                           
*                                                                               
         DC    AL1(COPBILQ,COPELMQ,0)            AURA HR LOCAL NUMBER           
         DC    AL1(JOBELQ,JOBAEHR#-JOBELD,L'JOBAEHR#)                           
*                                                                               
         DC    AL1(COPBILQ,COPELMQ,0)            AURA OE LOCAL NUMBER           
         DC    AL1(JOBELQ,JOBAEOE#-JOBELD,L'JOBAEOE#)                           
*                                                                               
         DC    AL1(COPBILQ,COPELMQ,0)            AURA CE LOCAL NUMBER           
         DC    AL1(JOBELQ,JOBAECE#-JOBELD,L'JOBAECE#)                           
*                                                                               
         DC    AL1(COPBILQ,COPELMQ,0)            ACTIVITY DATE -COMPRES         
         DC    AL1(RAPKTYPQ,RAPKDATE-RAPRECD,L'RAPKDATE)                        
*                                                                               
         DC    AL1(COPBILQ,COPELMQ,0)            ACTIVITY TIME -BINARY          
         DC    AL1(RAPKTYPQ,RAPKTIME-RAPRECD,L'RAPKTIME)                        
*                                                                               
         DC    AL1(COPBILQ,COPELMQ,0)                                           
         DC    AL1(ASTELQ,COPALLQ,0)                                            
*                                                                               
         DC    AL1(COPBILQ,COPELMQ,0)                                           
         DC    AL1(ABLELQ,COPALLQ,0)                                            
*                                                                               
* AAA (AUTO APPROVE)                                                            
*                                                                               
         DC    AL1(COPAPPQ,COPSTAQ+COPOIQ,0)     USED DATE BIT                  
         DC    AL1(0,TRNKSTA2-TRNKSTA,TRNSUSED)                                 
*                                                                               
         DC    AL1(COPAPPQ,COPSTAQ,0)            USED DATE                      
         DC    AL1(0,TRNKSUSE-TRNKSTA,L'TRNKSUSE)                               
*                                                                               
         DC    AL1(COPAPPQ,COPELMQ+COPOIQ,0)     APPROVE BIT                    
         DC    AL1(TRNELQ,TRNSTAT-TRNELD,TRNSAPPR)                              
*                                                                               
         DC    AL1(COPAPPQ,COPELMQ,0)             USED DATE                     
         DC    AL1(TRSELQ,TRSUDAT-TRSELD,L'TRSUDAT)                             
*                                                                               
         DC    AL1(COPAPPQ,COPELMQ,0)             USED DATE                     
         DC    AL1(TRSELQ,TRSUMOS-TRSELD,L'TRSUMOS)                             
*                                                                               
         DC    AL1(COPAPPQ,COPELMQ,0)             LAST MARKER ACTION            
         DC    AL1(TRSELQ,TRSMARK-TRSELD,L'TRSMARK)                             
*                                                                               
         DC    AL1(COPAPPQ,COPELMQ,COPADDQ)       DUE DATE                      
         DC    AL1(DUEELQ,DUEDATE-DUEELD,L'DUEDATE)                             
*                                                                               
         DC    AL1(COPAPPQ,COPELMQ,COPNARQ)       NARRATIVE                     
         DC    AL1(TRNELQ,TRNNARR-TRNELD,0)                                     
*                                                                               
         DC    AL1(COPAPPQ,COPSTAQ+COPOIQ,0)       TURN ON APPROVE BIT          
         DC    AL1(0,AAVPSTAT-AAVPSTA,AAVPAPP)                                  
*                                                                               
         DC    AL1(COPAPPQ,COPSTAQ,0)              USED DATE                    
         DC    AL1(0,AAVPUSED-AAVPSTA,L'AAVPUSED)                               
         DC    X'FF'                                                            
*                                                                               
**********************************************************************          
*            ****** TEST ITEMS ********                              *          
* THE FOLLOWING ITEMS ARE NOT IN THE TABLE AND THEY SHOULD NOT BE IN *          
* THE TABLE.  THEY WERE USED FOR TESTING AND NOW CAN BE USED AS A    *          
* GUIDE FOR ADDING ENTRIES THAT MAY BE NEEDED IN THE FUTURE.         *          
*                                                                    *          
* TO KEEP A STATUS BIT ON IN ACCDIR & ACCMST YOU MIGHT ADD THIS:     *          
*        DC    AL1(COPBILQ+COPAPPQ,COPSTAQ+COPOIQ,0)                 *          
*        DC    AL1(0,TRNRSTA2-TRNRSTA,TRNSEXCL)                      *          
* TO KEEP A STATUS BIT OFF IN ACCDIR & ACCMST:                       *          
*        DC    AL1(COPBILQ+COPAPPQ,COPSTAQ+COPNIQ,0)                 *          
*        DC    AL1(0,TRNRSTYP-TRNRSTA,X'0A')                         *          
* TO KEEP A STATUS FIELD IN ACCDIR & ACCMST:                         *          
*        DC    AL1(COPBILQ+COPAPPQ,COPSTAQ,0)                        *          
*        DC    AL1(0,TRNRSANL-TRNRSTA,L'TRNRSANL)                    *          
**********************************************************************          
*                                                                               
         LTORG                                                                  
         DROP  RB                                                               
*&&                                                                             
         EJECT                                                                  
*================================================================               
* ADD A SOON REQUEST FOR STEP2 FOR THE ORIGINAL TERMINAL                        
*================================================================               
                                                                                
AMSADD   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         L     R4,AIOA1                                                         
         USING GENQREC,R4                                                       
*                                                                               
         USING SPOOK,RE                                                         
         LA    RE,GENQSPOK         GET SPOOK ADDRESS                            
         MVI   SPOOKWEN,2          SET 'SOON'                                   
         DROP  RE                                                               
*                                                                               
         USING COMFACSD,RE                                                      
         L     RE,SRPAR4           GET A(COMFACS)                               
         ICM   RF,15,CREQTWA                                                    
         DROP  RE                                                               
*                                                                               
* NOTE X'80' IN PARM 5 MEANS USE UTL DATA IN SPOOK                              
*      X'40' IN PARM 5 MEANS A(20-BTYE RFHDR) IN PARM 6                         
* SRPAR6 IS A(TWA)                                                              
* SRPAR4 IS A(COMFACS)                                                          
         GOTO1 (RF),DMCB,(5,SRPAR6),GENQHDR,VDATAMGR,SRPAR4,           X        
               (X'C0',GENQSPOK),GENQRFH                                         
*                                                                               
NOTSJ    CLI   8(R1),X'FE'         TERMINAL QUEUE FULL                          
         BNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLI   8(R1),X'FF'         PRTQUE FULL                                  
         BNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     RE,8(R1)            GET ADDRESS OF PRTQUE KEY                    
         MVC   BLOCK(3),2(RE)                                                   
         SR    R0,R0                                                            
         ICM   R0,3,6(RE)                                                       
         BNZ   *+6                                                              
         DC    H'0'                JCL NOT FOUND                                
*                                                                               
         STH   R0,HALF             SAVE REPORT NUMBER IN HALF                   
         XIT1                                                                   
         LTORG                                                                  
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
* REMOVE FAUPDTAB ENTRY IF THERE                                      *         
***********************************************************************         
*                                                                               
UNLOCK   NTR1  BASE=*,LABEL=*                                                   
         OC    HEADER,HEADER                                                    
         BZ    UNLOCKX                                                          
         LA    R3,HEADER                                                        
         USING FWRECD,R3                                                        
*                                                                               
         L     R6,VUPDTAB                                                       
         LH    R4,0(R6)                                                         
         L     R5,2(R6)                                                         
         LA    R6,6(R6)                                                         
         USING UPDTABD,R6                                                       
UNLK010  CLI   UPDTABT,UPDTSQ      TEST UPDATIVE SOON ENTRY                     
         BNE   *+14                                                             
         CLC   FWRPQK,UPDTSOON     TEST KEY                                     
         BE    UNLK020                                                          
         BXLE  R6,R4,UNLK010                                                    
         B     UNLOCKX             NO LOCK FOUND                                
*                                                                               
UNLK020  MVC   BYTE,UPDCHAIN       SAVE CHAIN BYTE                              
*                                                                               
         LR    R0,R6               R0=DEST                                      
         LA    R1,1(R5)                                                         
         SR    R1,R6               R1=L'DEST                                    
         LR    RE,R6                                                            
         AR    RE,R4               RE=SOURCE                                    
         LR    RF,R1                                                            
         SR    RF,R4               RF=L'SOURCE                                  
         MVCL  R0,RE               MOVE & PAD LAST ENTRY WITH ZERO              
UNLOCKX  XIT1                                                                   
         DROP  R3,R6                                                            
         LTORG                                                                  
         EJECT                                                                  
*DMFILTAB                                                                       
*        PRINT OFF                                                              
*      ++INCLUDE DMFILTAB                                                       
*        PRINT ON                                                               
*                                                                               
*                                                                               
TALX_ERR NTR1  BASE=*,LABEL=*                                                   
         USING FWRECD,R3                                                        
*                                                                               
         XC    OPMSG,OPMSG                                                      
         MVC   OPMSG(30),=CL30'+TAL_CHECKS+ UNLOCK FAILURE'                     
         MVC   OPMSG+30(15),WORK                                                
         OC    OPMSG+30(15),SPACES                                              
         OC    OPMSG,SPACES                                                     
         GOTO1 VTICTOC,DUB,C'SSET' SUSPEND TIMERS                               
         GOTO1 VDMOD000,PARML,VWCTYPE,OPMSG,OPMSGL,C'LVL1'                      
         GOTO1 VTICTOC,DUB,C'RSET' RESET TIMERS                                 
*                                                                               
         GOTO1 VDATAMGR,DMCB,=C'OPMSG',('TALXMSGLQ',TALX_MSG)                   
*                                                                               
         CR    RB,RB                                                            
         XIT1                                                                   
*                                                                               
TALX_MSG DC    C'AUTONOTE*US-TALENTMFDEVTEAM,US-TALENTPRODUCTTEAM: '            
         DC    C'SOON CHECK LOCK FAILURE'                                       
TALXMSGLQ EQU *-TALX_MSG                                                        
         LTORG                                                                  
*                                                                               
*&&US                                                                           
         USING FILTABD,R5                                                       
SAIEXIT3 NTR1  BASE=*,LABEL=*                                                   
         NI    FLAG,X'FF'-FLTABYP                                               
                                                                                
         CLC   =X'0A24',IOA1       IF INST RECAP                                
         BNE   SAIEXT                                                           
*                                                                               
         L     R4,AIOA3            PROCESS IT                                   
         CLC   IOA1(13),0(R4)      SAME KEY AS PREVIOUS?                        
         BE    SAI100               NEEDS TO BE BYPASSED (KEY ADD)              
*                                   OR CHANGE (RECORD)                          
         CLI   RFILTY,X'32'        THIS TRFFIL                                  
         BE    SAIEXT                                                           
         SPACE                                                                  
* WAIT FOR SECOND RECORD (DIRECTORY ADD) TO SAVE KEY                            
         SPACE                                                                  
         MVC   0(13,R4),IOA1       SAVE KEY, BUT DO NORMAL PROCESSING           
         B     SAIEXT                                                           
         SPACE                                                                  
SAI100   DS    0H                                                               
         OI    FLAG,FLTABYP        BYPASS THIS RECORD IN ADD ROUTINE            
         SPACE                                                                  
* THIS WILL PROCESS THE RECORDS AS CHANGES, AND BYPASS KEY ADDS                 
         SPACE                                                                  
         CLI   DMFLTYP,DMFLIS      TEST INDEX SEQUENTIAL                        
         BE    SAIEXT              BYPASS                                       
         SPACE                                                                  
* SIMULATE COPY TO READ BEFORE WRITE - THIS 'ADD' IS REALLY A CHANGE            
         SPACE                                                                  
         GOTO1 VDATAMGR,DMCB,(X'88',GETREC),FILENAME,DALINK,AIOA2,     C        
               IOWORK                                                           
         TM    8(R1),X'FD'                                                      
         BZ    SAI120                                                           
         DC    H'0'                                                             
*                                                                               
SAI120   DS    0H                                                               
         LA    R4,IOA1             GET ADDR OF NEW REC                          
         LA    R4,24(,R4)          1ST ELEM                                     
         L     R7,AIOA2            OLD - EXISTING RECORD                        
         SPACE                                                                  
SAI140   DS    0H                                                               
         GOTO1 VHELLO,DMCB,(C'P',FILENAME),(R7),(R4),0                          
         CLI   12(R1),0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         ZIC   R0,1(R4)                                                         
         AR    R4,R0                                                            
         CLI   0(R4),0                                                          
         BNE   SAI140                                                           
         SPACE                                                                  
         GOTO1 VDATAMGR,DMCB,PUTREC,FILENAME,DALINK,AIOA2,IOWORK                
         CLI   8(R1),0                                                          
         BE    SAI160                                                           
         DC    H'0'                                                             
SAI160   DS    0H                                                               
         GOTO1 VDATAMGR,DMCB,DMUNLK,FILENAME  UNLOCK BLOCK                      
         CLI   8(R1),0                                                          
         BE    SAIEXT                                                           
         DC    H'0'                                                             
         SPACE                                                                  
SAIEXT   XIT1                                                                   
         DROP  R5                                                               
         LTORG                                                                  
*&&                                                                             
*                                                                               
*                                                                               
         EJECT                                                                  
         PRINT ON                                                               
GENQREC  DSECT ,            DSECT FOR WKFILE REQUEST REC                        
*                                                                               
GENQTYPE DS  CL1'8'                                                             
GENQSBTY DS  CL1'*'                                                             
         DS  CL23           SPACE FOR CTFILE KEY                                
GENQLEN  DS  XL2      +25                                                       
GENQSTAT DS  XL1      +26                                                       
         DS  CL1      +27                                                       
GENQRFH  DS  XL20     +28   RFHDR                                               
GENQSPOK DS  XL64     +48   SPOOK                                               
GENQHDR  DS  XL26     +112  REQUEST HEADER                                      
GENQREQ  DS  CL80     +138  (ONE OR MORE REQUESTS)                              
GENQRECL EQU *-GENQREC                                                          
*                                                                               
YES      EQU   C'Y'                                                             
NO       EQU   C'N'                                                             
*                                                                               
WORKD    DSECT                                                                  
SRPARS   DS    0XL32                                                            
SRPAR1   DS    A                   A(SYSFACS)                                   
SRPAR2   DS    A                   A(TIA)                                       
SRPAR3   DS    A                   A(UTL ENTRY)                                 
SRPAR4   DS    A                   A(COMFACS)                                   
SRPAR5   DS    A                   A(SELIST ENTRY)                              
SRPAR6   DS    A                   A(TWA)                                       
SRPAR7   DS    A                                                                
SRPAR8   DS    A                                                                
*                                                                               
*                                                                               
DUB      DS    D                                                                
DUB1     DS    D                                                                
FULL     DS    F                                                                
HALF     DS    H                                                                
BYTE     DS    C                                                                
FLAG     DS    C                                                                
FLFIRSTQ EQU   X'80'               FIRST TIME FLAG (SET WHEN DONE)              
FLENQZQ  EQU   X'40'               ENQ MEDZ MESSAGE HAS BEEN SENT               
FLUNVZQ  EQU   X'20'               MEDZ UNIVERSE UPDATES DETECTED               
FLZ5BYP  EQU   X'10'               Z5 BYPASS THIS RECORD - PREV ADDED           
*                                  OR UPDATED IN HOOK ROUTINE                   
FLB1BYP  EQU   X'08'               B1 BYPASS THIS RECORD - PREV ADDED           
*                                  OR UPDATED IN HOOK ROUTINE                   
FLBUBYP  EQU   X'04'               BU BYPASS THIS RECORD - PREV ADDED           
*                                  OR UPDATED IN HOOK ROUTINE                   
FLACBYP  EQU   FLBUBYP             BYPASS RECORD                                
FLTABYP  EQU   X'02'               TA BYPASS THIS RECORD - PREV ADDED           
*                                  OR UPDATED IN HOOK ROUTINE                   
FLNIBYP  EQU   X'01'               NI BYPASS THIS RECORD - PREV ADDED           
*                                                                               
*                                                                               
DMCB     DS    6F                                                               
*                                                                               
PQDMCB   DS    6F                                                               
*                                                                               
FWDMCB   DS    0XL24               FACWRK DMCB                                  
FWAACTN  DS    A                                                                
FWAFILE  DS    A                                                                
FWANDX   DS    A                                                                
FWAREC   DS    A                                                                
FWABUF   DS    A                                                                
         DS    A                                                                
*                                                                               
ACIREC   DS    A                   For PQ                                       
*                                                                               
AIOA1    DS    A                   A(IOA1)                                      
AIOA2    DS    A                   A(IOA2)                                      
AIOA3    DS    A                   A(IOA3)                                      
PARML    DS    6F                                                               
SMFPARM  DS    6F                                                               
         ORG   SMFPARM                                                          
SMFP1    DS    F                                                                
SMFP2    DS    F                                                                
SMFP3    DS    F                                                                
SMFP4    DS    F                                                                
SMFP5    DS    F                                                                
SMFP6    DS    F                                                                
         ORG                                                                    
                                                                                
*                                                                               
FINDEX   DS    XL32                FACWRK INDEX                                 
WORK     DS    XL64                                                             
KEY      DS    XL64                                                             
GDIR     DS    XL64                DIRECTORY KEY, GENFILE                       
DKEY     DS    XL64                KEY OF LAST DA RECORD ADDED                  
BLOCK    DS    XL64                                                             
SB1KEY   DS    XL13                                                             
SB1DA    DS    XL4                                                              
XSPKEY   DS    XL32                                                             
HEADER   DS    XL32                                                             
USID     DS    CL(L'FWRUSID)                                                    
LUID     DS    CL(L'FWRLUID)                                                    
PQID     DS    CL(L'FWRPQID)                                                    
PQIDL    DS    CL(L'FWRPQID)                                                    
WKID     DS    CL(L'FWRWKID)                                                    
IOWORK   DS    12D                                                              
SAVE     DS    50F                                                              
SAVELQ   EQU   *-SAVE                                                           
SMFREC   DS    CL(TRAKUID-TRAKD+L'TRAKUID)                                      
*                                                                               
FILENAME DS    CL7                 DMGR FILENAME                                
LASTFILE DS    CL1                 PREVIOUS FILE NUMBER                         
DALINK   DS    CL4                 DISK ADDRESS OF LAST ADDREC                  
DDA      DS    CL4                 DISK ADDRESS OF GETREC                       
DALAST   DS    CL1                 LAST DA FILE ADDED TO                        
GDA      DS    CL4                 DISK ADDRESS OF GETREC / GENFILE             
SENUM    DS    CL1                 SWITCHED SYSTEM                              
RACTN    DS    CL1                 RECORD TYPE (COPY CHNG ADD)                  
LASTACTN DS    CL1                 LAST TYPE (COPY CHNG ADD)                    
ELCODE   DS    XL1                                                              
COPYQ    EQU   X'01'                                                            
CHANGEQ  EQU   X'02'                                                            
ADDQ     EQU   X'03'                                                            
BYTE1    DS    X                                                                
TRFLAG   DS    X                   TRAFFIC FLAG                                 
SOAFLAG  DS    X                   SOA FLAG                                     
WKPQ#FLG DS    C                   Yes, No  Worker file has PQ rep#             
RECLEN   DS    XL2                 RECORD LENGTH                                
RECCOUNT DS    F                   RECORD COUNT                                 
PQUSERID DS    H                   USERID NUMBER OF ASSOCIATED PQ RPT           
PQCINUM  DS    0H          (Old)   C/I NUMBER OF ASSOCIATED PQ REPORT           
PQREP#   DS    H           (New)   Report number associated PQ report           
PQUSRID2 DS    H                   USERID NUMBER OF ASSOCIATED PQ RPT           
PQREPRNO DS    H                   REF NUMBER OF ASSOCIATED PQ REPORT           
PQSYSPRG DS    CL3                 SAVED SYSTEM PROGRAM                         
PQDESCR  DS    CL(L'QLDESC)        PQ DESCRIPTION                               
SYSNAME  DS    CL3                 3 CHR FACPAK NAME                            
SVWKSPP  DS    CL3                                                              
*                                                                               
DEMSAVE  DS    XL6                 DEMO EXIT SAVE AREA FOR DEMPROS              
*                                                                               
RELO     DS    A                                                                
VSWITCH  DS    V                                                                
VHELLO   DS    V                                                                
VDATCON  DS    V                                                                
VCALLOVL DS    V                                                                
VLOCKET  DS    V                                                                
VHEXOUT  DS    V                                                                
VRECUP   DS    V                                                                
VHEXIN   DS    V                                                                
VGETFACT DS    V                                                                
VSMFOUT  DS    V                                                                
*                                                                               
ATIA     DS    A                                                                
AUTL     DS    A                                                                
*                                                                               
*                                                                               
SAVEBUFF DS    0XL12                                                            
SBVPQTAB DS    A                   A(PQTAB)                                     
SBVPQFIL DS    A                   A(PQFILE DTF)                                
SBSAVE   DS    A                   DISPLACEMENT TO START OF SAVE AREA           
*                                                                               
SAVEAIO1 DS    A                   SAVED ADDRESS OF AIO1 BEFORE DEATH           
SAVEAIO2 DS    A                   SAVED ADDRESS OF AIO2 BEFORE DEATH           
SAVELEN  DS    F                   # OF BYTES THAT MATCHED BEFORE DEATH         
*                                                                               
SAVESIN  DS    F                                                                
NEXTSIN  DS    F                                                                
QSIN     DS    CL6                                                              
PRTQID   DS    CL8                                                              
NDX      DS    XL40                                                             
SVNDX    DS    XL40                                                             
         EJECT                                                                  
       ++INCLUDE DMPRTQW                                                        
         EJECT                                                                  
OPMSG    DS    0CL60                                                            
OPENQID  DS    CL8                 *ENQUEUE                                     
         DS    CL1                                                              
OPSYSID  DS    CL4                 SESYS (MEDZ)                                 
         DS    CL1                                                              
OPCMSG   DS    CL15                MESSAGE                                      
         DS    CL1                                                              
OPFACID  DS    CL8                 FACPAKID                                     
         DS    CL2                                                              
*                                                                               
         ORG   OPMSG                                                            
OPSOON   DS    CL(L'SOONMSG)       *SOON DUMP*                                  
         DS    CL1                                                              
OPDLOC   DS    CL6                 LOCATION OF DEATH                            
         DS    CL1                                                              
OPUSID   DS    CL7                 USER                                         
         DS    CL1                                                              
OPPRGM   DS    CL4                 PROGRAM/SUB                                  
         DS    CL1                                                              
OPLUID   DS    CL8                 LUID                                         
         DS    CL1                                                              
         ORG   OPMSG+L'OPMSG                                                    
OPMSGL   EQU   *-OPMSG                                                          
*                                                                               
         ORG   OPMSG                                                            
OPINVS   DS    CL(L'INVSMSG)       UNABLE TO UNINVISIBLE A REPORT               
         DS    CL1                                                              
OPIUSID  DS    CL7                 USER                                         
         DS    CL1                                                              
OPIPRGM  DS    CL4                 PROGRAM/SUB                                  
         DS    CL1                                                              
OPILUID  DS    CL8                 LUID                                         
         DS    CL1                                                              
OPPQNAM  DS    CL3                 LUID                                         
OPCOMMA  DS    CL1                                                              
OPPQNUM  DS    CL4                                                              
*                                                                               
         ORG   OPMSG                                                            
OPVERR#  DS    CL1                 Error number                                 
         DS    CL1                                                              
OPVTEXT  DS    CL(L'VSBLMSG)       *VISIBLE U=                                  
OPVUSRID DS    CL6                                                              
         DS    CL1                 COMMA                                        
OPVRPTID DS    CL6                                                              
         DS    CL3                                                              
OPFACADV DS    CL3                 FACPAKID                                     
         DS    CL2                                                              
OPVXTXT  DS    CL16                XTRA TEXT                                    
         DS    CL1                                                              
OPVSPP   DS    0CL3                SYSTEM PROGRAM                               
OPVPRTQ# DS    CL8                                                              
*                                                                               
         ORG   OPMSG+L'OPMSG                                                    
*                                                                               
*&&US                                                                           
ADADDTRN DS    A                   A(ADDTRN)                                    
ACTRNBLK DS    XL(TRNBLKL)         ADDTRN BLOCK                                 
ACFLG    DS    XL1                                                              
ACPOST   EQU   X'80'               ACC POSTING MODE                             
ACTLAST  EQU   X'40'               CALL ADDTRN LAST                             
ACPGM    DS    XL1                 PROGRAM FLAG                                 
PALAREA  DS    XL32                P&L BUCKET AREA                              
*&&                                                                             
*                                                                               
AFILTAB  DS    A                                                                
*                                                                               
         DS    0F                                                               
IOA      DS    XL28                FACWRK RECORD LEN AND RECOVERY HDR           
IOA1     DS    XL6144              FACWRK RECORD (MAX 6K)                       
IOA2     DS    XL6144              FILE RECORD                                  
IOA2X    EQU   *                                                                
IOA3     DS    XL6144              SAVE IOA1 FOR COMPARE                        
IOA1SAVE DS    XL6144              SAVE IOA1                                    
WBUFF    DS    XL14336             FACWRK BUFFER                                
PQBUFF   DS    XL14336             PQ BUFFER                                    
WORKX    EQU   *                                                                
         EJECT                                                                  
       ++INCLUDE SRUPDD                                                         
         ORG   FWRHDR                                                           
       ++INCLUDE DMRCVRHDR                                                      
FWRDATA  DS    X                                                                
         EJECT                                                                  
PKRECD   DSECT                                                                  
*                                                                               
PKINDEX  DS    0CL24               PRTQ INDEX ENTRY                             
*                                                                               
PKKEY    DS    0CL7                                                             
PKSRCID  DS    XL2                 USER ID NUMBER                               
PKSUBID  DS    CL3                 REPORT ID                                    
PKREPNO  DS    XL2                 FILE REPORT NUMBER WITHIN USERID             
*                                                                               
PKCLASS  DS    XL1                 CLASS                                        
PKTYPE   DS    XL1                 TYPE                                         
PKTYPUPQ EQU   X'80'                UPDATIVE SOON                               
PKATTB   DS    XL1                 ATTRIBUTES                                   
PKSTAT   DS    XL1                 FILE STATUS                                  
PKSINVB  EQU   X'02'               MARKED AS INVISIBLE                          
PKSEQ    DS    XL1                 CI SEQ NUM                                   
PKAGES   DS    XL1                 NUMBER OF CONTROL INTERVALS                  
PKAGELD  DS    XL2                 LIVE DATE                                    
PKAGEDD  DS    XL2                 DEAD DATE                                    
PKAGERD  DS    XL2                 RETN DATE                                    
         DS    XL1                                                              
PKAGELT  DS    XL2                 LIVE TIME (SECS*3)/4                         
         DS    XL2                                                              
*                                                                               
PKINFO   DS    XL2                 INFO PASSING FIELD                           
PKREPNOX DS    XL2                 UPPER LIMIT                                  
PKCIADDR DS    XL2                 TTTT OF FIRST CI                             
PKFLAG   DS    XL1                 FLAG VALUES                                  
PKFLDAT  EQU   X'80'               PASS BACK DATA                               
PKFLDSW  EQU   X'40'               SWITCH FROM INDEX TO DATA                    
PKFLUSR  EQU   X'20'               USER INFO SUPPLIED IN UKUSRINF               
PKFLHRS  EQU   X'10'               HOURS PASSED IN UKINFO                       
         DS    XL1                 N/D                                          
*                                                                               
PKUSRINF DS    XL8                 USER INFO                                    
         EJECT                                                                  
       ++INCLUDE DMWRKRD                                                        
         EJECT                                                                  
       ++INCLUDE DMWRKRK                                                        
UKRECD   DSECT                                                                  
         ORG   UKSYSPRG                                                         
UKSYSSUBPRG DS 0CL4                BOTH UKSYSPRG AND UKSUBPRG                   
         ORG                                                                    
         EJECT                                                                  
RQHHDRD  DSECT                                                                  
       ++INCLUDE DMREQHDRA                                                      
         EJECT                                                                  
COPD     DSECT                     DSECT TO COVER COPY TABLE ENTRIES            
COPPGM   DS    XL1                 PROGRAMS THAT USE THIS ENTRY                 
COPBILQ  EQU   X'80'                ACC BILLING PROGRAMS                        
COPAPPQ  EQU   X'40'                ACC AUTO APPROVE                            
COPDEF   DS    XL1                 DATA DEFINITION                              
COPELMQ  EQU   X'80'                DATA IS IN AN ELEMENT                       
COPSTAQ  EQU   X'40'                DATA IS IN THE STATUS AREA                  
COPOIQ   EQU   X'08'                DATA BIT IS ON                              
COPNIQ   EQU   X'04'                DATA BIT IS OFF                             
COPSTA   DS    XL1                 STATUS BYTE                                  
COPADDQ  EQU   X'80'                ELEMENT MAY BE ADDED TO FACWRK              
COPNARQ  EQU   X'40'                SPECIAL NARRATIVE HANDLING                  
COPCDE   DS    XL1                 ELEMENT CODE                                 
COPDSP   DS    XL1                 DISPLACEMENT TO DATA                         
COPALLQ  EQU   0                    THE ENTIRE ELEMENT                          
COPLEN   DS    XL1                 LENGTH OF DATA                               
         ORG   COPLEN                                                           
COPBIT   DS    XL1                 BIT SETTING FOR OI  & NI                     
COPLNQ   EQU   *-COPD              LENGTH OF ENRTY                              
         EJECT                                                                  
* ACTRACKD                                                                      
         PRINT OFF                                                              
       ++INCLUDE ACTRACKD                                                       
         PRINT ON                                                               
* ACGENFILE                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACGENFILE                                                      
         PRINT ON                                                               
* ACGENRAC                                                                      
         PRINT OFF                                                              
       ++INCLUDE ACGENRAC                                                       
         PRINT ON                                                               
*&&US                                                                           
* ACADDTRND                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACADDTRND                                                      
         PRINT ON                                                               
*&&                                                                             
* CTGENFILE                                                                     
         PRINT OFF                                                              
       ++INCLUDE CTGENFILE                                                      
         PRINT ON                                                               
* DDCOMFACS                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDCOMFACS                                                      
         PRINT ON                                                               
* DDCCTRYEQUS                                                                   
         PRINT OFF                                                              
       ++INCLUDE DDCTRYEQUS                                                     
         PRINT ON                                                               
* DMFILTABD                                                                     
         PRINT OFF                                                              
       ++INCLUDE DMFILTABD                                                      
         PRINT ON                                                               
* DMPRTQD                                                                       
         PRINT OFF                                                              
       ++INCLUDE DMPRTQD                                                        
         PRINT ON                                                               
* DMPRTQL                                                                       
         PRINT OFF                                                              
       ++INCLUDE DMPRTQL                                                        
         PRINT ON                                                               
* FAFACTS                                                                       
         PRINT OFF                                                              
       ++INCLUDE FAFACTS                                                        
         PRINT ON                                                               
* FADSECTS                                                                      
         PRINT OFF                                                              
       ++INCLUDE FADSECTS                                                       
       ++INCLUDE FAWSSVRD                                                       
* DDSPOOK                                                                       
       ++INCLUDE DDSPOOK                                                        
         PRINT ON                                                               
*&&US                                                                           
* TAGENFILE                                                                     
         PRINT OFF                                                              
       ++INCLUDE TAGENFILE                                                      
         PRINT ON                                                               
* TASYSWORKD                                                                    
         PRINT OFF                                                              
       ++INCLUDE TASYSWORKD                                                     
         PRINT ON                                                               
* SPTRPAT                                                                       
         PRINT OFF                                                              
       ++INCLUDE SPTRPAT                                                        
         PRINT ON                                                               
* NEGENUNIT                                                                     
         PRINT OFF                                                              
       ++INCLUDE NEGENUNIT                                                      
       ++INCLUDE SPTRNREV                                                       
       ++INCLUDE SPTRNRCP                                                       
         PRINT ON                                                               
* SPGENSTAB                                                                     
         PRINT OFF                                                              
       ++INCLUDE SPGENSTAB                                                      
         PRINT ON                                                               
* NEGENUBILL                                                                    
         PRINT OFF                                                              
       ++INCLUDE NEGENUBILL                                                     
         PRINT ON                                                               
* SPTRSHIP                                                                      
         PRINT OFF                                                              
       ++INCLUDE SPTRSHIP                                                       
* SPGENSNV                                                                      
         PRINT OFF                                                              
       ++INCLUDE SPGENSNV                                                       
* SPGENDRORD                                                                    
         PRINT OFF                                                              
       ++INCLUDE SPGENDRORD                                                     
*&&                                                                             
*                                                                               
*&&UK                                                                           
* MEDEMFILED                                                                    
         PRINT OFF                                                              
       ++INCLUDE MEDEMFILED                                                     
         PRINT ON                                                               
* MEFILUNVD                                                                     
         PRINT OFF                                                              
       ++INCLUDE MEFILUNVD                                                      
         PRINT ON                                                               
*&&                                                                             
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'101SRUPD00   10/16/18'                                      
         END                                                                    
