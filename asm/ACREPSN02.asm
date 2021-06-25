*          DATA SET ACREPSN02  AT LEVEL 008 AS OF 04/13/20                      
*PHASE ACSN02B                                                                  
*INCLUDE PRNTBL                                                                 
*INCLUDE SMTP                                                                   
***********************************************************************         
*  ID  LVL   DATE    TICKET            COMMENTS                       *         
* ---- --- ------- ------------ --------------------------------------*         
* RKEJ 008 16JAN20 <SPEC-40066> ADDED EMAIL OVERIDE EID JCL CARD      *         
*                               LOGIC                                 *         
* VGUP 008 18OCT19 <SPEC-40074> ADDED JOB NAME AND ENVIRONMENT IN     *         
*                               MISSING POSTING WORKER FILE ERRORS    *         
***********************************************************************         
         TITLE '- READ WORKER FILES AND PASS TO ACPOSTWRK'                      
ACSN02   CSECT                                                                  
         PRINT NOGEN                                                            
         USING ACWORKD,R7          R7=A(GLOBAL W/S)                             
         USING WORKD,RC            RC=A(LOCAL W/S)                              
         NMOD1 0,**ACSN**,RA,R9                                                 
         L     R7,0(,R1)           ACWORKD                                      
         LA    RC,SPACEND                                                       
*                                                                               
         CLI   MODE,RUNFRST                                                     
         BE    RFRST                                                            
         CLI   MODE,RUNLAST                                                     
         BE    RLAST                                                            
XIT      XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
* RUNFRST - INITIALISE VALUES                                         *         
***********************************************************************         
*                                                                               
RFRST    MVI   RUNINDS,0                                                        
         MVI   MSTRT,0                                                          
*                                                                               
         L     RF,ADMASTC                                                       
         CLI   MCTSTRUN-MASTD(RF),X'FF'                                         
         BNE   *+8                                                              
         OI    RUNINDS,RUNITEST    SET TEST=YES                                 
*                                                                               
         CLI   RCWRITE,YES                                                      
         BE    *+8                                                              
         OI    RUNINDS,RUNIWRNO    SET WRITE=NO                                 
*                                                                               
         OI    RCFLAG1,RCFREPLC    SET LOWER CASE                               
*                                                                               
         USING DICTATED,R1                                                      
         LA    R1,DMCB             GET REPORT LITERALS                          
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
*                                                                               
         USING MASTD,R2                                                         
         L     R2,ADMASTC                                                       
         LA    R3,PHASES           R3=A(LOADABLE PHASE LIST)                    
         LHI   R0,PHASEN                                                        
         LA    R4,APHASES             R4=A(PHASE ADDRESSES)                     
         MVC   MCDUB,=CL8'T00AXX'                                               
                                                                                
RFRST02  GOTOR HEXOUT,DMCB,0(R3),MCDUB+4,1                                      
         GOTOR MCVLOADM,DMCB,0                                                  
         BE    *+6                 MUSTTEST PHASE NOT FOUND                     
         DC    H'0'                                                             
         MVC   0(L'APHASES,R4),4(R1)                                            
         AHI   R3,L'PHASES                                                      
         AHI   R4,L'APHASES                                                     
         BCT   R0,RFRST02                                                       
*                                                                               
***********************************************************************         
* READ AND VALIDATE INPUT CARDS                                       *         
***********************************************************************         
         CLC   =C'CARDS',RCFFPARM  TEST SPECIAL PARM VALUE FOR CARDS            
         BNE   XIT                                                              
         XC    RCFFPARM,RCFFPARM                                                
*                                                                               
RFRST03  GOTO1 CARDS,DMCB,IO,=C'RE00'                                           
         CLI   IO,C'*'             SKIP                                         
         BE    RFRST03             NEXT CARD                                    
         CLC   IO(2),=C'/*'                                                     
         BE    XIT                                                              
*                                                                               
         USING OPTTABD,R2                                                       
RFRST05  LA    R2,OPTTAB           OPTIONAL INPUT CARDS                         
         SR    RF,RF                                                            
*                                                                               
RFRST07  CLI   OPTTABD,OPTTEOTQ    TEST END OF OPTION TABLE                     
         BNE   *+6                                                              
         DC    H'0'                INVALID INPUT OPTION                         
         IC    RF,OPTTKLEN         LENGTH FOR COMPARE                           
         EX    RF,*+8                                                           
         BE    RFRST09                                                          
         CLC   IO(0),OPTTKWRD      MATCH CARD FIELD TO TABLE                    
         LA    R2,OPTTABL(R2)                                                   
         B     RFRST07                                                          
*                                                                               
RFRST09  LA    R1,IO+1(RF)         R1=A(FIRST DATA BYTE)                        
         ICM   RF,3,OPTTVDSP                                                    
         LA    RF,ACSN02(RF)       RF=A(VALIDATION ROUTINE)                     
         BASR  RE,RF               VALIDATE INPUT OPTION                        
         B     RFRST03             GET NEXT CARD                                
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* RUNLAST                                                             *         
***********************************************************************         
*                                                                               
RLAST    L     RF,ADMASTC                                                       
         MVC   SENO,MCIDSENO-MASTD(RF)                                          
         XC    WKID,WKID           CLEAR WORKER-ID                              
         MVI   RCSUBPRG,1                                                       
         EJECT                                                                  
***********************************************************************         
* READ ALL WKFILE INDEX RECORDS AND FILTER OUT WORKER FILES THAT ARE  *         
* REQUIRED FOR THIS ACCOUNT SYSTEM. WORKER FILES THAT APPLY ARE READ  *         
* AND (IF VALID) ARE PASSED TO ACPOSTWRK                              *         
***********************************************************************         
*                                                                               
WORKI02  GOTO1 WORKER,DMCB,WKINDX,AIWORK,WKID,0                                 
         TM    8(R1),X'80'         TEST END OF INDEX                            
         BNZ   WORKIX                                                           
*                                                                               
         CLI   WKTYPE,WKTPOST      POSTING-TYPE ONLY                            
         BNE   WORKI02                                                          
*                                                                               
         TM    WKSTAT,WKSKEEP      DON'T WANT KEEP-STATUS IDS                   
         BNZ   WORKI02                                                          
*                                                                               
         TM    WKSTAT,WKSHOLD      DON'T WANT HOLD-STATUS IDS                   
         BNZ   WORKI02                                                          
*                                                                               
         GOTO1 GETUID,WKUSER       GET COMPANY CODE                             
         BNE   WORKI02                                                          
*                                                                               
         GOTO1 GETCPY,WKCPY        READ COMPANY AND TEST OPTION                 
         BNE   WORKI02                                                          
*                                                                               
WORKI04  CLC   RCFFPARM(L'WKSYSPRG),SPACES                                      
         BNH   WORKI06                                                          
         CLC   WKSYSPRG,RCFFPARM   APPLY SINGLE WORK FILE FILTER                
         BNE   WORKI02                                                          
         B     WORKI10                                                          
*                                                                               
WORKI06  CLI   FLTTYP,0            TEST ANY FILE TYPE FILTERS                   
         BE    WORKI10                                                          
         LA    RF,FLTTYP           MATCH FILE TO FILTER TABLE                   
         LA    R0,FLTTYPN                                                       
*                                                                               
WORKI08  CLI   0(RF),0             TEST END OF TABLE                            
         BE    WORKI02             NOT FOUND, SKIP WORKER FILE                  
         CLC   WKSYSPRG,0(RF)      TAKE ONLY FILES IN TABLE                     
         BE    WORKI10                                                          
         LA    RF,L'FLTTYP(RF)                                                  
         BCT   R0,WORKI08                                                       
         B     WORKI02                                                          
*                                                                               
WORKI10  GOTO1 GETPRG,WKSYSPRG     CHECK SYSTEM/PROGRAM VALID FOR ACSN          
         BNE   WORKI02                                                          
*                                                                               
         XC    WKIO(256),WKIO                                                   
         GOTO1 WORKER,DMCB,WKREAD,AIWORK,WKID,WKIOLN                            
         CLI   8(R1),0                                                          
         BNE   WORKI02                                                          
*                                                                               
         MVC   P,SPACES                                                         
         GOTO1 DISWRK,P+1                                                       
         MVI   FORCEHED,C'Y'                                                    
         MVI   RCSUBPRG,1                                                       
         GOTO1 ACREPORT                                                         
*                                                                               
         USING RUNXTRAD,R6                                                      
         L     R6,VEXTRAS                                                       
         OI    RCFLAG3,X'80'       INDICATE COMING FROM ACSN                    
         MVC   SVACSPEC,ACSPECS                                                 
         GOTO1 POSTWRK,DMCB,ACWORKD,WKID                                        
         MVC   ACSPECS,SVACSPEC                                                 
         NI    RCFLAG3,X'FF'-X'80' CLEAR JUST IN CASE                           
         TM    ERRIND,UPDERR       DO WE HAVE ERROR FROM POSTWRK/UPDT?          
         BO    WORKI12                                                          
*        CP    UPDATERR,=P'0'                                                   
*        BNE   WORKI12                                                          
         GOTO1 DATAMGR,DMCB,=C'COMMIT'  ISSUE COMMIT                            
         B     WORKI02                                                          
*                                                                               
WORKI12  MVC   EMDETL(L'SVWRKFID),SVWRKFID                                      
         OC    EMDETL,SPACES                                                    
         BAS   RE,SENDMAIL                                                      
         NI    ERRIND,X'FF'-UPDERR ERROR HAS BEEN HANDLED - RESET               
         B     WORKI02                                                          
*                                                                               
WORKIX   CLI   MSTRT,0             ANY EMAILS?                                  
         BE    XIT                 NO                                           
         MVI   MSTRT,2                                                          
         BAS   RE,SENDMAIL                                                      
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO READ THE ID RECORD AND GET THE COMPANY CODE              *         
***********************************************************************         
*                                                                               
         USING UIDTABD,R2          R2=A(USER-ID TABLE)                          
GETUID   NTR1  ,                                                                
         L     R2,AUIDTAB                                                       
         LA    R0,UIDTABMX         R0=MAXIMUM NUMBER OF ENTRIES                 
*                                                                               
GETUID02 OC    UIDTUSER,UIDTUSER   TEST EOT                                     
         BZ    GETUID04                                                         
         CLC   UIDTUSER,WKUSER     MATCH ON USER-ID NUMBER                      
         BE    GETUID40                                                         
         LA    R2,UIDTABL(R2)                                                   
         BCT   R0,GETUID02                                                      
         DC    H'0'                USER-ID TABLE FULL                           
*                                                                               
GETUID04 MVC   UIDTUSER,WKUSER     SAVE IN TABLE                                
*                                                                               
         LA    R3,IO                                                            
         USING CTIREC,R3                                                        
         XC    CTIKEY,CTIKEY       READ CONTROL FILE FOR COMPANY                
         MVI   CTIKTYP,CTIKTYPQ                                                 
         MVC   CTIKNUM,UIDTUSER                                                 
         GOTOR DATAMGR,DMCB,DMREAD,CTFILE,CTIKEY,CTIKEY                         
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
GETUID40 CLI   UIDTSENO,0          TEST ACCOUNT SYSTEM SE NUMBER SET            
         BE    GETUIDN                                                          
         ST    R2,AUIDNTRY                                                      
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
*                                                                               
GETCPY   NTR1  ,                                                                
         LA    R2,IO                                                            
         USING CPYRECD,R2                                                       
         MVC   CPYKEY,SPACES                                                    
         L     R1,AUIDNTRY         GET THE COMPANY CODE                         
         MVC   CPYKCPY,UIDTCPY-UIDTABD(R1)                                      
         GOTO1 DATAMGR,DMCB,DMREAD,ACCFIL,CPYRECD,CPYRECD                       
         BNE   GETCPYN                                                          
*                                                                               
         LA    R1,CPYRECD                                                       
         AH    R1,DATADISP                                                      
*                                                                               
GETCPY04 CLI   0(R1),0             TEST END OF RECORD                           
         BE    GETCPYN                                                          
*                                                                               
         USING CPYELD,R1                                                        
         CLI   CPYEL,CPYELQ        TEST COMPANY ELEMENT                         
         BE    GETCPY06                                                         
*                                                                               
         LLC   R0,1(R1)            BUMP TO NEXT ELEMENT                         
         AR    R1,R0                                                            
         B     GETCPY04                                                         
*                                                                               
GETCPY06 CLI   CPYLN,CPYLN3Q                                                    
         BNH   GETCPYN                                                          
         TM    CPYSTATC,CPYRQBFE   REQUESTED BALANCED FILE?                     
         BNO   GETCPYN             NO, SKIP THIS WORKER FILE                    
*                                                                               
GETCPYY  CR    RE,RE                                                            
         B     GETCPYX                                                          
*                                                                               
GETCPYN  CLI   *,0                                                              
*                                                                               
GETCPYX  B     XIT                                                              
         DROP  R1,R2                                                            
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO LOOK-UP SYSTEM/PROGRAM IN PRGTAB                         *         
*                                                                     *         
* NTRY - R1=A(SYSTEM/PROGRAM VALUE)                                   *         
* EXIT - R5=A(PRGTAB ENTRY) WITH CC EQUAL IF FOUND                    *         
*        R5=A(0) WITH CC NOT EQUAL IF NOT FOUND                       *         
***********************************************************************         
*                                                                               
         USING PRGTABD,R5                                                       
GETPRG   L     R5,APRGTAB                                                       
*                                                                               
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
         DROP  R5                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO FORMAT A WORKER KEY INTO PRINTABLE FORMAT                *         
*                                                                     *         
* NTRY - R1=A(OUTPUT AREA), WKID CONTAINS WORKER KEY                  *         
*                                                                     *         
* EXIT - R1=A(NEXT AVAILABLE OUTPUT BYTE)                             *         
***********************************************************************         
DISWRK   NTR1  ,                                                                
         XC    SVWRKFID,SVWRKFID                                                
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
*                                                                               
DISWRK02 MVC   0(L'UIDTCODE,R2),UIDTCODE                                        
         LA    R2,L'UIDTCODE-1(R2)                                              
         CLI   0(R2),C' '                                                       
         BH    *+8                                                              
         BCT   R2,*-8                                                           
         MVI   1(R2),C','                                                       
         LA    R2,2(R2)                                                         
         MVC   0(L'WKSYSPRG,R2),WKSYSPRG                                        
         LA    R2,L'WKSYSPRG(R2)                                                
         MVC   0(L'WKSUB,R2),WKSUB                                              
         CLI   0(R2),0                                                          
         BNE   *+8                                                              
         MVI   0(R2),C'*'                                                       
         LA    R2,L'WKSUB(R2)                                                   
         UNPK  WORK(3),WKDAY(2)                                                 
         MVC   0(2,R2),WORK                                                     
         MVC   2(L'WKTYPE,R2),WKTYPE                                            
         MVI   3(R2),C','                                                       
         LA    R2,4(R2)                                                         
         SR    R0,R0                                                            
         ICM   R0,3,WKSEQN                                                      
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  0(5,R2),DUB                                                      
         CLI   0(R2),C'0'                                                       
         BNE   *+14                                                             
         MVC   0(5,R2),1(R2)                                                    
         B     *-14                                                             
         LA    R1,4(R2)                                                         
         CLI   0(R1),C' '                                                       
         BH    *+8                                                              
         BCT   R1,*-8                                                           
         LA    R1,2(R1)                                                         
*                                                                               
         LA    RE,P+1                                                           
         SR    R2,RE                                                            
         EX    R2,*+8                                                           
         B     *+10                                                             
         MVC   SVWRKFID(0),P+1                                                  
*                                                                               
DISWRKX  XIT1  REGS=(R1)           RETURN A(NEXT SLOT) TO CALLER                
         DROP  R1                                                               
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
**********************************************************************          
* EMAIL ERROR REPORT                                                 *          
**********************************************************************          
*                                                                               
SENDMAIL NTR1                                                                   
         CLI   MSTRT,0             INITIALIZE IF FIRST CALL                     
         BNE   SENDM02                                                          
         USING MASTD,R2                                                         
         L     R2,ADMASTC                                                       
         MVC   SUBJNME,MCJOB                                                    
         USING SSOOFFD,RF                                                       
         L     RF,MCSSB                                                         
         DROP  R2                                                               
         MVC   SUBENVT,=C'PROD'      DEFAULT ALLOCATED PROD                     
         CLI   SSODSPAC,C'A'                                                    
         BE    SENDM01                                                          
         MVC   SUBENVT,=C'CSC '                                                 
         CLI   SSODSPAC,C'C'           IS IT CSC                                
         BE    SENDM01                                                          
         MVC   SUBENVT,=C'FQA '                                                 
         CLI   SSODSPAC,C'Q'           IS IT FQA                                
         BE    SENDM01                                                          
         MVC   SUBENVT,=C'TST '                                                 
         DROP  RF                                                               
*                                                                               
         USING MASTD,RE                                                         
SENDM01  L     RE,ADMASTC                                                       
         L     RF,MCAEXTRA                                                      
         USING MCEXTRA,RF                                                       
         CLC   MC@EMAIL,SPACES          DID WE PASS EID = EMAIL ADDRESS         
         JNH   *+16                     NO -SEND EMAIL TO EXISTING USER         
         MVC   TOWHO,SPACES             YES -OVERRIDE EMAIL ADDRESSES           
         MVC   TOWHO,MC@EMAIL           WITH PASSED EID                         
         DROP  RE,RF                                                            
*                                                                               
         GOTOR VSMTP1,DMCB,('SMTPAINI',JESMAIL)                                 
         GOTOR VSMTP1,DMCB,('SMTPAPRS',TOWHO),(L'SUBDESC,SUBDESC)               
         GOTOR VSMTP1,DMCB,('SMTPAPTL',EMHEAD)                                  
         GOTOR VSMTP1,DMCB,('SMTPAPTL',EMHEADU)                                 
         MVI   MSTRT,1                                                          
*                                                                               
SENDM02  CLI   MSTRT,1                                                          
         BNE   SENDM04                                                          
         GOTOR VSMTP1,DMCB,('SMTPAPTL',EMDETAIL)                                
         B     SENDMX                                                           
*                                                                               
SENDM04  CLI   MSTRT,2             FINISHED WITH THIS EMAIL?                    
         BNE   SENDMX              NO                                           
         GOTOR VSMTP1,DMCB,('SMTPASND',0)                                       
         GOTOR VSMTP1,DMCB,('SMTPAEND',0) DETACH SMTP                           
*                                                                               
SENDMX   B     XIT                                                              
         EJECT                                                                  
         LTORG                                                                  
YES      EQU   C'Y'                                                             
*                                                                               
AIWORK   DC    A(IWORK)                                                         
APRGTAB  DC    A(PRGTAB)                                                        
VPRNTBL  DC    V(PRNTBL)                                                        
VSMTP1   DC    V(SMTP)                                                          
AUIDTAB  DC    A(UIDTAB)                                                        
AUIDNTRY DC    A(0)                                                             
*                                                                               
PHASES   DS    0AL1                LOADABLE PHASES                              
         DC    AL1(QPOSTWRK)                                                    
PHASEN   EQU   (*-PHASES)/L'PHASES                                              
         EJECT                                                                  
ACCFIL   DC    C'ACCOUNT'                                                       
CTFILE   DC    C'CTFILE '                                                       
*                                                                               
WKREAD   DC    C'READ   '                                                       
WKINDX   DC    C'INDEX  '                                                       
WKCLOS   DC    C'CLOSE  '                                                       
*                                                                               
*                                                                               
FLTTYPN  EQU   20                                                               
FLTTYP   DC    (FLTTYPN)XL3'00'    FILTER TYPE TABLE                            
*                                                                               
MSTRT    DS    X                   EMAIL STAGE                                  
JESMAIL  DC    CL8'JESMAIL '                                                    
TOWHO    DC    CL48'NA-OOB_TEAM:'                                               
*OWHO    DC    C'RDES:'                                                         
SUBDESC  DS    0CL80                                                            
         DC    CL44'THE FOLLOWING WORKER FILES CONTAINED ERRORS'                
         DC    CL3' - '                                                         
SUBJNME  DS    CL8               JOB NAME                                       
         DC    CL3' - '                                                         
SUBENVT  DS    CL4               ENVIRONMENT CSC - PROD - TST - FQA             
         DC    CL18' '                                                          
*                                                                               
EMHEAD   DS    0CL80                                                            
         DC    CL80'AGENCY/WORK ID'                                             
EMHEADU  DS    0CL80                                                            
         DC    CL80' '                                                          
EMDETAIL DS    0CL80                                                            
EMDETL   DS    CL20                                                             
         DC    CL60' '                                                          
         EJECT                                                                  
***********************************************************************         
* INPUT CARD OPTIONS                                                  *         
***********************************************************************         
OPTTAB   DS    0X                                                               
         DC    AL1(4),C'TYPE=     ',AL2(VALTYP-ACSN02)                          
*                                                                               
OPTTABX  DC    AL1(OPTTEOTQ)                                                    
*                                                                               
OPTTABD  DSECT                     ** CARD OPTION TABLE **                      
OPTTEOTQ EQU   0                   END OF TABLE INDICATOR                       
OPTTKLEN DS    XL1                 LENGTH OF KEYWORD                            
OPTTKWRD DS    CL10                KEYWORD                                      
OPTTVDSP DS    AL2                 DISPLACEMENT TO VALIDATION ROUTINE           
OPTTABL  EQU   *-OPTTABD           LENGTH OF TABLE ENTRY                        
         EJECT ,                                                                
***********************************************************************         
* ACSN02 CSECT                                                        *         
***********************************************************************         
ACSN02   CSECT                                                                  
         EJECT                                                                  
DICI     DS    0X                  ** DICTIONARY INPUT **                       
         DCDDL AC#PGAAJ,30                                                      
         DCDDL AC#PGAAT,30                                                      
         DCDDL AC#PGAA1,30                                                      
         DCDDL AC#PGAA2,30                                                      
         DCDDL AC#PGAA8,30                                                      
         DCDDL AC#PGACE,30                                                      
         DCDDL AC#PGADJ,30                                                      
         DCDDL AC#PGADK,30                                                      
         DCDDL AC#PGAFA,30                                                      
         DCDDL AC#PGAIO,30                                                      
         DCDDL AC#PGAPA,30                                                      
         DCDDL AC#PGAPE,30                                                      
         DCDDL AC#PGARU,30                                                      
         DCDDL AC#PGASA,30                                                      
         DCDDL AC#PGAT4,30                                                      
         DCDDL AC#PGAT5,30                                                      
*                                                                               
         DCDDL AC#PGA05,30                                                      
         DCDDL AC#PGA07,30                                                      
         DCDDL AC#PGA08,30                                                      
         DCDDL AC#PGA09,30                                                      
         DCDDL AC#PGA21,30                                                      
         DCDDL AC#PGA23,30                                                      
         DCDDL AC#PGA25,30                                                      
         DCDDL AC#PGA27,30                                                      
         DCDDL AC#PGA29,30                                                      
         DCDDL AC#PGA47,30                                                      
         DCDDL AC#PGA54,30                                                      
         DCDDL AC#PGA55,30                                                      
         DCDDL AC#PGA8A,30                                                      
         DCDDL AC#PGA90,30                                                      
         DCDDL AC#PGA91,30                                                      
         DCDDL AC#PGA98,30                                                      
*                                                                               
         DCDDL AC#PGNBA,30                                                      
         DCDDL AC#PGNMX,30                                                      
         DCDDL AC#PGNMY,30                                                      
*                                                                               
         DCDDL AC#PGPBA,30                                                      
         DCDDL AC#PGPMX,30                                                      
         DCDDL AC#PGPMY,30                                                      
         DCDDL AC#PGP05,30                                                      
         DCDDL AC#PGP54,30                                                      
*                                                                               
         DCDDL AC#PGSBA,30                                                      
         DCDDL AC#PGSMV,30                                                      
         DCDDL AC#PGSMX,30                                                      
         DCDDL AC#PGSMY,30                                                      
         DCDDL AC#PGS03,30                                                      
         DCDDL AC#PGS54,30                                                      
*                                                                               
         DCDDL AC#PGTBI,30                                                      
         DCDDL AC#PGTCK,30                                                      
         DCDDL AC#PGTCN,30                                                      
         DCDDL AC#PGTEC,30                                                      
         DCDDL AC#PGTPK,30                                                      
         DCDDL AC#PGTVB,30                                                      
                                                                                
DICIX    DC    AL1(0)                                                           
         SPACE 2                                                                
DICO     DS    0C                  ** DICTIONARY OUTPUT **                      
         DSDDL                                                                  
DICOX    DS    0C                                                               
         EJECT                                                                  
WORKD    DSECT                     ** LOCAL WORKING STORAGE **                  
RUNINDS  DS    XL1                 RUN CONTROL INDICATORS                       
RUNITEST EQU   X'80'               .  TEST=YES                                  
RUNIWRNO EQU   X'40'               .  WRITE=NO                                  
RUNINOLO EQU   X'20'               .  NOLOGOS                                   
RUNIREMO EQU   X'10'               .  REMOTE COMPANY                            
RUNISCOA EQU   X'08'               .  SUPPRESS COMPANY ANALYSIS                 
*                                                                               
SENO     DS    XL1                 ACCOUNT SYSTEM SE NUMBER                     
SVACSPEC DS    A                   SAVE MY SPECS                                
SVWRKFID DS    CL20                SAVE WORKER FILE ID                          
*                                                                               
APHASES  DS    0A                  ** ADDRESSES OF LOADED PHASES **             
POSTWRK  DS    A                                                                
*                                                                               
WKID     DS    0XL16               ** WORKER FILE KEY **                        
WKKEY    DS    0XL8                WORKER KEY                                   
WKUSER   DS    XL2                 USER-ID NUMBER                               
WKSYSPRG DS    0CL3                                                             
WKSYS    DS    CL1                 SYSTEM CODE                                  
WKSYSACC EQU   C'A'                ACCOUNTING                                   
WKSYSNET EQU   C'N'                NETPAK                                       
WKSYSPRT EQU   C'P'                SPOT                                         
WKSYSSPT EQU   C'S'                PRINT                                        
WKSYSTAL EQU   C'T'                TALENT                                       
WKPRG    DS    CL2                 PROGRAM ID                                   
WKSUB    DS    XL1                 SUB-PROGRAM/LEDGER CODE                      
WKCPY    EQU   WKSUB               COMPANY CODE (SOME WORKER FILES)             
WKDAY    DS    PL1                 DAY ADDED (PWOS DD)                          
WKTYPE   DS    CL1                 FILE TYPE                                    
WKTPOST  EQU   C'P'                POSTING FILE                                 
         DS    XL2                 N/D                                          
WKSEQN   DS    XL2                 FILE SEQUENCE NUMBER                         
WKSTAT   DS    XL1                 WORKER STATUS                                
WKSHOLD  EQU   X'40'               STATUS HOLD                                  
WKSKEEP  EQU   X'08'               STATUS KEEP                                  
         ORG   WKID+L'WKID                                                      
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
* DDCOREQUS                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDCOREQUS                                                      
         PRINT ON                                                               
* ACGENFILE                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACGENFILE                                                      
         PRINT ON                                                               
* CTGENFILE                                                                     
         PRINT OFF                                                              
       ++INCLUDE CTGENFILE                                                      
         PRINT ON                                                               
* DDDICTATED                                                                    
         PRINT OFF                                                              
       ++INCLUDE DDDICTATED                                                     
         PRINT ON                                                               
* DDREPXTRAD                                                                    
         PRINT OFF                                                              
       ++INCLUDE DDREPXTRAD                                                     
         PRINT ON                                                               
* DDSMTPD                                                                       
         PRINT OFF                                                              
       ++INCLUDE DDSMTPD                                                        
         PRINT ON                                                               
         EJECT                                                                  
* FASSBOFF                                                                      
         PRINT OFF                                                              
SSOOFFD  DSECT                                                                  
       ++INCLUDE FASSBOFF                                                       
         PRINT ON                                                               
PRGTABD  DSECT                     ** PROGRAM TABLE DSECT **                    
PRGTSP   DS    0CL3                                                             
PRGTSYS  DS    CL1                 SYSTEM CODE                                  
PRGTEOTQ EQU   255                 END OF TABLE INDICATOR                       
PRGTPRG  DS    CL2                 PROGRAM ID                                   
PRGTNAML EQU   30                  PROGRAM NAME LENGTH                          
PRGTDISP DS    AL2                 DISPLACEMENT TO NAME IN DICO                 
PRGTABL  EQU   *-PRGTABD                                                        
*                                                                               
UIDTABD  DSECT                     ** USER-ID TABLE **                          
UIDTUSER DS    XL2                 USER-ID NUMBER                               
UIDTCODE DS    CL10                USER-ID CODE                                 
UIDTCPY  DS    XL1                 COMPANY CODE                                 
UIDTSENO DS    XL1                 ACCOUNT SYSTEM SE NUMBER                     
UIDTLANG DS    XL1                 LANGUAGE CODE                                
UIDTALPH DS    CL2                 ALPHA-ID CODE                                
UIDTABL  EQU   *-UIDTABD                                                        
UIDTABMX EQU   1024                MAXIMUM NUMBER OF USER-ID'S                  
         EJECT                                                                  
ACSN02   CSECT                                                                  
PRGTAB   DS    0X                  ** SYSTEM/PROGRAM TABLE **                   
*                                                                               
*                                       ACCOUNTING                              
*                                                                               
         DC    AL1(WKSYSACC),C'AJ',AL2(AC@PGAAJ-DICO)                           
         DC    AL1(WKSYSACC),C'AT',AL2(AC@PGAAT-DICO)                           
         DC    AL1(WKSYSACC),C'A1',AL2(AC@PGAA1-DICO)                           
         DC    AL1(WKSYSACC),C'A2',AL2(AC@PGAA2-DICO)                           
         DC    AL1(WKSYSACC),C'A8',AL2(AC@PGAA8-DICO)                           
         DC    AL1(WKSYSACC),C'CE',AL2(AC@PGACE-DICO)                           
         DC    AL1(WKSYSACC),C'DJ',AL2(AC@PGADJ-DICO)                           
*        DC    AL1(WKSYSACC),C'DK',AL2(AC@PGADK-DICO)                           
         DC    AL1(WKSYSACC),C'FA',AL2(AC@PGAFA-DICO)                           
         DC    AL1(WKSYSACC),C'IO',AL2(AC@PGAIO-DICO)                           
         DC    AL1(WKSYSACC),C'PA',AL2(AC@PGAPA-DICO)                           
         DC    AL1(WKSYSACC),C'PE',AL2(AC@PGAPE-DICO)                           
         DC    AL1(WKSYSACC),C'RU',AL2(AC@PGARU-DICO)                           
         DC    AL1(WKSYSACC),C'SA',AL2(AC@PGASA-DICO)                           
         DC    AL1(WKSYSACC),C'T4',AL2(AC@PGAT4-DICO)                           
         DC    AL1(WKSYSACC),C'T5',AL2(AC@PGAT5-DICO)                           
*                                                                               
         DC    AL1(WKSYSACC),C'05',AL2(AC@PGA05-DICO)                           
         DC    AL1(WKSYSACC),C'07',AL2(AC@PGA07-DICO)                           
         DC    AL1(WKSYSACC),C'08',AL2(AC@PGA08-DICO)                           
         DC    AL1(WKSYSACC),C'09',AL2(AC@PGA09-DICO)                           
         DC    AL1(WKSYSACC),C'21',AL2(AC@PGA21-DICO)                           
         DC    AL1(WKSYSACC),C'23',AL2(AC@PGA23-DICO)                           
         DC    AL1(WKSYSACC),C'25',AL2(AC@PGA25-DICO)                           
         DC    AL1(WKSYSACC),C'27',AL2(AC@PGA27-DICO)                           
         DC    AL1(WKSYSACC),C'29',AL2(AC@PGA29-DICO)                           
         DC    AL1(WKSYSACC),C'47',AL2(AC@PGA47-DICO)                           
         DC    AL1(WKSYSACC),C'54',AL2(AC@PGA54-DICO)                           
         DC    AL1(WKSYSACC),C'55',AL2(AC@PGA55-DICO)                           
         DC    AL1(WKSYSACC),C'8A',AL2(AC@PGA8A-DICO)                           
         DC    AL1(WKSYSACC),C'90',AL2(AC@PGA90-DICO)                           
         DC    AL1(WKSYSACC),C'91',AL2(AC@PGA91-DICO)                           
         DC    AL1(WKSYSACC),C'98',AL2(AC@PGA98-DICO)                           
*                                                                               
*                                       NET                                     
*                                                                               
         DC    AL1(WKSYSNET),C'BA',AL2(AC@PGNBA-DICO)                           
         DC    AL1(WKSYSNET),C'ML',AL2(AC@PGNMX-DICO)                           
         DC    AL1(WKSYSNET),C'MX',AL2(AC@PGNMX-DICO)                           
         DC    AL1(WKSYSNET),C'MY',AL2(AC@PGNMY-DICO)                           
*                                                                               
*                                       PRINT                                   
*                                                                               
         DC    AL1(WKSYSPRT),C'BA',AL2(AC@PGPBA-DICO)                           
         DC    AL1(WKSYSPRT),C'ML',AL2(AC@PGPMX-DICO)                           
         DC    AL1(WKSYSPRT),C'MX',AL2(AC@PGPMX-DICO)                           
         DC    AL1(WKSYSPRT),C'MY',AL2(AC@PGPMY-DICO)                           
         DC    AL1(WKSYSPRT),C'05',AL2(AC@PGP05-DICO)                           
         DC    AL1(WKSYSPRT),C'54',AL2(AC@PGP54-DICO)                           
*                                                                               
*                                       SPOT                                    
*                                                                               
         DC    AL1(WKSYSSPT),C'BA',AL2(AC@PGSBA-DICO)                           
         DC    AL1(WKSYSSPT),C'ML',AL2(AC@PGSMX-DICO)                           
         DC    AL1(WKSYSSPT),C'MV',AL2(AC@PGSMV-DICO)                           
         DC    AL1(WKSYSSPT),C'MX',AL2(AC@PGSMX-DICO)                           
         DC    AL1(WKSYSSPT),C'MY',AL2(AC@PGSMY-DICO)                           
         DC    AL1(WKSYSSPT),C'03',AL2(AC@PGS03-DICO)                           
         DC    AL1(WKSYSSPT),C'54',AL2(AC@PGS54-DICO)                           
*                                                                               
*                                       TALENT                                  
*                                                                               
         DC    AL1(WKSYSTAL),C'BI',AL2(AC@PGTBI-DICO)                           
         DC    AL1(WKSYSTAL),C'PB',AL2(AC@PGTBI-DICO)                           
         DC    AL1(WKSYSTAL),C'CK',AL2(AC@PGTCK-DICO)                           
         DC    AL1(WKSYSTAL),C'CU',AL2(AC@PGTCK-DICO)                           
         DC    AL1(WKSYSTAL),C'PC',AL2(AC@PGTCK-DICO)                           
         DC    AL1(WKSYSTAL),C'CN',AL2(AC@PGTCN-DICO)                           
         DC    AL1(WKSYSTAL),C'EC',AL2(AC@PGTEC-DICO)                           
         DC    AL1(WKSYSTAL),C'PK',AL2(AC@PGTPK-DICO)                           
         DC    AL1(WKSYSTAL),C'VB',AL2(AC@PGTVB-DICO)                           
         DC    AL1(WKSYSTAL),C'P1',AL2(AC@PGTCK-DICO)                           
         DC    AL1(WKSYSTAL),C'P2',AL2(AC@PGTPK-DICO)                           
*                                                                               
PRGTABX  DC    AL1(PRGTEOTQ)                                                    
         EJECT                                                                  
***********************************************************************         
* WORK AREAS                                                                    
***********************************************************************         
*                                                                               
UIDTAB   DC    (UIDTABMX)XL(UIDTABL)'00'                                        
*                                                                               
IWORK    DC    6144X'00'                                                        
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'008ACREPSN02 04/13/20'                                      
         END                                                                    
