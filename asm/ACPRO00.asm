*          DATA SET ACPRO00    AT LEVEL 035 AS OF 07/10/20                      
*PHASE T60B00A                                                                  
*INCLUDE TWABLD                                                                 
*INCLUDE VATICAN                                                                
*INCLUDE PRORATA                                                                
*INCLUDE ACJOBCOL                                                               
*INCLUDE ACPROFLT                                                               
*INCLUDE SRCHPASS                                                               
*INCLUDE ACSRCHP                                                                
*INCLUDE JOBAPP                                                                 
*INCLUDE ACRAPPER                                                               
*INCLUDE KHDUMMY                                                                
         TITLE 'T60B00 - PRODUCTION CONTROLLER'                                 
* GHOA 14FEB18 034 DSRD-18081 NEW OPT/MAIN 'MAIL IF PERSN ASSIGN 2 JOB'         
* ASAX 26MAY20 035 DSRD-25987 NEW OPT/MAIN 'DATE EST IN LAST FISCAL YR'         
T60B00   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 LENWORK,T60B00,R7,RR=R2,CLEAR=YES                                
         ST    R2,RELO                                                          
         LR    R9,R1                                                            
         LR    R8,RC                                                            
         USING SPOOLD,R8                                                        
         LA    RC,SPOOLEND                                                      
         USING GEND,RC                                                          
         ST    R9,SYSPARMS                                                      
         LA    R9,IO                                                            
         AH    R9,=Y(LENIOAS)      GRABBING 3 2000 BYTE I/O AREAS               
         ST    R9,ASUBSYSD                                                      
         USING SUBSYSD,R9                                                       
                                                                                
         L     RE,20(R1)                                                        
         MVC   FACFLAG,7(RE)       SAVE CONNECT FLAG                            
         MVC   FACUPD,10(RE)       AND UPDATIVE FACPAK ID                       
                                                                                
         L     R1,SYSPARMS                                                      
         L     RA,4(R1)                                                         
         USING T60BFFD,RA                                                       
                                                                                
PRO      MVI   SPACES,C' '                                                      
         MVC   SPACES+1(131),SPACES                                             
         BAS   RE,SYSINIT          INITIALIZE SYSTEM DEPENDENT                  
         OI    CONSERVH+6,X'01'    SERVICE FIELD IS ALWAYS MODIFIED             
         OI    CONSERVH+6,X'80'                                                 
         ST    RB,SYSRB            SYSTEM BASE REGS                             
         ST    RD,SYSRD                                                         
         ST    R7,BASER7           SAVE SECOND BASE REGISTER                    
*                                                                               
         OC    TWAVPRNT,TWAVPRNT   IF OFFLINE                                   
         BNZ   PRO1                THEN SKIP DDLINK STUFF                       
         L     R2,SYSPARMS         TEST IF CALLED FROM DDLINK UPLOAD            
         L     R2,16(R2)           R2=A(COMFACS)                                
         L     RF,CGLOBBER-COMFACSD(R2)                                         
         GOTO1 (RF),DMCB,=C'GETD',BLOCK,10,GLVDLUWF                             
         CLI   8(R1),0             WORKER FILE GLOBAL NOT PRESENT               
         BNE   PRO1                                                             
         L     RF,CGENNEW-COMFACSD(R2)                                          
         MVI   GENSTAT6,GES$UPLD   ENABLE DDLINK AUTO UPLOAD FOR                
         MVI   MODE,TESTGLOB       PRESTO JOB MAINTENANCE                       
         GOTO1 (RF),DMCB,SPOOLD                                                 
         DC    H'0'                GENNEW SHOULD RETURN TO MONITOR              
*                                                                               
PRO1     MVI   RACHANGE,C'N'                                                    
         TM    CONRECH+4,X'20'     TEST IF RECORD CHANGED                       
         BZ    *+12                YES                                          
         TM    CONACTH+4,X'20'     TEST IF ACTION CHANGED                       
         BO    *+8                 NO                                           
         MVI   RACHANGE,C'Y'       NOTE CHANGE IN EITHER                        
*                                                                               
         CLI   CONRECH+5,0         IF FALINK THEN INIT GENCON, GET              
         BE    PRO2                TWAALIAS AND LOAD IN 70                      
         ZIC   R2,CONRECH+5                                                     
         BCTR  R2,0                                                             
         EX    R2,FALCOMP                                                       
         BNE   PRO2                                                             
*                                                                               
         CLI   CONACTH+5,0                                                      
         BE    PRO2                                                             
         ZIC   R2,CONACTH+5                                                     
         BCTR  R2,0                                                             
         EX    R2,MNTCOMP                                                       
         BNE   PRO2                                                             
*                                                                               
         MVI   GCMODE,C'S'                                                      
         GOTO1 GENCON,DMCB,(R8)    INITIALIZE GENCON                            
         MVI   GCMODE,C' '                                                      
*                                                                               
         GOTO1 CALLOV,DMCB,(X'70',0),0                                          
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         XR    RF,RF                                                            
         ICM   RF,7,1(R1)                                                       
         GOTO1 (RF),DMCB,(RC)                                                   
         B     XIT                                                              
*                                                                               
PRO2     CLC   CONPRSHH+8(6),LOWPERS                                            
         BNE   PRO4                                                             
         CLI   TWAMODE,1           TEST OFF-LINE                                
         BE    PRO4                YES                                          
*                                                                               
         MVI   GCMODE,C'S'                                                      
         GOTO1 GENCON,DMCB,(R8)    INITIALIZE GENCON                            
         MVI   GCMODE,C' '                                                      
         LA    R2,CONPERSH         ALLOW INPUT OF PERSON HERE                   
         GOTO1 ANY                                                              
         MVC   TWAALIAS,WORK                                                    
*                                                                               
PRO4     CLI   CONREC,C'E'         IF RECORD IS 'ESTIMATE', PASS IT             
         BNE   PRO5                ON TO GENCON FOR SECURITY                    
         CLI   CONREC+1,C'L'       RECORD 'ELIST' DOESN'T COUNT                 
         BE    PRO5                                                             
         CLI   THISLSEL,C'R'       IF DISPLAYING OR SELECTING                   
         BE    PRO5                FOR REPORT, VALIDATE FOR REPORT              
         CLI   THISLSEL,REPSELQ                                                 
         BE    PRO5                                                             
         OI    GENSTAT4,LEAVEACT   DO NOT CHANGE ACTION TO REPORT               
*                                                                               
PRO5     OI    GENSTAT3,OKVALSEL                                                
*                                                                               
                                                                                
         BAS   RE,CALLGENC         OFF TO GENCON                                
                                                                                
         CLI   GOAGAIN,C'Y'        TEST FOR FLAG TO CALL GENCON AGAIN           
         BNE   PRO6                NO                                           
         NI    CONRECH+6,X'BF'     RESET INSERT CURSOR BIT                      
         B     PRO4                                                             
*                                                                               
PRO6     OI    CONRECH+4,X'20'     SET PREV VALIDATED BITS                      
         OI    CONACTH+4,X'20'                                                  
         MVC   LASTOV,OVERLAY      SAVE THIS TIME OVERLAY                       
         MVC   LASTCOMP,CUL        SAVE THIS TIME COMPANY CODE                  
*                                                                               
PROX     B     XIT                 THEN WE'RE THROUGH                           
         EJECT                                                                  
*              INITIALIZE SYSTEM ADDRESSES                                      
*                                                                               
SYSINIT  NTR1                                                                   
*                                  GET TERMINAL VALUES                          
         MVI   DDS,C'N'                                                         
         CLI   TWAOFFC,C'*'        TEST FOR DDS TERMINAL                        
         BNE   *+8                                                              
         MVI   DDS,C'Y'                                                         
         MVC   TERM,TWATRM                                                      
         MVC   USERID,TWAORIG                                                   
         MVC   AGYALPHA,TWAAGY                                                  
         MVC   AUTHOR,TWAAUTH      SAVE AUTHORITY CODE                          
         MVI   TWANSAVE,0          OUTSMART GENCON - DON'T RESTORE              
*                                                                               
*                                  SET UP CERTAIN ADDRESSES                     
         L     R1,SYSPARMS                                                      
         MVC   CUL(1),0(R1)        SET UP COMPANY/UNIT/LEDGER                   
         L     R2,0(R1)            GET A(TIOB)                                  
         LA    R2,0(R2)            CLEAR HOB                                    
         USING TIOBD,R2                                                         
         ST    R2,ATIOB                                                         
         MVC   MODFRST,TIOBFRST    EXTRACT TIOB VALUES                          
         MVC   MODLAST,TIOBLAST                                                 
         MVC   CURDISP,TIOBCURD                                                 
         ZIC   RE,TIOBAID          GET PF KEY INPUT THIS TIME                   
         LA    RF,12                                                            
         CR    RE,RF                                                            
         BNH   *+6                 ADJUST FOR PF13-PF24                         
         SR    RE,RF                                                            
         STC   RE,PFKEY                                                         
         DROP  R2                                                               
*                                                                               
         L     RF,20(R1)                                                        
         ST    RF,AXTRAI           SET A(EXTRA INFO BLOCK)                      
         MVC   AGYOPTS,0(R1)                                                    
         MVC   AGYCTRY,1(RF)                                                    
         MVC   AGYLANG,3(RF)                                                    
         MVC   AGYCURR,4(RF)                                                    
         CLI   AGYCTRY,0                                                        
         BNE   *+8                                                              
*&&US*&& MVI   AGYCTRY,CTRYUSA                                                  
*&&UK*&& MVI   AGYCTRY,CTRYGBR                                                  
*&&US*&& MVI   AGYLANG,LANGEUS     MICHAEL'S ORDERS-NO LANGUAGE SOFT            
*&&UK                                                                           
         CLI   AGYLANG,0                                                        
         BNE   *+8                                                              
         MVI   AGYLANG,LANGEUK                                                  
*&&                                                                             
*                                                                               
         L     RF,8(R1)                                                         
         USING ACCFACSD,RF                                                      
         ST    RF,AACCFACS         SAVE ADDRESS                                 
         MVC   VACCEMU,AACCEMU     EXTRACT V(ACCEMU)                            
         DROP  RF                                                               
*                                                                               
         LM    R3,R4,12(R1)        A(TIA) A(COMFACS)                            
         ST    R3,ATIA                                                          
         ST    R4,ACOMFACS                                                      
         USING COMFACSD,R4                                                      
         MVC   CALLOV,CCALLOV                                                   
         MVC   DATAMGR,CDATAMGR                                                 
         MVC   GETMSG,CGETMSG                                                   
         MVC   HELLO,CHELLO                                                     
         MVC   SCANNER,CSCANNER                                                 
         MVC   DATCON,CDATCON                                                   
         MVC   PARSNIP,CPARSNIP                                                 
         MVC   GETPROF,CGETPROF                                                 
         MVC   GETFACT,CGETFACT                                                 
*                                                                               
         L     RF,GETFACT                                                       
         GOTO1 (RF),DMCB,0         GET A(PROGRAMS AREA)                         
         L     R1,DMCB                                                          
         USING FACTSD,R1                                                        
         TM    FATFLAG,X'08'       DOES ID HAVE A PASSWORD ?                    
         BZ    SYS0                YES                                          
         MVC   PASSWD,FAPASSWD                                                  
         B     SYS1                                                             
*                                                                               
SYS0     MVI   AUTHOR,X'00'        NO, CLEAR AUTHORITY CODE                     
         DROP  R1                                                               
*                                                                               
SYS1     XC    DMCB,DMCB           OBTAIN CORE-RESIDENT ADDRESSES               
         LA    R2,CORETAB                                                       
         LA    R0,CORES            COUNTER                                      
         LA    R3,COREFACS         POINT TO ADDRESS AREA                        
         L     RF,CALLOV                                                        
         LA    R1,DMCB                                                          
         MVC   DMCB+4(3),=X'D9000A'                                             
*                                                                               
SYS2     MVC   DMCB+7(1),0(R2)                                                  
         CLI   0(R2),0             TEST SPARE                                   
         BE    SYS3                                                             
         GOTO1 (RF),(R1),0                                                      
         MVC   0(4,R3),DMCB        SAVE MODULE ADDRESS                          
SYS3     LA    R2,1(R2)            NEXT MODULE NUMBER                           
         LA    R3,4(R3)            NEXT ADDRESS                                 
         BCT   R0,SYS2                                                          
*                                                                               
*----------------------------------------------------------------------         
* ACCGEN STORAGE RECLAIMED FROM PROSYSD, 5/94                                   
*----------------------------------------------------------------------         
*        SR    R3,R3               SET UP COMMON ENTRIES                        
*        LA    R4,WRICOMM          FOR ACCGEN                                   
*        LA    R5,NWRICOMM                                                      
*        SPACE 1                                                                
*YS4     MVC   0(4,R4),ACCGEN      ALL GO TO ACCGEN                             
*        STC   R3,0(R4)                                                         
*        LA    R3,1(R3)                                                         
*        LA    R4,4(R4)                                                         
*        BCT   R5,SYS4                                                          
*----------------------------------------------------------------------         
*                                                                               
         SR    R3,R3               SET UP COMMON ENTRIES                        
         LA    R4,PROCOMM          FOR PROGEN                                   
         LA    R5,NPROCOMM                                                      
*                                                                               
SYS6     MVC   0(4,R4),PROGEN      ALL GO TO PROGEN                             
         STC   R3,0(R4)                                                         
         LA    R3,1(R3)                                                         
         LA    R4,4(R4)                                                         
         BCT   R5,SYS6                                                          
*                                                                               
SYS8     CLI   TWAMODE,1           TEST IF OFFLINE                              
         BNE   SYS10               NO                                           
*                                                                               
         L     RE,TWADCONS                                                      
         USING TWADCOND,RE                                                      
         MVC   BINSRCH,TBINSRCH    YES-GET EXTRA ADDRESSES                      
         MVC   COVAIL,TCOVAIL                                                   
         DROP  RE                                                               
         EJECT                                                                  
*              OTHER INITIALIZATION                                             
*                                                                               
*                                  SEED SYSD WITH DUMP COMMENTS                 
*                                                                               
SYS10    MVC   DUMPSYSD,=C'**SYSD**'                                            
*        MVC   DUMPAGEN,=C'*ACCGEN*'  REMOVED FROM PROSYSD 5/94                 
         MVC   DUMPASRT,=C'*ASSORT*'                                            
         MVC   DUMPACIO,=C'*ACCIOD*'                                            
         MVC   DUMPPRO,=C'*PROGEN*'                                             
         MVC   DUMPUDAT,=C'*USEFUL*'                                            
         GOTO1 DATCON,DMCB,(5,0),(1,TODAYP)                                     
         DATE  DUB,DATE=NO         GET COUNTRY                                  
         MVC   COUNTRY,DUB+6                                                    
*                                                                               
*                                  SET SYSTEM DEPENDENT VALUES                  
SYS12    L     R1,RELO                                                          
         L     RF,=V(TWABLD)                                                    
         AR    RF,R1                                                            
         ST    RF,VTWABLD                                                       
         L     RF,=V(VATICAN)                                                   
         AR    RF,R1                                                            
         ST    RF,VVATICAN                                                      
         L     RF,=V(DUMMY)        END OF SYSTEM BASE                           
         AR    RF,R1                                                            
         ST    RF,SYSDUMMY                                                      
         L     RF,=V(ACJOBCOL)     V(ACJOBCOL)                                  
         AR    RF,R1                                                            
         ST    RF,VJOBCOL                                                       
         L     RF,=V(PRORATA)      V(PRORATA)                                   
         AR    RF,R1                                                            
         ST    RF,VPRORATA                                                      
         L     RF,=V(SRCHPASS)                                                  
         AR    RF,R1                                                            
         ST    RF,VSRCHPAS                                                      
         L     RF,=V(ACSRCHP)                                                   
         AR    RF,R1                                                            
         ST    RF,VACSRCHP                                                      
         L     RF,=V(JOBAPP)                                                    
         AR    RF,R1                                                            
         ST    RF,VJOBAPP                                                       
         L     RF,=V(ACPROFLT)                                                  
         AR    RF,R1                                                            
         ST    RF,VPROFLT                                                       
         L     RF,=V(ACRAPPER)                                                  
         AR    RF,R1                                                            
         ST    RF,VRAPPER                                                       
         GOTO1 SETADDS                                                          
*                                                                               
SYS15    MVI   SYSTEM,C'A'         ACCOUNT                                      
         MVI   MAXIOS,NIOS         USES 3 I/O AREAS                             
         MVC   SIZEIO,=AL4(LIOS)   EACH I/O IS 1000 BYTES                       
         LA    R1,USER             SET HOOK ROUTINE ADDRESS                     
         ST    R1,GETUSER                                                       
         MVC   LKEY,=H'42'         DETAILS OF DIRECTORY AND KEY                 
         MVC   LSTATUS,=Y(L'ACSTATUS)                                           
         MVC   DATADISP,=Y(ACRECORD-ACKEYD)  ACCOUNT FILE DISP                  
         MVC   SYSDIR,=C'ACCFIL  '                                              
         MVC   SYSFIL,=C'ACCFIL  '                                              
         MVC   REQFILE,=C'ACCREQ '                                              
         OI    GENSTAT1,OKADDEL    OK FOR GENCON TO ADD OVER DEL RECS           
         MVI   USEIO,C'Y'          ONLY ACCPAK DOES THIS                        
         MVI   ACTELOPT,C'N'       NO ACTIVITY ELEMENTS                         
         MVI   GETMSYS,16          USES GETMSG FOR SYSTEM 16                    
         MVC   LWORK,=AL4(LENWORK)    WE TOOK XXXXX BYTES IN NMOD               
         MVC   RCPROG(2),=C'AC'    PREFIX FOR REPORT NO.                        
         MVC   SYSPHASE,=X'D9060B00'    PRESET FOR SYSTEM CALLOVS               
         MVI   FILTIDNO,60         SET PROG REC FILTER FIELD                    
         MVC   SECMASK1,AUTHOR     SET AUTHORIZATION BITS AS MASK               
         LA    R1,BUFF                                                          
         ST    R1,ASTARTSV                                                      
         LH    R1,=Y(OFFBLK-SUBSYSD)                                            
         LA    R1,SUBSYSD(R1)      SET STORAGE ADCON                            
         ST    R1,AOFFBLK                                                       
*                                                                               
*YS20    GOTO1 DATAMGR,DMCB,=C'DTFADD ',=C'ACCOUNT '                            
*        L     RE,12(R1)           RE=A(DTF)                                    
*        USING ISDTF,RE                                                         
*        TM    ISFTYPE,ISFTEMU     TEST EMULATED FILE                           
*        BZ    *+8                                                              
         MVI   EMULATE,C'Y'        YES-SET SWITCH                               
*                                                                               
         LHI   RF,FASAVE-T60BFFD                                                
         LA    RF,T60BFFD(RF)                                                   
         ST    RF,AFASAVE                                                       
*                                                                               
SYSINTX  B     XIT                                                              
*        DROP  RE                                                               
*                                                                               
XIT      XIT1                                                                   
         EJECT                                                                  
* THIS ROUTINE CALLS GENCON BUT DOES NOT RETAIN CONTROL WHEN GENCON             
* EXITS.  INSTEAD, THE ROUTINE THAT CALLS THIS WILL GET CONTROL.                
* THIS ALLOWS THE CONTROLLER TO CALL GENCON AGAIN WITH A NEW RECORD             
* AND ACTION IF, FOR INSTANCE, AN OVERLAY HAD A LIST ACTION AND A               
* SELECTION WAS MADE.                                                           
*                                                                               
CALLGENC NTR1                                                                   
         ST    RD,SYSRD            GENCON USES THIS TO EXIT ITSELF              
*                                                                               
         MVI   GOAGAIN,C'N'        DON'T GO AGAIN UNLESS OVERLAY WANTS          
         GOTO1 GENCON,DMCB,(R8)        TO CALL ANOTHER OVERLAY                  
         B     XIT                                                              
         EJECT                                                                  
* HOOK ROUTINE CALLED BY GENCON AFTER STORAGE HAS BEEN INITIALIZED              
*                                                                               
USER     NTR1  BASE=SYSRB,LABEL=NO                                              
         L     RA,ATWA                                                          
         L     R8,ASPOOLD                                                       
         L     R9,ASYSD                                                         
         L     R7,BASER7                                                        
         ST    RD,COMMRD                                                        
*                                                                               
         TM    FACFLAG,XIROSYS+XIROMODE+XIWRONGF                                
         BZ    USER02                                                           
         OI    GENSTAT4,NODELLST                                                
         OI    GENSTAT5,NOCHGLST                                                
*                                                                               
         CLC   =C'DI',CONACT       CHECK CONNECT IF NOT DISPLAY                 
         BE    USER02                                                           
         CLC   =C'REP',CONACT      REPORT                                       
         BE    USER02                                                           
         CLI   CONACT,C'S'         SELECT                                       
         BE    USER02                                                           
         CLI   CONACT,C'L'         OR LIST                                      
         BE    USER02                                                           
*                                                                               
         LA    R2,CONACTH                                                       
         MVC   MYMSGNO,=AL2(360)                                                
         XC    WORK,WORK                                                        
         TM    FACFLAG,XIROMODE    CONNECTED IN READ ONLY MODE?                 
         BO    USER00              YES                                          
*                                                                               
         MVC   MYMSGNO,=AL2(357)   MUST BE CONNECTED TO WRONG FACPAK            
         MVI   WORK,L'FACUPD                                                    
         MVC   WORK+1(L'FACUPD),FACUPD                                          
         TM    FACFLAG,XIWRONGF    CONNECTED TO WRONG FACPAK?                   
         BO    USER00              YES                                          
*                                                                               
         MVC   MYMSGNO,=AL2(358)   CONNECTED TO READ ONLY SYSTEM                
         XC    WORK,WORK                                                        
*                                                                               
USER00   OI    GENSTAT2,USGETTXT                                                
         MVI   GETMSYS,6           USE ACC MESSAGES                             
         GOTO1 XTRAXIT                                                          
                                                                                
USER02   OC    USERNAME,USERNAME   TEST IF I HAVE BEEN HERE BEFORE              
         BNZ   USER10              YES                                          
*                                                                               
         MVC   FILENAME,=CL8'CTFILE'                                            
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING CTIKEY,R4                                                        
         MVI   CTIKTYP,CTIKTYPQ                                                 
         MVC   CTIKNUM,TWAORIG     READ ID RECORD                               
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 READ                                                             
         GOTO1 HELLO,DMCB,(C'G',FILENAME),('CTORGELQ',AIO),0                    
         CLI   12(R1),0            TEST IF FOUND                                
         BNE   USER04              NO                                           
         L     R4,12(R1)                                                        
         USING CTORGD,R4                                                        
         MVC   USERNAME,CTORGNAM                                                
         MVC   USERADDR,CTORGADD                                                
         DROP  R4                                                               
*                                                                               
USER04   XC    AGYSIGN,AGYSIGN                                                  
         GOTO1 HELLO,DMCB,(C'G',FILENAME),('CTDSCELQ',AIO),0                    
         CLI   12(R1),0            TEST IF FOUND                                
         BNE   USER06              NO                                           
         L     R4,12(R1)                                                        
         USING CTDSCD,R4                                                        
         MVC   AGYSIGN(8),CTDSC                                                 
*                                                                               
* GET PERSONID                                                                  
USER06   OC    PASSWD,PASSWD                                                    
         BZ    USER07                                                           
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING CT0REC,R4                                                        
         MVI   CT0KTYP,CT0KTEQU    RECORD TYPE '0'                              
         MVC   CT0KAGY,TWAAGY      GET SECURITY AGENCY                          
         MVC   CT0KNUM,PASSWD      PERSON AUTH NUMBER                           
*                                                                               
         GOTO1 HIGH                                                             
         CLC   KEY(L'CT0KEY),KEYSAVE                                            
         BNE   USER07                                                           
         GOTO1 HELLO,DMCB,(C'G',FILENAME),(X'C3',AIO),0                         
         CLI   12(R1),0            TEST IF FOUND                                
         BNE   USER07              NO                                           
         L     R4,12(R1)                                                        
         MVC   PERSONID,2(R4)      PERSONAL ID                                  
         MVC   TWAALIAS,PERSONID                                                
         DROP  R4                                                               
*                                                                               
USER07   XC    FILENAME,FILENAME                                                
         GOTO1 SETCOMP                                                          
         L     R1,AOFFBLK                                                       
         USING OFFALD,R1                                                        
         MVC   OFFACOMF,ACOMFACS   INITIALIZE OFFAL BLOCK                       
         MVC   OFFAALPH,TWAAGY                                                  
         MVC   OFFACPY,CUL                                                      
         MVC   OFFACST1(OFFAOPOS-OFFACST1),COMPSTA1                             
         MVC   OFFALIMA,TWAACCS                                                 
         MVC   OFFAAUTH,TWAAUTH                                                 
         CLI   TWAMODE,1           TEST OFFLINE                                 
         BE    USER08              YES                                          
*                                                                               
         MVI   OFFAINDS,OFFAIOFF                                                
         MVI   OFFAACT,OFFAINI     SET TO INITIALIZE                            
         CLC   LASTCOMP,CUL        TEST FOR CHANGE IN COMPANIES                 
         BNE   *+14                YES                                          
         MVI   OFFAACT,OFFARES                                                  
         MVC   OFFASAV(OFFASAVL),SAVEOFFA                                       
         GOTO1 OFFAL                                                            
         BE    USER10                                                           
         DC    H'0'                                                             
*                                                                               
USER08   OI    OFFAINDS,OFFAIOFF+OFFAIOFS NOTE OFFLINE UNDER SPOOF              
         MVI   OFFAACT,OFFAINI                                                  
         GOTO1 OFFAL                                                            
         BE    USER10                                                           
         DC    H'0'                                                             
*                                                                               
USER10   CLI   RACHANGE,C'Y'       TEST IF RECORD/ACTION CHANGED                
         BNE   *+8                                                              
         MVI   CALLSP,0            YES-CLEAR STACK POINTER                      
*                                                                               
         CLI   CONWHENH+5,0        TEST ANY INPUT IN WHEN                       
         BE    USER16              NO                                           
*                                                                               
         ZIC   R1,CONACTH+5        GET ACTION FIELD LENGTH                      
         ZIC   RE,CONRECH+5        GET RECORD FIELD LENGTH                      
         LA    R0,3                                                             
         CR    R1,R0                                                            
         BL    *+6                                                              
         LR    R1,R0               MAXIMUM COMPARE DONE BY GENCON IS 3          
         CR    RE,R0                                                            
         BL    *+6                                                              
         LR    RE,R0                                                            
*                                                                               
         BCTR  R1,0                                                             
         EX    R1,REPCOMP          TEST FOR REPORT ACTION                       
         BE    USER16              YES-LEAVE WHEN FIELD ALONE                   
         EX    R1,UPDCOMP                                                       
         BE    USER16                                                           
         CLI   CONACTH+5,2         TEST AT LEAST 2 CHARACTERS INPUT             
         BL    USER12              YES-SKIP TEST                                
         EX    R1,SUMCOMP          TEST ACTION=SUMMARY                          
         BE    USER16                                                           
         CLI   CONACTH+5,3         TEST AT LEAST 3 CHARACTERS                   
         BL    *+12                                                             
         EX    R1,CONCOMP          TEST ACTION=CONTROL                          
         BE    USER16                                                           
*                                                                               
USER12   BCTR  RE,0                                                             
         EX    RE,ESTCOMP          TEST IF RECORD=ESTIMATE                      
         BNE   USER14              NO                                           
         EX    R1,LISCOMP          TEST FOR EST/LIST                            
         BE    USER16              YES-LEAVE WHEN ALONE                         
         EX    R1,SELCOMP          TEST FOR SELECT                              
         BE    USER16              YES                                          
*                                                                               
USER14   XC    CONWHEN,CONWHEN     CLEAR UNNECESSARY WHEN FIELD                 
         MVI   CONWHENH+5,0                                                     
         OI    CONWHENH+6,X'80'    XMIT BACK                                    
*                                                                               
USER16   CLC   =C'CLOSE',CONACT    MAKE USER TYPE IN ALL OF 'CLOSE'             
         BE    USER18                                                           
         LA    R2,CONACTH                                                       
         CLI   5(R2),2                                                          
         BL    USER18                                                           
         CLI   5(R2),4                                                          
         BH    USER18                                                           
         ZIC   R1,5(R2)                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8              TEST FOR CL(OS)                              
         B     *+10                                                             
         CLC   CONACT(0),=C'CLOSE'                                              
         BNE   USER18                                                           
         MVI   ERROR,INVACT                                                     
         MVI   GCMODE,C' '                                                      
         GOTO1 VERRCUR                                                          
*                                                                               
USER18   CLI   PFKEY,PF12          TEST FOR PF12                                
         BNE   USERX               NO                                           
         CLI   CALLSP,0            TEST ANY THING ON STACK                      
         BE    USERX               NO                                           
*                                                                               
         LA    R0,RETURNS          R0=COUNTER                                   
         LA    RE,RETTAB                                                        
         CLC   TWASCR,0(RE)        TEST SCREEN AGAISNT RETURN LIST              
         BE    USER20              RETURN IS SUPPORTED                          
         LA    RE,L'RETTAB(RE)                                                  
         BCT   R0,*-14                                                          
         B     USERX               CANNOT RETURN FROM THIS SCREEN               
*                                                                               
USER20   MVI   PFKEY,0             CLEAR OUT PF12 SETTING                       
         GOTO1 VRETURN                                                          
*                                                                               
USERX    B     XIT                                                              
         DROP  R1                                                               
*                                                                               
REPCOMP  CLC   CONACT(0),=CL8'REPORT'                                           
LISCOMP  CLC   CONACT(0),=CL8'LIST'                                             
UPDCOMP  CLC   CONACT(0),=CL8'UPDATE'                                           
CONCOMP  CLC   CONACT(0),=CL8'CONTROL'                                          
SUMCOMP  CLC   CONACT(0),=CL8'SUMMARY'                                          
SELCOMP  CLC   CONACT(0),=CL8'SELECT'                                           
MNTCOMP  CLC   CONACT(0),=CL8'MAINT   '                                         
ESTCOMP  CLC   CONREC(0),=CL8'ESTIMATE'                                         
FALCOMP  CLC   CONREC(0),=CL8'FALINK  '                                         
*                                                                               
* LIST OF SCREENS FROM WHICH RETURNS ARE ALLOWED                                
*                                                                               
RETTAB   DS    0XL1                                                             
         DC    X'B1'               CATEGORY MAINT                               
         DC    X'C0'               SCHEME MAINT                                 
         DC    X'C1'               CATEGORY DETAIL                              
         DC    X'C2'               JOB ESTIMATE                                 
         DC    X'C3'               JOB ELIST                                    
         DC    X'C7'               JOB DETAIL                                   
         DC    X'C4'               PANEL MAINT                                  
         DC    X'C6'               FIELD MAINT                                  
         DC    X'C8'               TEXT MAINT                                   
         DC    X'CA'               SESSION ESTIMATING                           
         DC    X'CF'               SCHEME COPY                                  
         DC    X'DA'               XCLIENT MAINT                                
         DC    X'DB'               XPRODUCT MAINT                               
         DC    X'DC'               XJOB MAINT                                   
         DC    X'B8'               TEXT LIST                                    
         DC    X'FA'               JOB NUMBER                                   
         DC    X'F1'               CLIENT MAINT                                 
         DC    X'F2'               PRODUCT MAINT                                
         DC    X'F3'               JOB MAINT                                    
         DC    X'F4'               OPTION MAINT                                 
         DC    X'E4'               OPTION LIST                                  
         DC    X'EE'               WORKCODE LIST                                
         DC    X'B4'               PANEL LIST                                   
         DC    X'B5'               LINK MAINT                                   
         DC    X'B6'               FUND MAINT                                   
         DC    X'B7'               JOB SUMMARY                                  
         DC    X'AC'               ADVERTISER MAINT                             
         DC    X'AD'               ADVERTISER LIST                              
         DC    X'AE'               ACCOUNT GROUP MAINT                          
         DC    X'AF'               ACCOUNT GROUP LIST                           
         DC    X'BB'               AUTHORIZATION HISTORY                        
         DC    X'BE'               AUTHORIZATION MAINT                          
         DC    X'BF'               AUTHORIZATION LIST                           
         DC    X'B6'               FUND MAINT                                   
RETURNS  EQU   (*-RETTAB)/L'RETTAB                                              
         EJECT                                                                  
*              CONSTANTS TABLES ETC                                             
*                                                                               
RELO     DS    A                                                                
LOWPERS  DC    X'D78599A29695'     C'PERSON' IN LOWER CASE                      
*                                                                               
CORETAB  DS    0C                  TABLE OF CORE-RESIDENT MODULES               
         DC    AL1(QGENCON)                                                     
         DC    AL1(QDRONE)                                                      
         DC    AL1(0)              FORMERLY QACCIO - NOW SPARE                  
         DC    AL1(QACCGEN)                                                     
         DC    AL1(QPROGEN)                                                     
         DC    AL1(QTASYSES)                                                    
         DC    AL1(QGETOPT)                                                     
         DC    AL1(QJOBBER)                                                     
         DC    AL1(QOFFAL)                                                      
*                                                                               
CORES    EQU   (*-CORETAB)                                                      
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
       ++INCLUDE ACPROWORKD                                                     
         ORG   T60BFFD+(11*1024)                                                
FASAVE   DS    (4*1024)X                                                        
         ORG   T60BFFD+(10*1024)   ORG TO BOTTOM 1K OF TWA                      
SECBLK   DS    1024X               ACCESS BLOCK                                 
SECBLKL  EQU   *-SECBLK                                                         
*                                                                               
CMPALPHA DS    CL2                 COMPANY ALPHA ID                             
*              OTHER DSECTS ARE HIDDEN IN HERE                                  
*                                                                               
*DDSPOOLD                                                                       
         PRINT OFF                                                              
       ++INCLUDE DDSPOOLD                                                       
         PRINT ON                                                               
*DDSPLWORKD                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDSPLWORKD                                                     
         PRINT ON                                                               
*ACGENFILE                                                                      
         PRINT OFF                                                              
       ++INCLUDE ACGENFILE                                                      
         PRINT ON                                                               
*ACGENBOTH                                                                      
         PRINT OFF                                                              
       ++INCLUDE ACGENBOTH                                                      
         PRINT ON                                                               
*CTGENFILE                                                                      
         PRINT OFF                                                              
       ++INCLUDE CTGENFILE                                                      
         PRINT ON                                                               
*FAFACTS                                                                        
         PRINT OFF                                                              
       ++INCLUDE FAFACTS                                                        
         PRINT ON                                                               
*FATIOB                                                                         
         PRINT OFF                                                              
       ++INCLUDE FATIOB                                                         
         PRINT ON                                                               
*DDACCFACS                                                                      
         PRINT OFF                                                              
       ++INCLUDE DDACCFACS                                                      
         PRINT ON                                                               
*DDCOMFACS                                                                      
         PRINT OFF                                                              
       ++INCLUDE DDCOMFACS                                                      
         PRINT ON                                                               
*DDCOREQUS                                                                      
         PRINT OFF                                                              
       ++INCLUDE DDCOREQUS                                                      
         PRINT ON                                                               
*DDCTRYEQUS                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDCTRYEQUS                                                     
         PRINT ON                                                               
*DDGLOBEQUS                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDGLOBEQUS                                                     
         PRINT ON                                                               
*DDLANGEQUS                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDLANGEQUS                                                     
         PRINT ON                                                               
*DDMASTD                                                                        
         PRINT OFF                                                              
       ++INCLUDE DDMASTD                                                        
         PRINT ON                                                               
*DDTWADCOND                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDTWADCOND                                                     
         PRINT ON                                                               
*FAXTRAINF                                                                      
         PRINT OFF                                                              
       ++INCLUDE FAXTRAINF                                                      
         PRINT ON                                                               
*SEACSFILE                                                                      
        PRINT OFF                                                               
       ++INCLUDE SEACSFILE                                                      
        PRINT ON                                                                
GOBLOCKD DSECT                                                                  
       ++INCLUDE ACGOBLOCK                                                      
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'035ACPRO00   07/10/20'                                      
         END                                                                    
