*          DATA SET RERES00    AT LEVEL 020 AS OF 04/15/09                      
*PHASE T81900C                                                                  
*INCLUDE GETKSRC                                                                
*INCLUDE KHDUMMY                                                                
         TITLE 'T81900 - RERES00 - REPPAK RESEARCH CONTROLLER'                  
*                                                                               
**********************************************************************          
*                                                                    *          
*  RERES00 - T81900 -- RESEARCH CONTROLLER                           *          
*                                                                    *          
* ------------------------------------------------------------------ *          
*                                                                    *          
*  !!!***!!! NOTE: IN THE RECORD ACTION TABLE, THERE ARE X'04'       *          
*            ENTRIES FOR SOME OF THE "RECORDS" INSTEAD OF X'01'      *          
*            THIS ENABLES "DISPLAY","CHANGE","LIST" ACTIONS TO BE    *          
*            USED WITHOUT ASSOCIATED REC/ACTION ENTRIES.             *          
*            FOR EXAMPLE, A USER CAN 'ADD' A 'MASTER' REPORT         *          
*            WHAT ACTUALLY HAPPENS IS, GENCON CALLS THE REPORT       *          
*            OVERLAY WITH VALREC TO ENSURE ALL FIELDS ARE VALID      *          
*            THEN, BASED ON THE "HELP ID" NUMBER GIVEN TO THE SCREEN *          
*            FIELDS, GENCON SAVES OFF THE USER ENTRIES IN A RECORD ON*          
*            THE CONTROL FILE.  WHEN YOU "LIST" MASTER, IT ACTUALLY  *          
*            LISTS THESE CONTROL FILE RECORDS, WHEN YOU "DISPLAY",   *          
*            GENCON RETRIEVES THE CONTROL FILE RECORD, LOADS THE     *          
*            SCREEN (IN THIS CASE, THE MASTER REPORT SCREEN),AND     *          
*            POPULATES THE FIELDS WITH THE DATA SAVED IN THE CONTROL *          
*            FILE RECORD.  APPARENTLY, THIS IS HANDLED BY THE        *          
*            GEGENPRG MODULE.                                        *          
*               GENCON REFERS TO THESE CTFILE  RECS AS               *          
*                          "PROGRAM RECORDS".                        *          
*            WHAT DOES THIS MEAN? AS THERE IS NO CLEAR DOCUMENTATION *          
*            I DON'T EXACTLY KNOW.                                   *          
*                                                                    *          
*  MOD LOG                                                           *          
*  -------                                                           *          
*                                                                    *          
*  08/29/89  PJS  SET 'USEKEYSV' BIT FOR GENCON. (NEED FOR INTEREP)  *          
*                                                                    *          
*  JAN09/90 (MRR) --- MAKE ALL REPORTS SOON'ABLE, EACH REPORT/OV HAS *          
*                      THE RULES FOR WHEN WE FORCE IT TO SOON.       *          
*                                                                    *          
*  APR09/90 (MRR) --- ADD FIXPAV ROUTINE THAT FIXES A PAV RECORD     *          
*                      LETTING IUN NORMALIZE IT TO REP RESEARCH RECS *          
*                                                                    *          
*  JUN06/90 (ZEN) --- ADD SHARE LOOKUPS TO FIXPAV                    *          
*                                                                    *          
*  SEP25/90 (MRR) --- >ALL SCREENS CHANGED                           *          
*                     >RENAME FIXPAV TO IUNDEM                       *          
*                                                                    *          
*  OCT01/90 (MRR) --- >SET ADDR OF LINK REGETIUN                     *          
*                                                                    *          
*  OCT04/90 (MRR) --- >SET ADDR OF OFORMAT FOR REGETIUN CALL         *          
*                                                                    *          
*  OCT11/90 (MRR) --- >MODIFY DEMO VALUE VAL ROUTINE FOR 1 DECIMAL   *          
*                                                                    *          
*  NOV13/90 (MRR) --- >SET NTWA                                      *          
*                                                                    *          
*  JUL22/96 (GL ) --- >SUPPORT MEDIAFAX                              *          
*                                                                    *          
*  JAN01/01 (FJD) --- >"FIX" GLOBBER LOGIC                           *          
*                                                                    *          
*  JUN15/01 (BU ) --- >LOCK OUT 'NOW' FOR GEN AVAIL REPORT           *          
*                                                                    *          
*  MAR12/03 (BU ) --- >GLOBBER LOGIC CHANGE                          *          
*                                                                    *          
*  SEP09/03 (BU ) --- >TITLES REPORT: NO 'NOW'                       *          
*                                                                    *          
**********************************************************************          
*                                                                               
T81900   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 LENWORK,T81900**,RA,RR=R2,CLEAR=YES                              
         USING GEND,RC                                                          
         USING SPOOLD,R8                                                        
         USING SYSD,R9             SYSTEM SPECIFIC WORK                         
         ST    R2,RELO                                                          
         LR    R8,RC               R8=A(SPOOLD)                                 
         LA    RC,SPOOLEND         RC=A(GENCON STORAGE)                         
         LA    R9,IO               START OF IO 1 + TOTAL IO AREA LEN            
         AH    R9,=Y(IOLEN)        R9=A(RESRCH SYSTEM WORKNG STORAGE)           
         LA    RF,OFORMAT                                                       
         ST    RF,AOFORMAT                                                      
         ST    R1,SYSPARMS         THIS CODE FOR CURSOR POSITIONING             
         L     R0,0(R1)                                                         
         ST    R0,ATIOB                                                         
         L     R7,4(R1)                                                         
         ST    R7,ATWA             R7=A(TWA)                                    
         USING CONHEADH-64,R7                                                   
         MVI   SPACES,C' '                                                      
         MVC   SPACES+1(131),SPACES                                             
         ST    RB,SYSRB            SYSTEM BASE REGS                             
         ST    RA,SYSRA                                                         
                                                                                
*                                                                               
*   TO PERMIT RETURN VIA GLOBBER                                                
*                                                                               
         XC    CONSERV,CONSERV     CLEAR SRV REQUEST FIELD                      
         OI    CONSERVH+6,X'81'    MARK SRV REQUEST AS MODIFIED                 
*                                  FOR NEXT INPUT                               
                                                                                
         BAS   RE,SYSINIT          INITIALIZE SYSTEM DEPENDENT                  
         BAS   RE,SETRD            SET RD SO GENCON WILL RETURN                 
*                                                                               
*        DETERMINE RECACT TABLE FOR AGENCY                                      
         LA    R1,AGYLIST          POINT TO AGENCY LIST                         
*                                                                               
AGYLP    DS    0H                                                               
*                                                                               
         CLI   0(R1),X'FF'         DONE IF END OF TABLE REACHED                 
         BE    AGYDN                                                            
         CLC   CPAREP,0(R1)        IF AGENCY IN LIST                            
         BE    AGYFD                                                            
*                                                                               
AGYCN    DS    0H                                                               
         LA    R1,L'CPAREP(R1)                                                  
         B     AGYLP                                                            
*                                                                               
AGYDN    DS    0H                                                               
*                                                                               
         L     RF,=A(RECACT1)          SWITCH RECORD        TABLES              
         A     RF,RELO                                                          
         ST    RF,ARECACT1                                                      
         L     RF,=A(RECACT3)          SWITCH RECORD/ACTION TABLES              
         A     RF,RELO                                                          
         ST    RF,ARECACT3                                                      
*                                                                               
AGYFD    DS    0H                                                               
*                                                                               
         B     AGYX                                                             
*                                                                               
AGYLIST  DS    0H                  LIST OF AGENCIES USING OLD SYSTEM            
****     DC    C'UV'                                                            
****     DC    C'B1'                                                            
         DC    X'FFFF'             END OF TABLE                                 
*                                                                               
AGYX     DS    0H                                                               
         L     RF,SYSPARMS                                                      
         L     RF,0(RF)            A(TIOB)                                      
         USING TIOBD,RF                                                         
         CLI   TIOBAID,12          LOOK FOR PFKEY 12 HIT                        
         BNE   SFMPF12X            SKIP SWAP IF NOT FOUND                       
*                                                                               
*   ??FOLLOWING COMMENTED OUT UNTIL CALLING SYS/PROG CAN BE                     
*   ??  PROPERLY SAVED/RETRIEVED IN TWA                                         
*                                                                               
         LA    RF,CONHEADH+3520                                                 
         USING AFRSTKEY,RF                                                      
*                                                                               
         OC    TWAGLCS,TWAGLCS      IF NO CALLING SYSTEM,                       
         BZ    PF12X                THEN EXIT                                   
         OC    TWAGLCP,TWAGLCP      IF NO CALLING PROGRAM,                      
         BZ    PF12X                THEN EXIT                                   
*                                                                               
         DROP  RF                                                               
*                                                                               
*        SR    RF,RF                                                            
*        LA    RE,CONHEADH         POINT TO FIRST FLD ON SCREEN                 
*        IC    RF,CONHEADH         LENGTH OF 1ST FIELD ON SCREEN                
*        LA    RE,0(RF,RE)         POINT TO SERVICE REQUEST FIELD               
*        MVC   8(17,RE),=CL17'=SW ' SET FOR RETURN TO MATCH                     
                                                                                
                                                                                
         XC    BLOCK(256),BLOCK     CLEAR FOR GLOBBER ELEMENT BUILD             
         LA    R3,BLOCK                                                         
         USING GLVXFRSY,R3                                                      
         MVC   GLVXFRSY,=C'REP'     FROM THE REP SYSTEM                         
         MVC   GLVXFRPR,=C'RSC'     RSC  PROGRAM                                
*                                                                               
         LA    RF,CONHEADH+3520                                                 
         USING AFRSTKEY,RF                                                      
*                                                                               
         MVC   GLVXTOSY,TWAGLCS     LAST CALLER SYSTEM                          
         MVC   GLVXTOPR,TWAGLCP     LAST CALLER PROG                            
*                                                                               
         DROP  RF                                                               
*          FOLLOWING TEMPORARILY HARD CODED                                     
**       MVC   GLVXTOSY,=C'REP'     LAST CALLER SYSTEM                          
**       MVC   GLVXTOPR,=C'RMP'     LAST CALLER PROG                            
         OI    GLVXFLG1,GLV1RETN+GLV1SEPS                                       
         DROP  R3                                                               
                                                                                
         L     RF,VGLOBBER                                                      
         GOTO1 (RF),DMCB,=C'PUTD',BLOCK,24,GLVXCTL                              
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
                                                                                
PF12X    B     EXIT                RETURN WITH SWAP                             
*                                                                               
SFMPF12X DS    0H                                                               
*                                  CHECK IF THIS IS A GLOBBER CALL              
         MVI   TRANSSW,0           INIT TRANSFER SWITCH                         
         OC    VGLOBBER,VGLOBBER   SKIP IF NO GLOBBER ADDRESS                   
         BZ    SFMGLBX                                                          
         XC    BLOCK(256),BLOCK    CLEAR FOR CONTROL ELEMENT RETRIEVAL          
*                                                                               
* CHECK FOR GLOBBER CONTROL ELEMENT                                             
*                                                                               
         GOTO1 VGLOBBER,DMCB,=C'GETD',BLOCK,24,GLVXCTL                          
         TM    DMCB+8,X'10'        SKIP IF NO ELM FOUND                         
         BNZ   SFMGLBX                                                          
         CLI   DMCB+8,0            NO OTHER ERRORS TOLERATED                    
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 (RF),DMCB,=C'DELE',,,GLVXCTL                                     
*                           DELETE CONTROL ELEMENT                              
*                                                                               
* SAVE OFF RELEVANT CONTROL ELEMENT INFO                                        
*                                                                               
         LA    R3,BLOCK                                                         
         USING GLVXFRSY,R3                                                      
*                                                                               
         LA    RF,CONHEADH+3520                                                 
         USING AFRSTKEY,RF                                                      
*                                                                               
         MVC   TWAGLCS,GLVXFRSY   SAVE CALLING SYSTEM                           
         MVC   TWAGLCP,GLVXFRPR   SAVE PROGRAM                                  
*                                                                               
         DROP  R3,RF                                                            
                                                                                
**       GOTO1 VGLOBBER,DMCB,=C'GETD',WORK,24,GLVXCTL  GET XCTL ELM             
**       CLI   DMCB+8,GLEGNF       SKIP IF NO ELM FOUND                         
**       BE    SFMGLBX                                                          
**       CLI   DMCB+8,0            NO OTHER ERRORS TOLERATED                    
**       BE    *+6                                                              
**       DC    H'0'                                                             
**       LA    R1,WORK             ESTABLISH XCTL ELM                           
**       USING GLVXFRSY,R1                                                      
**       CLC   =C'REP',GLVXTOSY    MAKE SURE REP/RSC WANTED                     
**       BNE   SFMGLBX                                                          
**       CLC   =C'RSC',GLVXTOPR                                                 
**       BNE   SFMGLBX                                                          
**       DROP  R1                                                               
**                                                                              
**       GOTO1 (RF),(R1),=C'DELE'  DELETE TRANSFER ELM                          
         MVI   TRANSSW,C'Y'        INDICATE WE ARE IN MIDST OF TRANSFER         
         L     R2,SYSPARMS                                                      
         L     R7,4(R2)            ESTABLISH SCREEN                             
         USING CONHEADH-64,R7                                                   
*                                                                               
         GOTO1 VGLOBBER,DMCB,=C'GETF',CONRECH,,GLVXREC  GET RECORD              
         GOTO1 (RF),(R1),=C'DELE'  DELETE RECORD                                
*                                                                               
         GOTO1 VGLOBBER,DMCB,=C'GETF',CONACTH,,GLVXACT  GET ACTION              
         GOTO1 (RF),(R1),=C'DELE'  DELETE ACTION                                
*                                                                               
         GOTO1 VGLOBBER,DMCB,=C'GETF',CONKEYH,,GLRKEY   GET KEY                 
         GOTO1 (RF),(R1),=C'DELE'  DELETE KEY                                   
*                                                                               
         GOTO1 VGLOBBER,DMCB,=C'GETF',CONWHENH,,GLRWHEN GET WHEN FIELD          
         GOTO1 (RF),(R1),=C'DELE'  DELETE WHEN                                  
*                                                                               
         GOTO1 VGLOBBER,DMCB,=C'GETF',CONKEYH,,GLVPRKEY GET KEY                 
         GOTO1 (RF),(R1),=C'DELE'  DELETE KEY                                   
*                                                                               
SFMGLBX  DS    0H                                                               
*                                                                               
         XC    DMCB,DMCB           OBTAIN CORE-RESIDENT ADDRESSES               
         XC    DMCB,DMCB           OBTAIN CORE-RESIDENT ADDRESSES               
         LA    R2,CORETAB                                                       
         LA    R0,CORES            COUNTER                                      
         LA    R3,COREFACS         POINT TO ADDRESS AREA                        
         L     RF,CALLOV                                                        
         LA    R1,DMCB                                                          
         MVC   DMCB+4(3),=X'D9000A'                                             
RES10    MVC   DMCB+7(1),0(R2)                                                  
         GOTO1 (RF),(R1),0                                                      
         MVC   0(4,R3),DMCB        SAVE MODULE ADDRESS                          
         LA    R2,1(R2)            NEXT MODULE NUMBER                           
         LA    R3,4(R3)            NEXT ADDRESS                                 
         BCT   R0,RES10                                                         
         MVC   VGETIUN,VRGETIUN    COPY INTO OLD LOCATION                       
*                                                                               
         BAS   RE,CKGLOB           CHECK GLOBBER CALL                           
*                                                                               
         SPACE 2                                                                
* CALL A ROUTINE THAT CALLS GENCON SO THAT GENCON WILL RETURN TO                
* NEXT INSTRUCTION AFTER THIS                                                   
* DON'T GO AGAIN UNLESS OVERLAY WANTS TO CALL ANOTHER OVERLAY                   
         SPACE 1                                                                
AGAIN    MVI   GOAGAIN,C'N'                                                     
         BAS   RE,CALLGENC                                                      
         SPACE 1                                                                
* IF OVERLAY WISHED TO CALL GENCON WITH A NEW RECORD AND ACTION,                
* THEN THIS FLAG WILL BE SET                                                    
         SPACE 1                                                                
         CLI   GOAGAIN,C'Y'                                                     
         BNE   DONE                                                             
         NI    CONRECH+6,X'BF'     RESET INSERT CURSOR BIT HERE                 
         B     AGAIN                                                            
         SPACE 1                                                                
DONE     OI    CONRECH+4,X'20'                                                  
         OI    CONACTH+4,X'20'                                                  
*                                                                               
EXIT     XIT1                                                                   
         SPACE 2                                                                
SETRD    NTR1 LABEL=*                                                           
         ST    RD,SYSRD            SET RD SO GENCON WILL RETURN CONTROL         
         B     EXIT                                                             
         EJECT                                                                  
*THIS ROUTINE CALL GENCON BUT DOES NOT RETAIN CONTROL WHEN GENCON               
* EXITS.  INSTEAD, THE ROUTINE THAT CALLS THIS WILL GET CONTROL.                
* THIS ALLOWS THE CONTROLLER TO CALL GENCON AGAIN WITH A NEW                    
* RECORD AND ACTION IF, FOR INSTANCE, AN OVERLAY HAD A LIST ACTION              
* AND A SELECTION WAS MADE.                                                     
         SPACE 2                                                                
CALLGENC NTR1 LABEL=*                                                           
         ST    RD,SYSRD            GENCON USES THIS TO EXIT ITSELF              
         SPACE 1                                                                
         GOTO1 GENCON,DMCB,(R8)    CALL GENCON-PASS A(WORKING STORAGE)          
         B     XIT                 THEN WE'RE THROUGH                           
         EJECT                                                                  
* INITIALIZE SYSTEM DEPENDENT VALUES                                            
         SPACE 1                                                                
SYSINIT  NTR1 LABEL=*                                                           
*                                  GET TERMINAL VALUES                          
         MVI   DDS,NO                                                           
         CLI   TWAOFFC,C'*'        TEST FOR DDS TERMINAL                        
         BNE   *+8                                                              
         MVI   DDS,YES                                                          
         MVC   TERM,TWATRM                                                      
         MVC   AUTH,TWAAUTH                                                     
         MVC   USERID,TWAORIG                                                   
         MVC   AGENCY,TWAAGY                                                    
         MVC   XTRA,SPACES                                                      
*                                                                               
         LA    RF,SAVSTART         SET A(SAVEAREA)                              
         ST    RF,ASTARTSV                                                      
*                                                                               
         LH    RE,=Y(SAVEND-SYSD)  END OF SAVEAREA                              
         LA    RE,SYSD(RE)                                                      
         SR    RE,RF               AMOUNT TO SAVE                               
         STH   RE,LSVTWA0          PASS TO GENCON                               
*                                                                               
SYS1     LA    R2,SYSV                                                          
         LA    R3,SYSVCON                                                       
         LA    R4,NVTYPES                                                       
*                                                                               
SYS2     L     R1,0(R3)            RELOCATE SYSTEM VTYPES                       
         A     R1,RELO                                                          
         ST    R1,0(R2)                                                         
         LA    R2,4(R2)                                                         
         LA    R3,4(R3)                                                         
         BCT   R4,SYS2                                                          
*                                                                               
         LA    R2,VCOMMON          SET UP COMMON ENTRIES                        
         SR    R3,R3                                                            
         LA    R4,SYSCOMM                                                       
         LA    R5,VCOUNT                                                        
*                                                                               
SYS4     ST    R2,0(R4)                                                         
         STC   R3,0(R4)                                                         
         LA    R3,4(R3)                                                         
         LA    R4,4(R4)                                                         
         BCT   R5,SYS4                                                          
*                                                                               
SYS6     LA    R2,ADTAB            SET ADCONS FOR EXTENDED ADDRESSING           
         LA    R0,NADCONS          COUNTER                                      
         LA    R3,EXTADS           POINT TO FIRST EXTENDED ADCON                
********************************************************************            
* THIS MUST BE NO-OPED IF THERE ARE NO ADCONS                      *            
********************************************************************            
*YS7     ICM   R1,7,0(R2)          GET DISPLACEMENT                *            
*        LA    R1,SYSD(R1)         INDEX INTO STORAGE              *            
*        ST    R1,0(R3)                                            *            
*        LA    R2,L'ADTAB(R2)      NEXT TABLE ENTRY                *            
*        LA    R3,4(R3)            NEXT WORKING STORAGE FIELD      *            
*        BCT   R0,SYS7                                             *            
********************************************************************            
         SPACE 1                                                                
* SET SYSTEM DEPENDENT VALUES                                                   
*                                                                               
SYS10    MVI   SYSTEM,C'R'         REP                                          
         MVI   GETMSYS,8           REP MESSAGES                                 
*                                                                               
         MVI   MAXIOS,NIOS         USES 2 I/O AREAS                             
         MVC   SIZEIO,=AL4(LIOS)   EACH I/O IS 1000 BYTES                       
         MVC   SYSDUMMY,VDUMMY     END OF SYSTEM BASE                           
         MVC   GETUSER,VGETREP     ROUTINE TO GET REP NAME AND ADDRESS          
*                                                                               
         MVC   LKEY,=H'27'          DETAILS OF DIRECTORY AND KEY                
         MVC   LSTATUS,=H'1'                                                    
         MVC   DATADISP,=H'34'                                                  
         MVC   SYSFIL,=C'REPFIL  '                                              
         MVC   SYSDIR,=C'REPDIR  '                                              
         MVC   REQFILE,=C'REPREQ  '                                             
*                                                                               
         MVI   NTWA,X'81'               SAVE FULL 6K                            
         MVC   LWORK,=Y(LENWORK)        SET WORK AREA LENGTH                    
         MVC   RCPROG(2),=C'RE'         PREFIX FOR REPORT NO.                   
         MVC   SYSPHASE,=X'D9081900'    PRESET FOR SYSTEM CALLOVS               
         L     R1,=A(RECACTS)           RECORD/ACTION DIRECTORY                 
         A     R1,RELO                                                          
         ST    R1,ARECACT                                                       
*                                                                               
         OI    GENSTAT3,USEKEYSV   FOR INTEREP.                                 
*                                                                               
         OI    GENSTAT2,DISTHSPG   KEEP LIST ON CURRENT PAGE AFTER SEL          
*                                                                               
*                                                                               
*        SET UP CERTAIN ROUTINE ADDRESSES-  (CANT WAIT FOR GENCON)              
*                                                                               
SYS12    L     R1,SYSPARMS                                                      
         LM    R2,R4,8(R1)         A(SYSLIST) A(TIA) A(COMFACS)                 
         ST    R4,ACOMFACS                                                      
         USING COMFACSD,R4                                                      
         ST    R3,ATIA                                                          
*                                                                               
         L     RF,=V(GETKSRC)                                                   
         A     RF,RELO                                                          
         ST    RF,VGETKSRC                                                      
*                                                                               
         MVC   CALLOV,CCALLOV                                                   
         MVC   DATAMGR,CDATAMGR                                                 
         MVC   GETMSG,CGETMSG                                                   
         MVC   HELLO,CHELLO                                                     
         MVC   SCANNER,CSCANNER                                                 
         MVC   DEMOUT,CDEMOUT                                                   
         MVC   DEMOVAL,CDEMOVAL                                                 
         MVC   DEMAND,CDEMAND                                                   
         MVC   DEMAINT,CDEMAINT                                                 
         MVC   VGLOBBER,CGLOBBER                                                
*                                                                               
********************************************************************            
*                                                                  *            
*     GET PARENT REP FROM REP RECORD                               *            
*                                                                  *            
********************************************************************            
         SPACE 2                                                                
PAREP    DS    0H                                                               
         MVC   CPAREP,TWAAGY       INIT PARENT REP AREA                         
         XC    KEY,KEY             GET PARENT REP FROM REP RECORD               
         LA    R4,KEY                                                           
         USING RREPKEY,R4                                                       
         MVI   RREPKTYP,X'01'                                                   
         MVC   RREPKREP,TWAAGY                                                  
         MVC   KEYSAVE,KEY         SAVE KEY                                     
         DROP  R4                                                               
*                                                                               
         GOTO1 DATAMGR,DMCB,(0,=CL8'DMREAD'),(0,=CL8'REPDIR'),         X        
               KEYSAVE,KEY                                                      
         CLI   DMCB+8,0            SKIP IF THERE ARE ERRORS                     
         BNE   PAREPX                                                           
         GOTO1 DATAMGR,DMCB,(0,=CL8'GETREC'),(0,=CL8'REPFIL'),         X        
               KEY+28,IO,DMWORK                                                 
         CLI   DMCB+8,0            SKIP IF THERE ARE ERRORS                     
         BNE   PARMPX                                                           
         MVI   ELCODE,X'01'        FIND HEADER ELEMENT                          
         LA    R4,IO                                                            
         BAS   RE,GETEL                                                         
         BNE   PAREPX              ELEMENT NOT FOUND                            
         USING RREPELEM,R6                                                      
         MVC   CPAREP,RREPPAR       SAVE PARENT REP CODE                        
         DROP  R6                                                               
*                                                                               
PAREPX   DS    0H                                                               
         MVI   ELCODE,X'04'        FIND PROFILE BIT ELEMENT                     
         LA    R4,IO                                                            
         BAS   RE,GETEL                                                         
         BNE   PARMPX                                                           
         USING RREPPGMP,R6                                                      
         ZIC   R0,RREPPGM#         NUMBER OF PROGRAM UNITS                      
         LA    RE,RREPPGM1         A(1ST UNIT)                                  
*                                                                               
PARMP1   CLI   0(RE),RREPQRMP      FIND THE RMP/RSR ONE                         
         BE    PARMP2                                                           
         LA    RE,RREPPGML(RE)                                                  
         BCT   R0,PARMP1                                                        
         B     PARMPX                                                           
PARMP2   MVC   RMPPROF(8),2(RE)                                                 
         B     *+8                                                              
         OI    RMPPROF+RMPIMPSB,RMPIMPSA                                        
         TM    RMPPROF+RMPIMPSB,RMPIMPSA                                        
         BZ    PARMPX                                                           
         LA    RE,OFORMAT2                                                      
         ST    RE,AOFORMAT                                                      
         DROP  R6                                                               
*                                                                               
PARMPX   DS    0H                                                               
         B     XIT                                                              
*                                                                               
IOLEN    EQU   NIOS*(LIOS+8)                                                    
*                                                                               
         EJECT                                                                  
* SYSTEM ROUTINES ENTERABLE FROM BASE OR OVERLAY                                
         SPACE 1                                                                
         DS    0H                                                               
VCOMMON  NTR1 LABEL=*,BASE=SYSRB                                                
         L     R7,ATWA                                                          
         L     R8,ASPOOLD                                                       
         L     R9,ASYSD            RESTORE POINTER TO SYSTEM STORAGE            
         L     RA,SYSRA            2ND BASE REG - R9 MUST BE SET FIRST          
         ST    RD,COMMRD                                                        
         SRL   RF,24                                                            
         LA    RF,VBRANCH(RF)      POINT TO ENTRY IN BRANCH TABLE               
         CLI   0(RF),0             IF A BRANCH INSTRUCTION                      
         BE    *+8                                                              
         B     0(RF)                  DO BRANCH                                 
*                                                                               
         ICM   RF,15,0(RF)         GET ADDRESS                                  
         A     RF,RELO             ELSE RELOCATE ADDRESS                        
         BASR  RE,RF               AND GO THERE                                 
         XIT1                      GET OUT                                      
*                                                                               
VBRANCH  B     VALSRC              SOURCE                                       
         B     VALBOOK             BOOK                                         
         B     VALDEM              DEMOS                                        
         B     VALSTA              STATION                                      
         B     VALMKT              MARKET                                       
         B     VALTIM              TIME                                         
         B     CURSERR             CURSOR ERROR                                 
         B     GETREP              GET REP                                      
         B     ERRXIT                                                           
         B     VALFILE             FILE                                         
         B     VALDLVL             DEMO PERFORMANCE LEVEL                       
         B     VALPURE             PURE NUMBER                                  
         B     VALDYTM             DAY/DETAIL/TIME                              
         B     VALDAY              DAY                                          
         B     VALINV              INVENTORY NUMBER                             
         B     VALDATE             DATE                                         
         B     IUNDEM              IUN'TIZE A DEMO RECORD (TP OR PAV)           
         B     MYCURS              PLACE CURSOR AT ITEM IN LIST                 
         B     CPROG               SWITCH BETWEEN OVERLAYS                      
         B     RPROG               RETURN TO PREVIOUS OVERLAY                   
         DC    AL4(QTRVAL)         VALIDATE QUARTER EXPRESSIONS                 
         DC    AL4(SLNVAL)         VALIDATE SPOT LENGTHS                        
         DC    AL4(RTCDVAL)        VALIDATE RATE CODE                           
         DC    AL4(GETBKTP)        2-CHAR BOOKTYPE SUPPORT                      
         SPACE 1                                                                
VCOUNT   EQU   (*-VBRANCH)/4                                                    
*                                                                               
         LTORG                                                                  
*                                                                               
         EJECT                                                                  
*  VALIDATE SOURCE - BEGIN TO SET UP OVERLAY DBLOCK (DBLOCKA1)                  
         SPACE 2                                                                
VALSRC   GOTO1 ANY                                                              
         LA    R4,DBLOCKA1                                                      
         USING DBLOCKD,R4                                                       
         XC    DBLOCK,DBLOCK                                                    
         MVC   DBCOMFCS,ACOMFACS                                                
         MVC   DBSELAGY,AGENCY     FOR MARKET SECURITY                          
         MVC   DBFILE,=C'PAV'      PRESET TO PAV                                
         MVC   SVFILE,=C'PAV'                                                   
         MVI   DBSELMED,C'T'                                                    
         MVI   SVMEDIA,C'T'                                                     
         MVI   PAVMEDIA,C'T'                                                    
         MVI   DBSELSRC,C'A'                                                    
         MVI   SVSOURCE,C'A'                                                    
         ZIC   R1,5(R2)                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   8(0,R2),=C'ARB '                                                 
         BNE   SRC3                                                             
         MVC   8(3,R2),=C'ARB '                                                 
         B     SRC20                                                            
SRC3     MVI   DBSELSRC,C'N'                                                    
         MVI   SVSOURCE,C'N'                                                    
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   8(0,R2),NSI                                                      
         BNE   SRC5                                                             
         MVC   8(3,R2),NSI                                                      
         B     SRC20                                                            
         SPACE 1                                                                
SRC5     MVI   DBSELSRC,C'S'                                                    
         MVI   SVSOURCE,C'S'                                                    
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   8(0,R2),SRC                                                      
         BNE   SRC7                                                             
         MVC   8(3,R2),SRC                                                      
         B     SRC20                                                            
         SPACE 1                                                                
SRC7     DS    0H                                                               
         MVI   DBSELSRC,C'M'                                                    
         MVI   SVSOURCE,C'M'                                                    
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   8(0,R2),=C'MFX '                                                 
         BNE   SRC8                                                             
         MVC   8(3,R2),=C'MFX'                                                  
         B     SRC20                                                            
                                                                                
SRC8     DS    0H                                                               
         MVI   DBSELSRC,C'H'       H=NHTI                                       
         MVI   SVSOURCE,C'H'                                                    
         MVI   DBSELMED,C'N'                                                    
         MVI   SVMEDIA,C'N'                                                     
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   8(0,R2),NHT                                                      
         BNE   SRC9                                                             
         MVC   8(3,R2),NHT                                                      
         B     SRC20                                                            
                                                                                
SRC9     DS    0H                                                               
         MVC   CONHEAD(L'INVSRC),INVSRC    INVALID SOURCE                       
         B     MYEND                                                            
                                                                                
SRC20    OI    6(R2),X'80'         TRANSMIT FULL SOURCE NAME                    
         B     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
*  VALIDATE BOOK(S)                                                             
         SPACE 1                                                                
*     CREATE 4 BYTE BOOK FIELD FROM:                                            
*            3 BYTE BOOK IN WORK                                                
*            1 BYTE BOOKTYPE IN BKTYKPE                                         
         SPACE 2                                                                
VALBOOK  XC    BKTYPE,BKTYPE                                                    
         GOTO1 ANY                                                              
         CLI   SVMEDIA,C'N'       FOR NETWORK USE SOURCE=C'N'                   
         BNE   BOOK5                                                            
         GOTO1 BOOKVAL,PARAS,(C'N',(R2)),(MAX,WORK),(C'B',SCANNER),    X        
               BKTYPE,(C'C',ACOMFACS)                                           
         B     BOOK6                                                            
BOOK5    GOTO1 BOOKVAL,PARAS,(SVSOURCE,(R2)),(MAX,WORK),(C'B',SCANNER),X        
               BKTYPE,(C'C',ACOMFACS)                                           
BOOK6    CLI   4(R1),0                                                          
         BNE   *+14                                                             
         MVC   CONHEAD(L'INVBOK),INVBOK   INVALID BOOK                          
         B     MYEND                                                            
         MVC   ACTUAL,4(R1)        PASS USER BACK NUMBER FOUND                  
         SPACE 1                                                                
         ZIC   R1,ACTUAL                                                        
         LA    R3,BOOKS                                                         
         LA    R4,BKTYPE                                                        
         LA    R5,WORK                                                          
BOOK10   MVC   0(3,R3),0(R5)       MOVE IN BOOK                                 
         MVC   3(1,R3),0(R4)       SAVE BKTYPE IN BOOK+3                        
         LA    R3,4(R3)                                                         
         LA    R4,1(R4)                                                         
         LA    R5,3(R5)                                                         
         BCT   R1,BOOK10                                                        
         B     XIT                                                              
         EJECT                                                                  
* VALIDATE DEMO(S)                                                              
         SPACE 1                                                                
*  FOR ROUTINES WITH OPTION OF TP OR PAV FILE, THAT FIELD MUST BE               
*  VALIDATED AND SVFILE SET BEFORE COMING TO THIS ROUTINE                       
         SPACE 2                                                                
VALDEM   GOTO1 ANY                                                              
         LA    R5,DBLOCKA2                                                      
         USING DBLOCKD,R5                                                       
         XC    DBLOCK,DBLOCK                                                    
         MVC   DBCOMFCS,ACOMFACS                                                
         MVI   DBSELMED,C'T'                                                    
         MVC   DBFILE,SVFILE                                                    
         GOTO1 DEMOVAL,PARAS,(NFLDS,(R2)),(MAX,DEMOS),(0,(R5))                  
*                                                                               
         CLI   4(R1),0             IF ALL DEMOS OKAY                            
         BE    VDEMERR                                                          
                                                                                
         MVC   ACTUAL,4(R1)        PASS USER BACK NUMBER FOUND                  
                                                                                
*                                                                               
*          CHECK FOR DMA IMP REQUEST (NOT SUPPORTED)                            
*                                                                               
         SR    R4,R4               INIT COUNTER                                 
         LA    R3,DEMOS                                                         
VALDEM20 AHI   R4,1                INCREMENT COUNTER                            
         CLI   0(R3),X'FF'         EOT?                                         
         BE    VDEMX               YES, EXIT VALIDATION (OK)                    
         CLI   1(R3),C'A'          DMA DEMO?                                    
         BE    VDEMERRA            YES, ERROR                                   
         LA    R3,3(R3)            NO,                                          
         B     VALDEM20              CHECK NEXT                                 
*                                                                               
VDEMX    B     XIT                                                              
*                                                                               
                                                                                
VDEMERR  MVC   CONHEAD(L'INVDEM),INVDEM     INVALID DEMO                        
         B     VDEMERRX                                                         
*                                                                               
VDEMERRA MVC   CONHEAD(L'INVDEMA),INVDEMA   INVALID DEMO (AHOMES)               
         STC   R4,0(R1)                     OVERRIDE INV DEM#                   
         B     VDEMERRX                                                         
*                                                                               
VDEMERRX MVC   FADDR,0(R1)         SAVE A(FIELD IN ERROR)                       
*                                  NUMBER OF FLD IN ERROR IN 1ST BYTE           
*                                                                               
         MVI   ERROR,SUPPLIED      INDICATE OUR ERROR MESSAGE                   
         B     MYCURS                                                           
*                                                                               
         DROP  R5                                                               
*                                                                               
         TITLE 'RERES00 - RESEARCH BASE - VALIDATE STATION - VALSTA'            
***********************************************************************         
*                                                                     *         
*        VALIDATE STATION                                             *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
VALSTA   DS    0H                                                               
         GOTO1 ANY                                                              
         LA    R5,DBLOCKA2                                                      
         USING DBLOCKD,R5                                                       
         XC    DBLOCK,DBLOCK       USE DEMAND TO VALIDATE STATION               
         MVC   DBCOMFCS,ACOMFACS                                                
         LA    RE,IO                                                            
         ST    RE,DBAREC                                                        
         LA    RF,1000                                                          
         XCEF                                                                   
         MVI   DBFUNCT,DBVLST      VALIDATE STATION                             
         MVC   DBFILE,=C'TP '                                                   
         MVI   DBSELSRC,C'N'       TRY NSI FIRST                                
         CLI   SVMEDIA,C'N'        FOR NETWORK, BUILD DIFF DBLOCK               
         BE    STAT30                                                           
         MVC   DBSELSTA,8(R2)                                                   
         MVI   DBSELSTA+4,C'T'                                                  
*                                                                               
         CLI   5(R2),4                                                          
         BE    STAT8                                                            
         BH    STAT2                                                            
         MVI   DBSELSTA+3,C' '     3 CHAR TV                                    
         B     STAT8                                                            
*                                                                               
STAT2    LA    R1,12(R2)           S/B XXXX-X                                   
         CLI   5(R2),6                                                          
         BE    *+12                                                             
         MVI   DBSELSTA+3,C' '     OR XXX-X                                     
         LA    R1,11(R2)                                                        
         CLI   0(R1),C'-'                                                       
         BE    *+14                                                             
         MVC   CONHEAD(L'INVSTN),INVSTN    INVALID STATION                      
         B     MYEND                                                            
         MVC   DBSELSTA+4(1),1(R1)                                              
*                                                                               
STAT8    LA    R3,SRCTBL                                                        
STAT10   MVC   ACTSTAT,DBSELSTA    SAVE STATION                                 
         MVC   DBSELSRC,0(R3)                                                   
         CLI   DBSELSRC,C'A'       FIX FOR KARE/WUSA CALL LETTER SWITCH         
         BNE   STAT11                                                           
         CLC   DBSELSTA(4),=C'KARE'                                             
         BNE   STAT11                                                           
         MVC   ACTSTAT,DBSELSTA                                                 
         MVC   ACTMKT,=H'107'      ARB MINN MKT NUMBER                          
         B     STATX                                                            
*                                                                               
STAT11   GOTO1 DEMAND,PARAS,DBLOCK,0                                            
         CLI   DBERROR,0                                                        
         BE    STAT20                                                           
         CLI   1(R3),X'FF'                                                      
         BNE   STAT18                                                           
         CLI   STATSW,C'I'         STATION SWITCH - I=INVALID ALLOWED           
         BE    XIT                                                              
         MVC   CONHEAD(L'INVSTN),INVSTN    INVALID STATION                      
         B     MYEND                                                            
*                                                                               
STAT18   LA    R3,1(R3)                                                         
         B     STAT10                                                           
*                                                                               
STAT20   MVC   ACTMKT,DBACTRMK     SAVE ACT MARKET                              
         B     STATX                                                            
*                                                                               
STAT30   DS    0H                  NETWROK FILE STATION VALIDATION              
         MVC   DBSELSTA,SPACES                                                  
         CLI   5(R2),5             NETWORK STATIONS ARE 4 CHAR OR LESS          
         BH    STATNX                                                           
         SR    R1,R1                                                            
         IC    R1,5(R2)                                                         
         BCTR  R1,0                                                             
         EXMVC R1,DBSELSTA,8(R2)                                                
         CLI   SVSOURCE,C'H'       FOR NHTI, PUT ' H' IN STAT+4                 
         BNE   STAT32                                                           
         CLC   DBSELSTA+3(2),=C'-H'                                             
         BE    *+10                                                             
         CLC   DBSELSTA+3(2),=C' H'                                             
         BNE   STATNX                                                           
         MVI   DBSELSTA+3,C' '                                                  
*                                                                               
STAT32   CLI   SVSOURCE,C'D'       IF NAD PUT A 'PN' IN STAT+4                  
         BNE   STAT35                                                           
         CLC   DBSELSTA+3(2),=C' N'                                             
         BE    *+10                                                             
         CLC   DBSELSTA+3(2),=C'-N'                                             
         BE    *+10                                                             
         CLC   DBSELSTA+3(2),=C'PN'                                             
         BNE   STATNX                                                           
         MVI   DBSELSTA+3,C'P'                                                  
*                                                                               
STAT35   MVI   DBSELMED,C'N'                                                    
         MVC   DBSELSRC,SVSOURCE                                                
         MVC   ACTSTAT,DBSELSTA                                                 
         MVI   DBFUNCT,DBVLST                                                   
         GOTO1 DEMAND,PARAS,DBLOCK,0                                            
         CLI   DBERROR,0                                                        
         BNE   STATNX              NOT FOUND                                    
         MVC   ACTMKT,DBACTRMK     SAVE ACT MARKET                              
         B     STATX                                                            
*                                                                               
STATX    MVI   STATSW,0            STATION SWITCH - 0=VALID                     
         B     XIT                                                              
*                                                                               
STATNX   MVC   CONHEAD(L'INVSTN),INVSTN    INVALID STATION                      
         B     MYEND                                                            
*                                                                               
SRCTBL   DC    C'NMSA'              NSI,MFX SRC,ARB                             
         DC    X'FF'                                                            
         DROP  R5                                                               
*                                                                               
         EJECT                                                                  
         TITLE 'RERES00 - RESEARCH BASE - VALIDATE MARKET - VALMKT'             
***********************************************************************         
*                                                                     *         
*        VALIDATE MARKET                                              *         
*                                                                     *         
*        GET MARKET NAME - BUILD IN WORK AS IF IT WERE FIELD HEADER   *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
VALMKT   LA    R5,DBLOCKA2                                                      
         USING DBLOCKD,R5                                                       
         MVI   DBFUNCT,DBGETMK                                                  
         MVC   DBSELRMK,ACTMKT                                                  
         GOTO1 DEMAND,PARAS,DBLOCK,0                                            
         CLI   DBERROR,0                                                        
         BNE   XIT                                                              
         SPACE 1                                                                
         GOTO1 DEFINE,PARAS,=C'MNAME',DBLOCK,BLOCK1                             
*                                  ALWAYS BUILD IN WORK-OVERLAY                 
         MVC   WORK+8(29),BLOCK1+2       CAN PUT ON SCREEN ITSELF               
         OI    WORK+6,X'80'                                                     
         B     XIT                                                              
         DROP  R5                                                               
         EJECT                                                                  
* VALIDATE TIME                                                                 
         SPACE 2                                                                
VALTIM   GOTO1 ANY                                                              
         ZIC   R3,5(R2)                                                         
         GOTO1 TIMVAL,PARAS,((R3),8(R2)),WORK                                   
         CLI   0(R1),X'FF'                                                      
         BNE   XIT                                                              
         MVC   CONHEAD(L'INVTIM),INVTIM                                         
         B     MYEND                                                            
         EJECT                                                                  
********NOTE - NO MORE GETMSYS2 - CODE IS NOW NO-OPPED, BUT IF THIS             
********ROUTINE IS TO BE USED, IT NEEDS TO BE CORRECTED*********                
*******************************************************************             
         SPACE 3                                                                
* CURSERR - SET CURSOR TO ERROR POSITION AND EXIT TO ERROR ROUTINE              
*                                                                               
* AT ENTRY, P1 = 0  FADDR=A(ERROR FIELD HEADER), FLAST=A(CURSOR)                
*           ERROR=SUPPLIED FOR USER MESSAGE, SPACE PADDED TEXT AT               
*           WORK.                                                               
*           FNDX=OPTIONAL FIELD INDEX NUMBER TO BE ATTACHED TO MSG              
*           XTRA=OPTIONAL EXTRA TEXT TO BE ATTACHED TO SYSTEM MSG               
*                                                                               
CURSERR  L     R1,ATIOB                                                         
         USING TIOBD,R1                                                         
         L     R2,FADDR                                                         
         LR    RF,R2               COMPUTE DISPLACEMENT OF ERROR FLDH           
         S     RF,ATWA             FROM TWA START                               
         STCM  RF,3,TIOBCURD                                                    
         LR    RF,R2                                                            
         SR    RE,RE                                                            
         ICM   RE,7,FLAST          RE=A(CURSOR POSITION)                        
         LA    RF,8(RF)            RF=A(FIELD START)                            
         SR    RE,RF               COMPUTE INDEX INTO FIELD FOR CURSOR          
         STC   RE,TIOBCURI                                                      
         OI    TIOBINDS,TIOBSETC                                                
*                                                                               
CURSERR2 CLI   ERROR,SUPPLIED      TEST FOR USER SUPPLIED TEXT                  
         BNE   *+14                NO                                           
         MVC   CONHEAD,WORK        YES-GET THE TEXT FROM WORK                   
         B     CURSERR4                                                         
*                                                                               
         SR    R0,R0                                                            
         IC    R0,GETMSYS          ERROR MESSAGE SYSTEM-BUDGET                  
*        CLI   ERROR,60            TEST FOR BUDGET SYSTEM ERROR                 
*        BH    *+8                 YES                                          
*        IC    R0,GETMSYS2         NO-USE GENCON ERROR SYSTEM                   
         GOTO1 GETMSG,DMCB+12,(ERROR,CONHEAD),(X'FF',DMCB),((R0),0)             
*                                                                               
CURSERR4 LA    R4,CONHEAD+L'CONHEAD-1 R4 POINTS TO END OF MSG FLD               
         LA    R3,L'CONHEAD        CALCULATE MESSAGE LENGTH                     
         CLI   0(R4),C' '          TEST FOR BLANK                               
         BNE   *+10                                                             
         BCTR  R4,0                BACK UP POINTER                              
         BCT   R3,*-10                                                          
         LA    R4,1(R4)            POINT TO BLANK AFTER LAST CHAR               
*                                                                               
CURSERR6 CLI   FNDX,0              TEST FOR INDEX NUMBER                        
         BE    CURSERR8                                                         
         LA    R0,L'CONHEAD                                                     
         LA    R3,8(R3)            ADD ON LENGTH OF INDEX MSG +1                
         CR    R3,R0               TEST FOR FIT IN FIELD                        
         BH    CURSERRX            NO - EXIT                                    
         LA    R4,CONHEAD-7(R3)                                                 
         MVC   0(7,R4),=C'- FLD#N'                                              
         EDIT  (B1,FNDX),(1,6(R4))                                              
         LA    R4,7(R4)            POINT TO BLANK AFTER INDEX MSG               
*                                                                               
CURSERR8 CLC   XTRA,SPACES         TEST FOR ANY EXTRA MESSAGE                   
         BE    CURSERRX                                                         
         LA    RE,XTRA+L'XTRA-1                                                 
         LA    R1,L'XTRA           CALCULATE LENGTH OF EXTRA MESSAGE            
         CLI   0(RE),C' '                                                       
         BNE   *+10                                                             
         BCTR  RE,0                                                             
         BCT   R1,*-10                                                          
*                                                                               
         LA    R0,L'CONHEAD                                                     
         LA    R3,1(R1,R3)         COMPUTE TOTAL MESSAGE LENGTH                 
         CR    R3,R0               TEST FOR FIT                                 
         BH    CURSERRX                                                         
         BCTR  R1,0                LESS ONE FOR EXECUTE                         
         EX    R1,*+8              MOVE XTRA TO MESSAGE FIELD                   
         B     CURSERRX                                                         
         MVC   1(0,R4),XTRA        EXECUTED                                     
*                                                                               
CURSERRX CLI   ACTNUM,ACTCHA       TEST IF ERROR OCCURRED                       
         BNE   *+16                ON A CHANGE AFTER A SELECT                   
         CLI   TWALACT,ACTSEL                                                   
         BNE   *+8                                                              
         MVI   ACTNUM,ACTSEL       KEEP SELECT AS LAST ACTION                   
         GOTO1 ERREX2                                                           
         DROP  R1                                                               
         EJECT                                                                  
* GETREP - GET REP DATA FROM CONTROL FILE USER ID RECORD                        
*                                                                               
GETREP   CLI   SVUSRNAM,C' '       DO ONE TIME ONLY                             
         BNH   GETREP1                                                          
         MVC   USERNAME,SVUSRNAM   GET SAVED AGENCY NAME/ADDRESS                
         MVC   USERADDR,SVUSRADD                                                
         B     GETREPX             EXIT                                         
*                                  AGENCY NAME & ADDRESS FROM ID REC.           
GETREP1  MVC   FILENAME,=CL8'CTFILE'                                            
         MVI   USEIO,C'Y'                                                       
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING CTIKEY,R4                                                        
         MVI   CTIKTYP,C'I'                                                     
         MVC   CTIKID+8(2),USERID                                               
         GOTO1 READ                                                             
         MVI   SVUSRNAM,C'*'                                                    
         GOTO1 HELLO,PARAS,(C'G',FILENAME),(X'36',AIO),0                        
         L     R6,12(R1)                                                        
         USING CTORGD,R6                                                        
         MVC   USERNAME,CTORGNAM                                                
         MVC   USERADDR,CTORGADD                                                
         MVC   SVUSRNAM,USERNAME                                                
         MVC   SVUSRADD,USERADDR                                                
         SPACE 1                                                                
GETREPX  XC    FILENAME,FILENAME                                                
         MVI   USEIO,C' '                                                       
         B     XIT                                                              
*                                                                               
         DROP  R6                                                               
*                                                                               
         EJECT                                                                  
*  ALLOW MY OWN ERROR MSG TO BE USED                                            
         SPACE 2                                                                
ERRXIT   CLI   ERROR,X'FE'                                                      
         BE    ERRX2                                                            
         SPACE 1                                                                
         GOTO1 ERREX               SYSTEM MESSAGE                               
ERRX2    GOTO1 ERREX2              MY OWN ERROR MSG                             
         EJECT                                                                  
* VALIDATE FILE (PAV OR TP) - DEFAULT IS PAV                                    
* MUST BE DONE AFTER VALSRCE - VALSRCE ASSUMES PAV FILE                         
*                                                                               
VALFILE  LA    R4,DBLOCKA1                                                      
         USING DBLOCKD,R4                                                       
         CLI   5(R2),0                                                          
         BE    VALFIL10                                                         
         SPACE 1                                                                
         ZIC   R1,5(R2)                                                         
         BCTR  R1,0                                                             
         EX    R1,PAVCOMP                                                       
         BNE   VALFIL20                                                         
         SPACE 1                                                                
VALFIL10 DS    0H                                                               
         MVC   DBFILE,=C'PAV'                                                   
         MVC   SVFILE,=C'PAV'                                                   
         MVC   8(4,R2),=C'PAV '                                                 
         OI    6(R2),X'80'         PUT TO SCREEN                                
         B     XIT                                                              
         SPACE 1                                                                
VALFIL20 EX    R1,TPCOMP                                                        
         BNE   VALFIL30                                                         
         MVC   DBFILE,=C'TP '                                                   
         MVC   SVFILE,=C'TP '                                                   
         MVC   8(4,R2),=C'TP  '                                                 
         OI    6(R2),X'80'         PUT TO SCREEN                                
         B     XIT                                                              
         SPACE 1                                                                
VALFIL30 EX    R1,INVCOMP                                                       
         BNE   VALFIL50                                                         
         MVC   DBFILE,=C'INV '                                                  
         MVC   SVFILE,=C'INV '                                                  
         MVC   8(4,R2),=C'INV '                                                 
         MVI   DBSELMED,C'U'                                                    
         MVI   SVMEDIA,C'U'                                                     
         MVI   PAVMEDIA,C'U'                                                    
         OI    6(R2),X'80'         PUT TO SCREEN                                
         B     XIT                                                              
         SPACE 1                                                                
VALFIL50 MVC   CONHEAD(L'INVFIL),INVFIL                                         
         B     MYEND                                                            
         DROP  R4                                                               
         SPACE 1                                                                
PAVCOMP  CLC   8(0,R2),=C'PAV '                                                 
TPCOMP   CLC   8(0,R2),=C'TP  '                                                 
INVCOMP  CLC   8(0,R2),=C'INV '                                                 
         EJECT                                                                  
* VALIDATE DEMO PERFORMANCE LEVEL FIELD                                         
*                                                                               
* RETURNS FULL WORD DEMO VALUE IN WORK                                          
*                                                                               
VALDLVL  XC    WORK,WORK                                                        
         SPACE 1                                                                
         ZIC   R3,5(R2)            LENGTH                                       
         MVI   ERROR,2                                                          
         CH    R3,=H'6'            NO MORE THAN 6 DIGITS                        
         BH    ERREND                                                           
*                                                                               
         GOTO1 CASHVAL,DMCB,(1,8(R2)),(R3)                                      
*                                                                               
         CLI   DMCB,X'00'                                                       
         BE    DLVL10                                                           
         MVI   ERROR,3             MUST BE NUMERIC                              
         B     ERREND                                                           
DLVL10   EQU   *                                                                
         MVC   WORK(4),DMCB+4                                                   
         SPACE 1                                                                
         B     XIT                                                              
         EJECT                                                                  
* VALIDATE PURE NUMBER                                                          
         SPACE 2                                                                
VALPURE  GOTO1 ANY                                                              
         LA    R4,DBLOCKA1                                                      
         USING DBLOCKD,R4                                                       
         CLI   5(R2),3             LENGTH MUST BE 3                             
         BL    INVPUR                                                           
         CLI   5(R2),4             OR 4                                         
         BH    INVPUR                                                           
         MVC   DUB(2),=2X'F0'      FIRST 2 CHARACTERS                           
         MVZ   DUB(2),8(R2)                                                     
         CLC   DUB(2),=2X'F0'      MUST BE NUMERIC                              
         BNE   INVPUR                                                           
         PACK  DUB,8(2,R2)                                                      
         CVB   R1,DUB                                                           
         STC   R1,DBSELPUR         THAT IS 1/4 HOURS                            
         SPACE 1                                                                
         CLI   10(R2),C'D'         TYPICAL                                      
         BE    PURE4                                                            
         CLI   10(R2),C'E'         WEEKEND                                      
         BE    PURE4                                                            
         CLI   10(R2),C'0'         OR NUMERIC                                   
         BL    INVPUR                                                           
         CLI   10(R2),C'9'                                                      
         BH    INVPUR                                                           
         SPACE 1                                                                
PURE4    LA    R1,DAYLST                                                        
         CLI   0(R1),C'9'                                                       
         BE    PURE8                                                            
PURE6    CLC   10(1,R2),0(R1)                                                   
         BE    PURE8                                                            
         LA    R1,2(R1)                                                         
         B     PURE6                                                            
PURE8    MVC   DBSELPUR+1(1),1(R1)                                              
         B     PURE10                                                           
         SPACE 1                                                                
DAYLST   DC    C'1',X'10'          MON                                          
         DC    C'2',X'20'          TUE                                          
         DC    C'3',X'30'          WED                                          
         DC    C'4',X'40'          THU                                          
         DC    C'5',X'50'          FRI                                          
         DC    C'6',X'60'          SAT                                          
         DC    C'7',X'70'          SUN                                          
         DC    C'8',X'80'          M-SU                                         
         DC    C'D',X'D0'          TYPICAL                                      
         DC    C'0',X'00'          M-F                                          
         DC    C'E',X'E0'          SA-SU (WEEKEND)                              
         DC    C'9',X'90'          OTHER                                        
         SPACE 1                                                                
PURE10   CLI   5(R2),3                                                          
         BE    PURE16                                                           
         LA    R1,WKLIST           MATCH FOURTH CHARACTER V LIST                
         SPACE 1                                                                
PURE12   CLI   0(R1),X'FF'                                                      
         BE    INVPUR                                                           
         CLC   0(1,R1),11(R2)                                                   
         BE    PURE14                                                           
         LA    R1,2(R1)                                                         
         B     PURE12                                                           
         SPACE 1                                                                
PURE14   OC    DBSELPUR+1(1),1(R1)                                              
         SPACE 1                                                                
PURE16   B     XIT                                                              
         SPACE 1                                                                
INVPUR   MVC   CONHEAD(L'INVPURE),INVPURE                                       
         B     MYEND                                                            
         SPACE 1                                                                
         DROP  R4                                                               
         SPACE 1                                                                
WKLIST   DC    C'1',AL1(02)                                                     
         DC    C'2',AL1(04)                                                     
         DC    C'3',AL1(06)                                                     
         DC    C'4',AL1(08)                                                     
         DC    C'5',AL1(10)                                                     
         DC    C'6',AL1(12)                                                     
         DC    C'7',AL1(14)                                                     
         DC    C'0',AL1(01)                                                     
         DC    C'A',AL1(03)                                                     
         DC    C'B',AL1(05)                                                     
         DC    C'C',AL1(07)                                                     
         DC    C'D',AL1(09)                                                     
         DC    C'E',AL1(11)                                                     
         DC    C'F',AL1(13)                                                     
         DC    C'G',AL1(15)                                                     
         EJECT                                                                  
*  VALIDATE DAY/DETAIL AND TIME FIELDS                                          
*  PARAMETER 1 = NUMBER OF DAY/DETAIL/TIME FIELDS                               
*  R2 POINTS AT 1ST DAY/DETAIL FIELD AT START                                   
         SPACE 2                                                                
VALDYTM  L     R3,0(R1)            FOR BCT LOOP                                 
         MVI   DAYTIMES,0                                                       
         XC    DAYTMLST(180),DAYTMLST                                           
         XC    DAYTMLST+180(180),DAYTMLST+180                                   
         SPACE 1                                                                
         CLI   SVFILE,C'I'                                                      
         BE    TPDT                VALIDATE LIKE TIME PERIOD                    
         CLI   SVFILE,C'T'         TOTALLY DIFFERENT VALIDATION                 
         BE    TPDT                FOR TIME PERIOD DAY/TIMES                    
         SPACE 1                                                                
         LA    R5,DAYTMLST                                                      
         SPACE 1                                                                
         GOTO1 ANY                 1 DAY/DETAIL REQUIRED                        
         SPACE 1                                                                
DYTM10   CLI   5(R2),0                                                          
         BE    DYTMX                                                            
         ST    R2,SAVER2                                                        
         MVI   ERROR,2             INVALID INPUT FIELD                          
         XC    DAYXPND,DAYXPND                                                  
         LA    R6,DAYXPND                                                       
         GOTO1 SCANNER,DMCB,(R2),(1,WORK),C',=,/'                               
         CLI   4(R1),1             1 LINE MAX                                   
         BNE   ERREND                                                           
         CLI   WORK+1,0                                                         
         BE    DYTM20              DEFAULT IS /D                                
         CLI   WORK+22,C'D'                                                     
         BE    DYTM20                                                           
         CLI   SVFILE,C'T'         NO /B FOR TP                                 
         BNE   DYTM15                                                           
         CLI   WORK+22,C'B'                                                     
         BE    ERREND                                                           
DYTM15   MVC   0(3,R6),WORK+12                                                  
         LA    R6,3(R6)                                                         
         SPACE 1                                                                
DYTM20   CLC   WORK+12(4),=C'M-SA'    NO MON-SAT AVE LINE, BUT                  
         BNE   DYTM30                                                           
         CLI   WORK+1,0            DEFAULT TO DAYS                              
         BE    *+12                                                             
         CLI   WORK+22,C'D'        OR EXPLICIT DAYS IS OK                       
         BNE   ERREND                                                           
         MVC   0(18,R6),DAYLIST                                                 
         B     DYTM50                                                           
         SPACE 1                                                                
DYTM30   CLC   WORK+12(3),=C'M-F'                                               
         BNE   *+14                                                             
         MVC   0(15,R6),DAYLIST                                                 
         B     DYTM50                                                           
         CLC   WORK+12(4),=C'M-SU'                                              
         BNE   *+14                                                             
         MVC   0(21,R6),DAYLIST                                                 
         B     DYTM50                                                           
         CLC   WORK+12(3),=C'SA-'  NEED 4 CHARS FOR THIS ONE                    
         BNE   DYTM35                                                           
         CLC   WORK+12(4),=C'SA-S'                                              
         BNE   DYTM95                                                           
         MVC   0(6,R6),DAYLIST+15                                               
         B     DYTM50                                                           
         SPACE 1                                                                
DYTM35   CLI   WORK+1,0                                                         
         BE    DYTM40                                                           
         CLI   WORK+22,C'D'                                                     
         BNE   ERREND                                                           
DYTM40   MVC   0(3,R6),WORK+12                                                  
         B     DYTM60                                                           
         SPACE 1                                                                
DYTM50   CLI   WORK+22,C'B'        BOTH AVERAGE AND DAILY                       
         BE    DYTM60                                                           
         CLI   WORK+22,C'D'        DAILY ONLY                                   
         BE    DYTM60                                                           
         CLI   WORK+22,C'A'        AVERAGE ONLY                                 
         BNE   ERREND                                                           
         CLI   SVFILE,C'T'         TIME PERIOD DOESN'T HAVE                     
         BNE   DYTM55                                                           
         CLC   WORK+12(3),=C'M-S'     M-SU AVERAGE LINE                         
         BE    ERREND                                                           
         CLC   WORK+12(3),=C'SA-'     OR SA-SU AVERAGE LINE                     
         BE    ERREND                                                           
         SPACE 1                                                                
DYTM55   XC    DAYXPND+3(24),DAYXPND+3   GET RID OF DAYS                        
         SPACE 1                                                                
DYTM60   BAS   RE,BUMP             LOOK AT TIME FIELD                           
         IC    R4,5(R2)                                                         
         GOTO1 TIMVAL,PARAS,((R4),8(R2)),1(R5)                                  
         CLI   0(R1),X'FF'                                                      
         BNE   DYTM70                                                           
         MVC   CONHEAD(L'INVTIM),INVTIM                                         
         B     MYEND                                                            
         SPACE 1                                                                
DYTM70   MVC   DUB,1(R5)                                                        
         LH    R1,DUB                                                           
         OC    DUB+2(2),DUB+2      TEST FOR END TIME                            
         BNZ   *+8                                                              
         STH   R1,DUB+2            NO-SET END TIME=START TIME                   
         MVC   1(4,R5),DUB                                                      
         SPACE 1                                                                
         LA    R6,DAYXPND                                                       
         SPACE 1                                                                
DYTM80   LA    R4,DAYTBL                                                        
DYTM90   CLC   0(3,R6),0(R4)                                                    
         BE    DYTM100                                                          
         LA    R4,4(R4)                                                         
         CLI   0(R4),X'FF'                                                      
         BNE   DYTM90                                                           
DYTM95   L     R2,SAVER2                                                        
         MVC   CONHEAD(L'INVDAY1),INVDAY1                                       
         B     MYEND                                                            
         SPACE 2                                                                
DAYTBL   DC    C'M-F0MON1TUE2WED3THU4FRI5SAT6SUN7M-S8SA-9'                      
         DC    X'FF'                                                            
         SPACE 2                                                                
DAYLIST  DC    C'MONTUEWEDTHUFRISATSUN'                                         
         SPACE 2                                                                
DYTM100  IC    R1,3(R4)                                                         
         SLL   R1,28                                                            
         SRL   R1,24                                                            
         STC   R1,0(R5)                                                         
         ZIC   R1,DAYTIMES         INCREMENT DAY/TIME LIST COUNT                
         LA    R1,1(R1)                                                         
         STC   R1,DAYTIMES                                                      
         OC    3(3,R6),3(R6)       ARE THERE MORE EXPANDED DAYS                 
         BZ    DYTM110                                                          
         MVC   6(4,R5),1(R5)       USE SAME TIME FOR ALL EXPANDED DAYS          
         SPACE 1                                                                
         LA    R6,3(R6)                                                         
         LA    R5,5(R5)                                                         
         B     DYTM80                                                           
         SPACE 2                                                                
DYTM110  LA    R5,5(R5)                                                         
         BAS   RE,BUMP             NEXT DAY/DETAIL FIELD                        
         BCT   R3,DYTM10                                                        
         SPACE 1                                                                
DYTMX    B     XIT                                                              
         EJECT                                                                  
* THIS ROUTINE IS DAY/TIME VALIDATION FOR TIME PERIOD                           
* VALID INPUT IS A RIGID FORMAT OF 7 DAYS AND AN INDICATOR OF DAILY,            
*  AVERAGES, OR BOTH DAILY AND AVERAGES, IE. M.W.F../B                          
*                                                                               
         SPACE 2                                                                
TPDT     LA    R5,DAYTMLST                                                      
         SPACE 1                                                                
         GOTO1 ANY                 1 DAY/DETAIL REQUIRED                        
         SPACE 1                                                                
TPDT10   CLI   5(R2),0                                                          
         BE    DYTMX                                                            
         ST    R2,SAVER2                                                        
         XC    DAYXPND,DAYXPND                                                  
         MVI   BYTE,0                                                           
         MVI   ERROR,2             INVALID INPUT FIELD                          
         GOTO1 SCANNER,DMCB,(R2),(1,WORK),C',=,/'                               
         CLI   4(R1),1             1 LINE MAX                                   
         BNE   TPDT20                                                           
         CLI   WORK,7              MUST HAVE SOMETHING FOR EVERY DAY            
         BE    TPDT30                                                           
TPDT20   MVC   CONHEAD(L'BADFMT1),BADFMT1                                       
         CLI   SVFILE,C'I'         FOR INV ALLOW M-F,M-S,SA-SU                  
         BNE   MYEND                                                            
         CLI   WORK,3                                                           
         BNE   TPDT25                                                           
         CLC   =C'M-F',WORK+12                                                  
         BNE   *+14                                                             
         MVC   WORK+12(7),=C'MTWTF..'                                           
         B     TPDT26                                                           
         CLC   =C'M-S',WORK+12                                                  
         BNE   MYEND                                                            
         MVC   WORK+12(7),=C'MTWTFSS'                                           
         B     TPDT26                                                           
*                                                                               
TPDT25   CLI   WORK,5                                                           
         BNE   MYEND                                                            
         CLC   =C'SA-SU',WORK+12                                                
         BNE   MYEND                                                            
         MVC   WORK+12(7),=C'.....SS'                                           
*                                                                               
TPDT26   MVI   WORK,7                                                           
         CLI   WORK+1,0                                                         
         BNE   *+16                                                             
         MVI   WORK+1,1                                                         
         MVI   WORK+22,C'B'        DEFAULT: BOTH FOR KEYWORDS                   
         B     TPDT40                                                           
         CLI   WORK+1,1                                                         
         BE    TPDT31                                                           
*                                                                               
TPDT30   CLI   SVFILE,C'I'                                                      
         BNE   TPDT31                                                           
         CLI   WORK+1,0                                                         
         BNE   *+12                                                             
         MVI   WORK+22,C'B'        DEFAULT TO BOTH FOR INV                      
         B     TPDT40                                                           
TPDT31   CLI   WORK+1,1                                                         
         BNE   TPDT35              NO DEFAULT FOR /A,/B,/D                      
         CLI   WORK+22,C'D'                                                     
         BE    TPDT40                                                           
         CLI   WORK+22,C'B'                                                     
         BE    TPDT40                                                           
         CLI   WORK+22,C'A'                                                     
         BE    *+14                                                             
TPDT35   MVC   CONHEAD(L'BADFMT2),BADFMT2                                       
         B     MYEND                                                            
         ZIC   RE,DAYTIMES         COUNT 1 FOR AVERAGE                          
         LA    RE,1(RE)                                                         
         STC   RE,DAYTIMES                                                      
*                                                                               
TPDT40   LA    R2,7                FOR BCT LOOP                                 
         LA    RF,WORK+12          INPUT FIELD                                  
         LA    R1,TPDYLST          TIME PERIOD DAY LIST                         
         LA    R6,DAYXPND                                                       
TPDT50   CLI   0(RF),C'.'                                                       
         BE    TPDT70                                                           
         CLC   0(1,RF),0(R1)                                                    
         BE    TPDT60                                                           
         L     R2,SAVER2           POINT R2 TO FIELD WITH ERROR                 
         MVC   CONHEAD(L'BADFMT1),BADFMT1                                       
         B     MYEND                                                            
         SPACE                                                                  
TPDT60   OC    0(1,R6),1(R1)                                                    
         CLI   WORK+22,C'A'                                                     
         BE    TPDT70                                                           
         CLI   WORK+22,C'B'                                                     
         BNE   *+10                                                             
         OC    BYTE(1),1(R1)       SAVE AVERAGE                                 
         LA    R6,1(R6)            NEXT DAYXPND ENTRY                           
         ZIC   RE,DAYTIMES                                                      
         LA    RE,1(RE)                                                         
         STC   RE,DAYTIMES         NUMBER OF DAYS                               
TPDT70   LA    R1,2(R1)            NEXT TPDYLST ENTRY                           
         LA    RF,1(RF)            NEXT DAY IN INPUT FIELD                      
         BCT   R2,TPDT50                                                        
         SPACE 1                                                                
         CLI   WORK+22,C'B'                                                     
         BNE   *+10                                                             
         MVC   0(1,R6),BYTE        NOW MOVE IN AVERAGE                          
         ZIC   RE,DAYTIMES                                                      
         LA    RE,1(RE)                                                         
         STC   RE,DAYTIMES         AND INCREMENT NUMBER OF DAYS                 
         SPACE 1                                                                
*  NOW MOVE THE DAYS AND TIMES TO DAYTMLST                                      
         SPACE 1                                                                
         L     R2,SAVER2                                                        
         BAS   RE,BUMP             LOOK AT TIME FIELD                           
         IC    R4,5(R2)                                                         
         GOTO1 TIMVAL,PARAS,((R4),8(R2)),1(R5)                                  
         CLI   0(R1),X'FF'                                                      
         BNE   TPDT90                                                           
         MVC   CONHEAD(L'INVTIM),INVTIM                                         
         B     MYEND                                                            
         SPACE 1                                                                
TPDT90   LA    R6,DAYXPND                                                       
TPDT95   MVC   0(1,R5),0(R6)                                                    
         CLI   1(R6),0             ARE THERE MORE DAYS                          
         BE    TPDT100             NO                                           
         MVC   6(4,R5),1(R5)       YES, USE SAME TIME                           
         LA    R6,1(R6)                                                         
         LA    R5,5(R5)                                                         
         B     TPDT95                                                           
         SPACE 1                                                                
TPDT100  BAS   RE,BUMP             NEXT DAY/DETAIL FIELD                        
         LA    R5,5(R5)                                                         
         BCT   R3,TPDT10                                                        
         SPACE 1                                                                
         B     DYTMX                                                            
         SPACE 2                                                                
TPDYLST  DC    C'M',X'40'                                                       
         DC    C'T',X'20'                                                       
         DC    C'W',X'10'                                                       
         DC    C'T',X'08'                                                       
         DC    C'F',X'04'                                                       
         DC    C'S',X'02'                                                       
         DC    C'S',X'01'                                                       
         EJECT                                                                  
***   VALIDATE DAY - RETURNS DAY NUMBER IN ACTUAL                               
VALDAY   GOTO1 ANY                                                              
         GOTO1 DAYVAL,DMCB,(5(R2),8(R2)),ACTUAL,FULL  VALIDATE DAYS             
         CLI   ACTUAL,0                                                         
         BNE   XIT                 VALID DAY EXPRSESSION                        
*                                                                               
         MVC   CONHEAD(L'INVDAY2),INVDAY2                                       
         B     MYEND                                                            
*                                                                               
         EJECT                                                                  
*** VALIDATE INVENTORY NUMBER - RETURNS VALID INV. NUMBER IN WORK               
         SPACE 2                                                                
VALINV   LA    R3,WORK                                                          
         CLI   5(R2),3           MUST HAVE QTR HR NO AND DAY                    
         BL    INVX                                                             
*                                                                               
         CLI   8(R2),C'0'          QTR HOUR MUST BE NUMERIC                     
         BL    INVX                                                             
         CLI   8(R2),C'9'                                                       
         BH    INVX                                                             
         CLI   9(R2),C'0'                                                       
         BL    INVX                                                             
         CLI   9(R2),C'9'                                                       
         BH    INVX                                                             
*                                                                               
         PACK  DUB(8),8(2,R2)                                                   
         CVB   R1,DUB                                                           
         STC   R1,0(R3)            QTR HR NO (BINARY)                           
*                                                                               
         CLI   10(R2),C'D'         TYPICAL                                      
         BE    INV20                                                            
         CLI   10(R2),C'E'         WEEKEND                                      
         BE    INV20                                                            
         CLI   10(R2),C'0'         OR NUMERIC                                   
         BL    INVX                                                             
         CLI   10(R2),C'9'                                                      
         BH    INVX                                                             
INV20    MVC   1(1,R3),10(R2)      DAY CODE (NUMERIC OR D OR E)                 
         CLI   5(R2),4                                                          
         BH    INVX                                                             
         BL    XIT                                                              
         MVC   2(1,R3),11(R2)      PROGRAM LENGTH CODE (A/N)                    
         B     XIT                                                              
         SPACE 1                                                                
INVX     MVC   CONHEAD(L'INVINV),INVINV                                         
         B     MYEND                                                            
         EJECT                                                                  
*** VALIDATE DATE - RETURNS YYMMDD EBCDIC IN WORK                               
*                           LENGTH OF VALID FIELD IN ACTUAL                     
         SPACE 2                                                                
VALDATE  GOTO1 DATVAL,DMCB,(0,8(R2)),WORK                                       
         MVC   ACTUAL(1),DMCB+3       LENGTH OF VALID FIELD                     
         CLI   ACTUAL,0                                                         
         BNE   XIT                                                              
         MVC   CONHEAD(L'INVDATE1),INVDATE1                                     
         BE    MYEND                                                            
         EJECT                                                                  
*                                                                               
*        IUNDEM --- TAKE A PAV OR TP RECORD IN IO AND PASS IT TO GETIUN         
*                    AND THEN DEMAINT TO USE THE SAME FORMULAS AS               
*                    REP INVENTORY RECORDS                                      
*                                                                               
*        CURRENT PAV RECORD IS IN IO                                            
*        WORK AREA TO BE USED IS IO+1000 FOR 2000                               
*                                                                               
*        P1  =   A(DBLOCK TO USE FOR GETIUN AND DEMAINT)                        
*                                                                               
IUNDEM   EQU   *                                                                
*                                                                               
         L     R4,0(R1)                                                         
         USING DBLOCKD,R4                                                       
*                                                                               
         TM    RMPPROF+RMPIMPSB,RMPIMPSA  RTG/IMP PRECISION                     
         BZ    *+8                                                              
         MVI   DBTAPEP,C'Y'                                                     
                                                                                
         GOTO1 VGETIUN,DMCB,(9,DBLOCKD),IO+1000                                 
         MVC   DBNUMVLS,=H'320'                                                 
         LA    RF,IO+1000                                                       
         USING IUNREC,RF                                                        
         MVC   NEWRTG(LENVALS),OLDRTG                                           
         MVC   NEWIMP(LENVALS),OLDIMP                                           
         MVC   NEWHPT(LENVALS),OLDHPT                                           
         MVC   NEWTOT(LENVALS),OLDTOT                                           
         GOTO1 DEMOUT,DMCB,(C'L',SHARES),DBLOCK,HOMSHR,0                        
         LA    RF,OFORMAT                                                       
         CLI   DBTAPEP,C'Y'                                                     
         BNE   *+8                                                              
         LA    RF,OFORMAT2                                                      
         GOTO1 DEMAINT,DMCB,=C'REP',DBLOCKD,IO+1000,(RF)                        
*                                                                               
         B     XIT                                                              
         DROP  R4,RF                                                            
         EJECT                                                                  
*                                                                               
*        POSITION CURSOR TO CORRECT FIELD IN ERRORS                             
*        INPUT : FADDR = AL1(FLD NUMBER),AL3(SCREEN HEADER)                     
*                                                                               
MYCURS   DS    0H                                                               
*                                                                               
         CLI   OFFLINE,C'Y'        SKIP IF OFF-LINE                             
         BE    MYCURSX                                                          
*                                                                               
         L     R2,FADDR            POINT TO FIRST FIELD IN ERROR                
*                                                                               
         L     R1,ATIOB            ESTABLISH TIOB                               
         USING TIOBD,R1                                                         
*                                                                               
         OI    6(R2),X'80'         TRANSMIT ERROR FIELD HEADER                  
         OI    TIOBINDS,TIOBSETC   INSTRUCT CURSOR SETTING                      
*                                                                               
         LR    RF,R2                                                            
         S     RF,ATWA                                                          
         STCM  RF,3,TIOBCURD       DISPLACEMENT FROM START OF TWA               
*                                                                               
         CLI   FADDR,0             APPLICATION MUST SET FIELD NUMBER            
         BE    MYCURSX                                                          
*                                                                               
         LA    RE,8(R2)            START OF FIRST FIELD                         
*                                                                               
         ZIC   RF,FADDR            NUMBER OF FIELD IN ERROR                     
         BCT   RF,*+8              RELATIVE FIELD NUMBER                        
         B     MYCURS0D            FIRST FIELD IS ONE IN ERROR                  
*                                                                               
         SR    R0,R0                                                            
*                                                                               
MYCURS0L DS    0H                                                               
*                                                                               
         IC    R0,5(R2)            R0 HAS FIELD LENGTH                          
*                                                                               
MYCURS1L DS    0H                                                               
*                                                                               
         CLI   0(RE),C','          SCAN FOR THE COMMAS                          
         BNE   MYCURS1C                                                         
*                                  IF COMMA FOUND                               
         BCT   RF,MYCURS1C            DECREMENT FIELD COUNTER                   
*                                     IF FIELD FOUND                            
         LA    RE,1(RE)                  BUMP TO START OF NEXT ITEM             
         B     MYCURS0D                  DONE                                   
*                                                                               
MYCURS1C DS    0H                                                               
*                                                                               
         LA    RE,1(RE)                                                         
         BCT   R0,MYCURS1L                                                      
*                                                                               
MYCURS1D DS    0H                  END OF DATA IN SCREEN FIELD                  
*                                                                               
MYCURS0C DS    0H                                                               
*                                                                               
         ZIC   R0,0(R2)            SCREEN FIELD LENGTH                          
         AR    R2,R0               BUMP TO NEXT SCREEN FIELD                    
*                                                                               
         BCT   RF,MYCURS0L         DECREMENT FIELD COUNTER                      
*                                     IF FIELD FOUND                            
         LA    RE,8(R2)                  POINT TO FIRST IN FIELD                
*                                                                               
MYCURS0D DS    0H                                                               
*                                                                               
         LA    RF,8(R2)            START OF FIELD                               
         SR    RE,RF               DISPLACEMENT TO POSITION IN FIELD            
         STC   RE,TIOBCURI                                                      
*                                                                               
MYCURSX  DS    0H                                                               
         GOTO1 VERRXIT                                                          
*                                                                               
         DROP  R1                                                               
*                                                                               
         EJECT                                                                  
*                                                                               
* THIS ROUTINE FACILITATES THE ACTION OF SWITCHING, WITHIN A SINGLE             
* TRANSACTION, BETWEEN ONE OVERLAY AND ANOTHER BY CHANGING THE RECORD,          
* ACTION, PRINT AND KEY FIELDS ON THE SCREEN AND CALLING GENCON AGAIN.          
* FIRST IT OPTIONALLY SAVES THE CURRENT TWA IN TEMPSTR AND PUSHES THE           
* CURRENT OVERLAY NUMBER ONTO A STACK.  THEN IT CHANGES THE RECORD,             
* ACTION, PRINT AND KEY FIELDS TO THE DATA SPECIFIED IN THE PARAMETER           
* LIST.  FINALLY, IT SETS THE FLAG 'GOAGAIN' TO 'Y' AND TAKES AN ERROR          
* EXIT BACK TO GENCON WHICH RETURNS BACK TO THE CONTROLLER.  THE                
* CONTROLLER THEN RECOGNIZES THE FLAG AND CALLS GENCON AGAIN.  KEY              
* PARAMETERS ARE POSITIONAL, ENDING WITH A ZERO.                                
*                                                                               
* (FLAG,0),RECORD,ACTION,PRINT,(L'KEY1,KEY1),(L'KEY2,KEY2),...,0                
*              IF FLAG IS A 'Y', THEN SCREEN IS SAVED                           
*              RECORD, ACTION, IS REQUIRED                                      
*                                                                               
*  (R7 POINTS TO TWA)                                                           
*                                                                               
CPROG    LR    R4,R1               SAVE POINTER TO PARMS                        
*                                                                               
         CLI   0(R4),C'Y'          SHOULD WE SAVE THE SCREEN?                   
         BE    *+12                YES                                          
         MVI   CALLSP,0            NO -- CLEAR STACK                            
         B     CP3                                                              
*                                                                               
         ZIC   R3,CALLSP           GET STACK POINTER                            
         LR    R2,R3               R2=ORIGINAL STACK POINTER VALUE              
         LA    RF,CALLSTCK(R3)     RF=A(NEXT POSITION)                          
         MVC   0(1,RF),MYSCRNUM    SLOT IN SCREEN NUMBER                        
         MVC   1(1,RF),TWALACT                                                  
         MVC   2(1,RF),TWALREC     SLOT IN SCREEN NUMBER                        
         LA    R3,1(R3)                                                         
         STC   R3,CALLSP                                                        
         CLI   CALLSP,4            TEST MORE THAN 4 NEST LEVELS                 
         BNH   *+6                                                              
         DC    H'0'                YES-A REAL PROBLEM                           
*                                                                               
         SRL   R2,1                DIVIDE ORIGINAL LEVEL BY TWO                 
         LA    R2,3(R2)            ADD BACK THREE TWA PAGES                     
         SLL   R2,32-8             MOVE TO HIGH ORDER BYTE                      
         ICM   R2,3,2(RA)                                                       
         GOTO1 DATAMGR,DMCB,(0,=C'DMREAD'),=C'TEMPSTR',(R2),ATIA,0              
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         STC   R3,BYTE             SAVE STACK LEVEL                             
         L     RE,ATIA             RE=DESTINATION                               
         LA    RF,3072             MOVE/CLEAR HALF A TWA                        
         LA    R0,CONRECH          START AT RECORD HEADER                       
         LR    R1,RF               MOVE RECORD HEADER FOR 3072 BYTES            
         TM    BYTE,X'01'          TEST FOR ODD NUMBER                          
         BO    *+6                 YES                                          
         AR    RE,RF               NO-MOVE TO SECOND HALF OF TWA                
         MVCL  RE,R0                                                            
         GOTO1 DATAMGR,DMCB,(0,=C'DMWRT'),=C'TEMPSTR',(R2),ATIA,0               
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
CP3      LA    R4,4(R4)            BUMP TO SECOND PARM                          
         XC    CONREC,CONREC       SET NEW RECORD TYPE                          
         OI    CONRECH+6,X'80'                                                  
         NI    CONRECH+4,X'DF'                                                  
         L     RF,0(R4)                                                         
         ZIC   RE,0(R4)                                                         
         STC   RE,CONRECH+5                                                     
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   CONREC(0),0(RF)                                                  
*                                                                               
         LA    R4,4(R4)            BUMP TO THIRD PARM                           
         XC    CONACT,CONACT       SET NEW ACTION TYPE                          
         OI    CONACTH+6,X'80'                                                  
         NI    CONACTH+4,X'DF'                                                  
         L     RF,0(R4)                                                         
         ZIC   RE,0(R4)                                                         
         STC   RE,CONACTH+5                                                     
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   CONACT(0),0(RF)                                                  
*                                                                               
         LA    R4,4(R4)            BUMP TO FOURTH PARM                          
         XC    CONWHEN,CONWHEN     SET NEW PRINT OPTION                         
         OI    CONWHENH+6,X'80'                                                 
         NI    CONWHENH+4,X'DF'                                                 
         L     RF,0(R4)                                                         
         ZIC   RE,0(R4)                                                         
         STC   RE,CONWHENH+5                                                    
         CLI   CONWHENH+5,0        IS THERE A PRINT OPTION?                     
         BE    CP5                 NO                                           
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   CONWHEN(0),0(RF)                                                 
*                                                                               
CP5      DS    0H                                                               
*                                                                               
         XC    CONKEY,CONKEY       SET NEW KEY FIELDS                           
         OI    CONKEYH+6,X'80'                                                  
         NI    CONKEYH+4,X'DF'                                                  
         LA    R2,CONKEY           BUILD KEY FIELD FROM FIFTH PARM ON           
         LR    R3,R2                                                            
*                                                                               
         LA    R4,4(R4)            BUMP TO FIFTH PARM                           
         OC    0(4,R4),0(R4)                                                    
         BZ    CPX                 NO KEY PROVIDED                              
*                                                                               
CP10     L     RF,0(R4)            ADD PARM TO KEY FIELD                        
         ZIC   RE,0(R4)                                                         
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R2),0(RF)                                                    
         LA    R2,0(R2,RE)         BUMP R2 PAST IT                              
*                                                                               
         CLI   0(R2),C' '          BACKUP UNTIL NON-SPACE                       
         BH    *+10                                                             
         BCTR  R2,0                                                             
         B     *-10                                                             
         LA    R2,1(R2)                                                         
*                                                                               
         LA    R4,4(R4)            BUMP TO NEXT PARM                            
         OC    0(4,R4),0(R4)                                                    
         BZ    CPX                 IF PARM IS ZERO THEN NO MORE PARMS           
*                                                                               
         MVI   0(R2),C','          ELSE PUT COMMA BEFORE NEXT PARM              
         LA    R2,1(R2)            BUMP R2 TO WHERE NEXT PARM GOES              
         B     CP10                DISPLAY NEXT PARM                            
*                                                                               
CPX      SR    R2,R3               INSERT FAKE INPUT LEN INTO KEY FIELD         
         STC   R2,CONKEYH+5                                                     
*                                                                               
         MVI   RETURNED,0          CLEAR FLAG INDICATING RETURN                 
*                                                                               
         MVI   GOAGAIN,C'Y'        SET FLAG TO CALL GENCON AGAIN                
         SR    R2,R2                                                            
         GOTO1 ERREX2                                                           
         EJECT                                                                  
* THIS ROUTINE RESTORES THE TWA AND OVERLAY NUMBER TO THAT WHICH IS             
* ON THE TOP OF THE OVERLAY STACK.  IT THEN SETS THE 'GOAGAIN' FLAG             
* TO 'YES' AND TAKES AN ERROR EXIT BACK TO GENCON.  WHEN THE CONTROLLER         
* GETS CONTROL BACK IT WILL CALL GENCON AGAIN WITH THE RESTORED SCREEN.         
* (R7 POINTS TO TWA)                                                            
*                                                                               
RPROG    ZIC   R3,CALLSP           GET STACK POINTER                            
         BCTR  R3,0                DECREMENT POINTER TO POP STACK               
         STC   R3,CALLSP                                                        
         LA    RE,CALLSTCK(R3)                                                  
         MVC   MYSCRNUM,0(RE)      EXTRACT OVERLAY NUMBER                       
         MVC   ACTNUM,1(RE)      EXTRACT OVERLAY NUMBER                         
         MVC   RECNUM,2(RE)      EXTRACT OVERLAY NUMBER                         
*                                                                               
         LR    R2,R3                                                            
         SRL   R2,1                DIVIDE LEVEL BY TWO                          
         LA    R2,3(R2)            START AT TWA PAGE 3                          
         SLL   R2,32-8             MOVE PAGE TO HOB                             
         ICM   R2,3,2(R7)                                                       
         GOTO1 DATAMGR,DMCB,(0,=C'DMREAD'),=C'TEMPSTR',(R2),ATIA,0              
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    RE,CONRECH                                                       
         LA    RF,3072             MOVE RECORD HEADER FOR 3072 BYTES            
         L     R0,ATIA                                                          
         LR    R1,RF                                                            
         LA    R3,1(R3)            RESTORE ORIGINAL LEVEL                       
         STC   R3,BYTE                                                          
         TM    BYTE,X'01'          TEST FOR ODD LEVEL                           
         BO    *+6                 YES                                          
         AR    R0,R1               NO-MUST BE IN SECOND HALF OF PAGE            
         MVCL  RE,R0                                                            
*                                                                               
         L     R2,ATWA             MUST SET INDICTOR TO XMIT ALL FIELDS         
         LA    R2,64(R2)               OR SCREEN WILL BE MESSED UP              
         CLI   0(R2),0                                                          
         BE    *+16                FIND END OF TWA                              
         SR    R0,R0                                                            
         IC    R0,0(R2)                                                         
         AR    R2,R0                                                            
         B     *-16                                                             
         MVC   1(2,R2),=X'0101'    SET INDICATOR TO XMIT ALL FIELDS             
*                                                                               
         MVC   TWASCR,MYSCRNUM     DON'T LET GENCON RELOAD THE SCREEN           
         MVC   OVERLAY,MYSCRNUM    SET OVERLAY TOO                              
*                                                                               
         MVI   GOAGAIN,C'Y'        SET FLAG TO CALL GENCON AGAIN                
         SR    R2,R2                                                            
         GOTO1 ERREX2                                                           
*                                                                               
         LTORG                                                                  
*                                                                               
         EJECT                                                                  
* SUPPORTING SMALL SUB-ROUTINES                                                 
*                                                                               
BUMP     ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         BR    RE                                                               
         EJECT                                                                  
*                                                                               
* CHECK FOR GLOBBER LOADER VARIABLES FROM RMP                                   
*                                                                               
CKGLOB   NTR1 LABEL=*                                                           
         OC    VGLOBBER,VGLOBBER   SKIP IF NO GLOBBER ADDRESS                   
         BZ    CKGLOBNO                                                         
*                                                                               
         L     RF,VGLOBBER                                                      
                                                                                
         GOTO1 (RF),DMCB,=C'GETD',WORK,6,GLRCONNO                               
         TM    DMCB+8,X'10'        NO VARIABLES FOUND, SKIP                     
         BNZ   CKGLOBNO                                                         
         GOTO1 (RF),DMCB,=C'DELE',,,GLRCONNO                                    
                                                                                
         CLC   WORK(6),=CL6'MASTER'                                             
         BNE   CKGLOBNO                                                         
         MVC   CONREC(6),WORK                                                   
         MVI   CONRECH+5,6                                                      
         OI    CONRECH+6,X'80'    XMIT FIELD                                    
         MVI   CONRECH+1,X'02'    MODIFIED                                      
         MVI   CONRECH+4,X'CA'    INPUT INDICATORS                              
                                                                                
         MVC   CONACT(3),=CL3'REP'                                              
         MVI   CONACTH+5,3                                                      
         OI    CONACTH+6,X'80'    XMIT FIELD                                    
         MVI   CONACTH+1,X'02'    MODIFIED                                      
         MVI   CONACTH+4,X'CA'    INPUT INDICATORS                              
                                                                                
         MVC   CONWHEN(3),=CL3'NOW'                                             
         MVI   CONWHENH+5,3                                                     
         OI    CONWHENH+6,X'80'    XMIT FIELD                                   
         MVI   CONWHENH+1,X'02'    MODIFIED                                     
         MVI   CONWHENH+4,X'CA'   INPUT INDICATORS                              
                                                                                
CKGLOBYS SR    R0,R0                                                            
         B     *+8                                                              
CKGLOBNO LA    R0,1                                                             
         LTR   R0,R0                                                            
         XIT1                                                                   
         EJECT                                                                  
* GETEL - ELEMENT SEARCH                                                        
*         AT ENTRY, R4 POINTS TO RECORD, ELCODE IS SET                          
*         AT EXIT, CC=EQ IF ELEMENT FOUND AND R6 POINTS TO IT                   
*                  CC=NEQ IF ELEMENT DOES NOT EXIST                             
GETEL    LR    R0,RE                                                            
         GOTO1 HELLO,DMCB,(C'G',SYSFIL),(ELCODE,(R4)),0                         
         L     R6,12(R1)                                                        
         LR    RE,R0                                                            
         CLI   12(R1),0                                                         
         BR    RE                                                               
         SPACE 3                                                                
MYEND    MVI   ERROR,X'FE'         USING MY OWN ERROR MSG                       
ERREND   GOTO1 VERRXIT                                                          
         SPACE 2                                                                
**                                                                              
         EJECT                                                                  
XIT      XIT1                                                                   
         EJECT                                                                  
*                                                                               
* LITERAL POOL                                                                  
         LTORG                                                                  
* CONSTANTS,TABLES,ETC                                                          
*                                                                               
         SPACE 1                                                                
RELO     DS    A                                                                
SYSVCON  DS    0F                                                               
         DC    V(DUMMY)                                                         
NVTYPES  EQU   (*-SYSVCON)/4                                                    
ADTAB    DS    0AL3                                                             
NADCONS  EQU   (*-ADTAB)/L'ADTAB                                                
CORETAB  DS    0X                                                               
         DC    X'30E0260947131C1B22212908'                                      
CORES    EQU   (*-CORETAB)                                                      
OFORMAT  DC    C'IUNUIUN',X'530B00'  RATING BASED DEMOS                         
OFORMAT2 DC    C'IUNUIUN',X'5A0B00'  IMP BASED DEMOS                            
*                                                                               
SHARES   DC    X'00',C'S',X'01'                                                 
         DC    X'00',C'S',X'02'                                                 
         DC    X'00',C'S',X'03'                                                 
         DC    X'FF'                                                            
*                                                                               
* PATCH AREA                                                                    
*                                                                               
PATCH    DS    0H                                                               
         DC    XL32'00'                                                         
         EJECT                                                                  
*                                                                               
NHT      DC    C'NHT '                                                          
NSI      DC    C'NSI '                                                          
*ARB      DC    C'ARB '                                                         
SRC      DC    C'SRC '                                                          
*        MESSAGES                                                               
*                                                                               
INVSRC   DC    C'* ERROR * INVALID SOURCE'                                      
INVBOK   DC    C'* ERROR * INVALID BOOK'                                        
INVDEM   DC    C'* ERROR * INVALID DEMO'                                        
INVDEMA  DC    C'* ERROR * DMA IMPRESSIONS NOT SUPPORTED    '                   
INVSTN   DC    C'* ERROR * INVALID STATION'                                     
INVTIM   DC    C'* ERROR * INVALID TIME EXPRESSION'                             
INVFIL   DC    C'* ERROR * INVALID FILE'                                        
DLVL1    DC    C'* ERROR * INVALID DEMO PERFORMANCE LEVEL'                      
DLVL2    DC    C'* ERROR * FORMAT IS XX.X'                                      
INVPURE  DC    C'* ERROR * INVALID PURE NUMBER'                                 
INVDAY1  DC    C'* ERROR * INVALID DAY/DETAIL'                                  
INVDAY2  DC    C'* ERROR * INVALID DAY'                                         
INVINV   DC    C'* ERROR * INVALID INVENTORY NUMBER'                            
INVDATE1 DC    C'* ERROR * INVALID DATE'                                        
BADFMT1  DC    C'7 DAYS REQUIRED-USE DAY OR DOT. EX: MTWTF..'                   
BADFMT2  DC    C'DETAIL EXPRESSION REQUIRED - /A, /D OR /B'                     
BADFMT3  DC    C'M-F, M-S, SA-SU KEYWORDS USE /A ONLY'                          
         EJECT                                                                  
RECACTS  DS    0D                                                               
         SPACE 1                                                                
*        SEE TOP OF MODULE FOR NOTE ON X'04' ENTS                               
* X'01' ENTRIES ARE AVAILABLE REPORTS                                           
*                                                                               
* CL8 EXPANDED REPORT NAME                                                      
* CL1 REPORT NUMBER (01 IS RESERVED)                                            
* CL1 PHASE NUMBER FOR DATA DICTIONARY - LEAVE AS ZERO FOR NOW                  
* CL1 PHASE NUMBER FOR HELP SCREEN - LEAVE AS ZERO FOR NOW                      
         SPACE 1                                                                
         DC    X'01',C'PURE    ',AL1(02),X'0000'                                
         DC    X'01',C'FLEXI   ',AL1(03),X'0000'                                
         DC    X'01',C'SEARCH  ',AL1(04),X'0000'                                
         DC    X'01',C'LAYOUT  ',AL1(05),X'0000'                                
         DC    X'01',C'MASTER  ',AL1(06),X'0000'                                
         DC    X'01',C'DMENU   ',AL1(07),X'0000'                                
         DC    X'01',C'TITLES  ',AL1(08),X'0000'                                
         DC    X'01',C'MPR     ',AL1(09),X'0000'                                
         DC    X'01',C'FETCH   ',AL1(12),X'0000'                                
         DC    X'04',C'XMASTER ',AL1(13),X'0000'                                
         DC    X'04',C'AVAIL   ',AL1(14),X'0000'                                
         DC    X'04',C'KATZ    ',AL1(15),X'0000'                                
         SPACE 2                                                                
         SPACE 2                                                                
* X'02' ENTRIES ARE AVAILABLE ACTIONS                                           
*                                                                               
* CL8 EXPANDED ACTION NAME                                                      
* CL1 ACTION NUMBER                                                             
* CL1 ACTION EQUATE                                                             
* CL1 SPARE                                                                     
         SPACE 1                                                                
         DC    X'02',C'ADD     ',AL1(01,01,00)                                  
         DC    X'02',C'CHANGE  ',AL1(02,01,00)                                  
         DC    X'02',C'DISPLAY ',AL1(03,01,00)                                  
         DC    X'02',C'DELETE  ',AL1(04,01,00)                                  
         DC    X'02',C'SELECT  ',AL1(05,01,00)                                  
         DC    X'02',C'RESTORE ',AL1(06,01,00)                                  
         DC    X'02',C'LIST    ',AL1(10,10,00)                                  
         DC    X'02',C'REPORT  ',AL1(12,12,00)                                  
         DC    X'02',C'HELP    ',AL1(00,00,00)                                  
         SPACE 2                                                                
* DIRECTORY OF PHASES FOR SELECTED RECORD/ACTION                                
         SPACE 1                                                                
* X'03' ENTRIES ARE OK REC/ACT COMBOS                                           
* CL1 RECORD NUMBER                                                             
* CL1 ACTION EQUATE                                                             
* CL1 PHASE NUMBER FOR SCREEN                                                   
* CL1 PHASE NUMBER FOR EDIT                                                     
* CL1 PHASE NUMBER FOR SPECS                                                    
* CL1 PHASE NUMBER FOR REPORT                                                   
* CL1 WHEN OK BITS 80=SCREEN 40=NOW 20=SOON 10=OV 08=DDS                        
* CL2 CODE FOR REPORTS                                                          
* CL2 CODE FOR EOD HANDLING                                                     
         SPACE 1                                                                
*                                 SC  SP  AC                                    
*                                   OV  RP                                      
         DC    X'03',AL1(02,12),X'F101000178',C'SPSP'     PURE  REPORT          
         DC    X'03',AL1(03,12),X'F202000278',C'SFSF'     FLEXI REPORT          
         DC    X'03',AL1(04,12),X'F303000378',C'SSSS'    SEARCH REPORT          
         DC    X'03',AL1(05,12),X'F404000478',C'SLSL'   LAYOUT  REPORT          
         DC    X'03',AL1(06,12),X'F505000578',C'SMSM'    MASTER REPORT          
         DC    X'03',AL1(07,12),X'F606000678',C'SDSD' DEMO MENU REPORT          
         DC    X'03',AL1(08,12),X'F707000738',C'STST'    TITLES REPORT          
         DC    X'03',AL1(09,12),X'F808000978',C'SXSX'    MPR    REPORT          
         DC    X'03',AL1(09,12),X'F808000918',C'SXSX'    MPR - 2 COPIES         
         DC    X'03',AL1(12,01),X'C535000080',C'    '    FETCH MAINT            
         DC    X'03',AL1(13,12),X'FC0C000C78',C'XMXM'    TST MASTER RPT         
         DC    X'03',AL1(14,12),X'FC0C000C38',C'GAGA'    GENERAL AVAIL          
         DC    X'FF'                                                            
*                                                                               
RECACT1  DS    0X                  ALTERNATE RECORD TABLE                       
*        SEE TOP OF MODULE FOR NOTE ON X'04' ENTS                               
* X'01' ENTRIES ARE AVAILABLE REPORTS                                           
*                                                                               
* CL8 EXPANDED REPORT NAME                                                      
* CL1 REPORT NUMBER (01 IS RESERVED)                                            
* CL1 PHASE NUMBER FOR DATA DICTIONARY - LEAVE AS ZERO FOR NOW                  
* CL1 PHASE NUMBER FOR HELP SCREEN - LEAVE AS ZERO FOR NOW                      
         SPACE 1                                                                
         DC    X'01',C'PURE    ',AL1(02),X'0000'                                
         DC    X'01',C'FLEXI   ',AL1(03),X'0000'                                
         DC    X'01',C'SEARCH  ',AL1(04),X'0000'                                
         DC    X'01',C'LAYOUT  ',AL1(05),X'0000'                                
         DC    X'04',C'MASTER  ',AL1(06),X'0000'                                
         DC    X'01',C'DMENU   ',AL1(07),X'0000'                                
         DC    X'04',C'TITLES  ',AL1(08),X'0000'                                
         DC    X'01',C'MPR     ',AL1(09),X'0000'                                
         DC    X'01',C'DPT     ',AL1(10),X'0000'                                
         DC    X'01',C'MENU    ',AL1(11),X'0000'                                
         DC    X'01',C'FETCH   ',AL1(12),X'0000'                                
         DC    X'04',C'XMASTER ',AL1(13),X'0000'                                
         DC    X'04',C'AVAIL   ',AL1(14),X'0000'                                
         DC    X'04',C'KATZ    ',AL1(15),X'0000'                                
         DC    X'FF'                                                            
*                                                                               
RECACT3  DS    0X                                                               
*                                                                               
* X'03' ENTRIES ARE OK REC/ACT COMBOS                                           
* CL1 RECORD NUMBER                                                             
* CL1 ACTION EQUATE                                                             
* CL1 PHASE NUMBER FOR SCREEN                                                   
* CL1 PHASE NUMBER FOR EDIT                                                     
* CL1 PHASE NUMBER FOR SPECS                                                    
* CL1 PHASE NUMBER FOR REPORT                                                   
* CL1 WHEN OK BITS 80=SCREEN 40=NOW 20=SOON 10=OV 08=DDS                        
* CL2 CODE FOR REPORTS                                                          
* CL2 CODE FOR EOD HANDLING                                                     
         SPACE 1                                                                
*                                 SC  SP  AC                                    
*                                   OV  RP                                      
         DC    X'03',AL1(02,12),X'F101000178',C'SPSP'     PURE  REPORT          
         DC    X'03',AL1(03,12),X'F202000278',C'SFSF'     FLEXI REPORT          
         DC    X'03',AL1(04,12),X'F303000378',C'SSSS'    SEARCH REPORT          
         DC    X'03',AL1(05,12),X'F404000478',C'SLSL'   LAYOUT  REPORT          
         DC    X'03',AL1(06,12),X'FC0C000C78',C'SMSM'    MASTER REPORT          
         DC    X'03',AL1(07,12),X'F606000678',C'SDSD' DEMO MENU REPORT          
         DC    X'03',AL1(08,12),X'FD0D000D38',C'STST'    TITLES REPORT          
         DC    X'03',AL1(09,12),X'F808000978',C'SXSX'    MPR    REPORT          
         DC    X'03',AL1(09,12),X'F808000918',C'SXSX'    MPR - 2 COPIES         
         DC    X'03',AL1(10,01),X'FA0A000080',C'    '    DAYPART MAINT          
         DC    X'03',AL1(10,10),X'EA0A000A80',C'    '    DAYPART LIST           
*        DC    X'03',AL1(10,12),X'EA0A000A78',C'DPDP'    DAYPART REPORT         
         DC    X'03',AL1(11,01),X'FB0B000080',C'    '    MENU    MAINT          
         DC    X'03',AL1(11,10),X'EB0B000B80',C'    '    MENU    LIST           
*        DC    X'03',AL1(11,12),X'EB0B000B78',C'MNMN'    MENU    REPORT         
         DC    X'03',AL1(12,01),X'C535000080',C'    '    FETCH MAINT            
         DC    X'03',AL1(14,12),X'FE0E000E38',C'GAGA'    MASTER AVAIL           
         DC    X'03',AL1(15,12),X'FC0C000C38',C'SMSM'    KATZ MASTER            
         DC    X'FF'                                                            
         EJECT                                                                  
         TITLE 'VALIDATE QUARTERS - QTRVAL'                                     
***********************************************************************         
*                                                                     *         
*        VALIDATE QUARTER                                             *         
*                                                                     *         
*FORMATS  - 1Q95,95/Q1,Q1/95, 1Q95-4Q95, 1-4Q95, 3Q95-2Q96            *         
*                                                                     *         
*                                                                     *         
*NTRY    P0 - 0   AL1(QTR LIST LENGTH)  0 IMPLIES SCREEN FIELD        *         
*        P0 - 1-3 AL3(QUARTERS)                                       *         
*                                                                     *         
*        P1 -     A(QUARTERS     SAVEAREA LIST)                       *         
*                 ENTRY XL1(YEAR)        - BINARY                     *         
*                       XL1(QUARTER)     - BINARY                     *         
*                                                                     *         
*EXIT            QUARTER LIST FILLED IN AT P1 - EOT = NULL ENTRY      *         
*                                                                     *         
*       P1 - 0   AL1(NUMBER OF ENTRIES IN LIST)                       *         
*                                                                     *         
*NOTE: ROUTINE USES WORK                                              *         
*                   FLDH                                              *         
*                   FLD                                               *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
         DS    0D                                                               
QTRVAL   NTR1  LABEL=*,BASE=*                                                   
*                                                                               
         LR    R5,R1               SAVE PARAMETERS POINTER                      
*                                                                               
         L     R2,0(R5)            POINT TO QUARTERS COMING IN                  
*                                                                               
         CLI   0(R5),0             IF LENGTH NOT GIVEN THEN SCREEN FLD          
         BE    QTRVSRX                                                          
*                                  ELSE                                         
         XC    FLDH,FLDH           CREATE DUMMY SCREEN FIELD                    
         XC    FLD,FLD                                                          
*                                                                               
         SR    RF,RF                                                            
         IC    RF,0(R5)            GET FIELD LENGTH                             
         STC   RF,FLDH+5           SET FIELD LENGTH                             
         BCTR  RF,0                DECREMENT FOR EXECUTE                        
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   FLD(0),0(R2)                                                     
*                                                                               
         LA    R2,FLDH             POINT TO DUMMY SCREEN FIELD                  
*                                                                               
QTRVSRX DS     0H                                                               
*                                                                               
         GOTO1 ANY                                                              
*                                                                               
         L     R3,4(R5)            POINT TO QUARTERS RETURN AREA                
         XC    0(2,R3),0(R3)       INIT RETURN AREA                             
*                                                                               
         XC    WORK+16(4),WORK+16  INIT WORKAREA                                
         LA    RF,WORK+16          SAVE A(CURRENT YEAR)                         
         ST    RF,WORK                                                          
         LA    RF,1(RF)            SAVE A(CURRENT QUARTER)                      
         ST    RF,WORK+4                                                        
*                                                                               
         SR    R0,R0                                                            
         ICM   R0,1,5(R2)          GET INPUT LENGTH                             
         BZ    QTRVALX             NOTHING ENTERED                              
*                                                                               
         LA    R4,1                INIT FIELD COUNTER FOR ERRORS                
         SR    R6,R6               INIT YEAR/QTR INDICATOR                      
         LA    R1,8(R2)            POINT TO INPUT                               
         LR    RE,R1               SAVE START OF FIELD                          
*                                                                               
QTRVLOOP DS    0H                                                               
*                                                                               
         CLI   0(R1),C'0'          OKAY IF NEXT DIGIT IS NUMERIC                
         BL    QTRVLP10                                                         
         CLI   0(R1),C'9'                                                       
         BNH   QTRVCONT                                                         
*                                                                               
QTRVLP10 DS    0H                                                               
*                                                                               
         CLI   0(R1),C','          OKAY IF END OF FIELD                         
         BE    QTRVLP15                                                         
*                                                                               
         CLI   0(R1),C'Q'          INTERESTED IF 'Q'                            
         BE    *+8                                                              
         CLI   0(R1),C'/'          INTERESTED IF '/'                            
         BE    *+8                                                              
         CLI   0(R1),C'-'          INTERESTED IF '-'                            
         BNE   QTRVNVE                                                          
*                                                                               
         LR    R6,R1               SAVE INDICATOR                               
*                                                                               
         CR    RE,R1               IF NO PRECEDING NUMBER                       
         BNE   QTRVLP15                                                         
*                                                                               
         CLI   0(R6),C'Q'             MUST BE A 'Q'                             
         BNE   QTRVNVE                                                          
*                                                                               
         LA    RE,1(RE)               FIELD STARTS AT NEXT BYTE                 
         B     QTRVCONT               CONTINUE FIELD EXAMINATION                
*                                                                               
QTRVLP15 DS    0H                                                               
*                                                                               
         LTR   RF,R6               POINT TO SECTION TERMINATOR Q-/              
         BNZ   *+6                                                              
         LR    RF,R1               IF NONE USE COMMA POSITION                   
*                                                                               
         SR    RF,RE               LENGTH OF NUMBER                             
         BZ    QTRVNVE                                                          
*                                                                               
         LPR   RF,RF               'Q1' PRODUCES NEGATIVE LENGTH                
*                                                                               
         CH    RF,=H'2'            MAX 2 DIGITS ALLOWED                         
         BH    QTRVNVE                                                          
*                                                                               
         BCTR  RF,0                DECREMENT FOR EXECUTE                        
         EX    RF,*+8                                                           
         B     *+10                                                             
         PACK  DUB,0(0,RE)         PACK NUMBER                                  
*                                                                               
         CVB   RE,DUB              CVB                                          
*                                                                               
         LTR   RF,RF               ASSUME YEAR IF LENGTH IS 2                   
         BZ    QTRVLP20                                                         
*                                                                               
         CH    RE,=H'70'           STORE NEXT CENTURY AS +100                   
         BH    *+8                                                              
         LA    RE,100(RE)                                                       
*                                                                               
         L     RF,WORK             STORE IN YEAR BUCKET                         
         STC   RE,0(RF)                                                         
*                                                                               
         B     QTRVLP30                                                         
*                                                                               
QTRVLP20 DS    0H                                                               
*                                                                               
         LTR   RE,RE               MUST HAVE A QUARTER                          
         BNP   QTRVNVE                                                          
*                                                                               
         CH    RE,=H'4'            QUARTER HAS MAX OF 4                         
         BH    QTRVNVE                                                          
*                                                                               
         L     RF,WORK+4           STORE IN QTR  BUCKET                         
         STC   RE,0(RF)                                                         
*                                                                               
QTRVLP30 DS    0H                                                               
*                                                                               
         CLI   0(R6),C'-'          IF START OF RANGE                            
         BNE   QTRVLP40                                                         
*                                                                               
         L     RF,WORK             BUMP YEAR AND QUARTER POINTERS               
         LA    RF,2(RF)                                                         
         ST    RF,WORK                                                          
*                                                                               
         L     RF,WORK+4           BUMP YEAR AND QUARTER POINTERS               
         LA    RF,2(RF)                                                         
         ST    RF,WORK+4                                                        
*                                                                               
QTRVLP40 DS    0H                                                               
*                                                                               
         CLI   0(R1),C','          SKIP IF NOT END OF FIELD                     
         BNE   QTRVLP50                                                         
*                                                                               
         LA    RF,WORK+16          POINT TO START OF QUARTERS                   
*                                                                               
         CLI   1(RF),0             MUST HAVE A STARTING QUARTER                 
         BE    QTRVNVE                                                          
*                                                                               
         CLI   0(RF),0             IF NO YEAR GIVEN                             
         BNE   *+10                                                             
         MVC   0(1,RF),2(RF)          COPY ENDING YEAR                          
*                                                                               
         CLI   0(RF),0             MUST HAVE A YEAR                             
         BE    QTRVNVE                                                          
*                                                                               
         BAS   RE,QTRVRTN          RETURN QUARTERS                              
*                                                                               
         LA    R4,1(R4)            BUMP NUMBER OF ENTRIES                       
         XC    WORK+16(4),WORK+16  INIT WORKAREA                                
         LA    RF,WORK+16          SAVE A(CURRENT YEAR)                         
         ST    RF,WORK                                                          
         LA    RF,1(RF)            SAVE A(CURRENT QUARTER)                      
         ST    RF,WORK+4                                                        
*                                                                               
QTRVLP50 DS    0H                                                               
*                                                                               
         LA    RE,1(R1)            START OF NEXT FIELD                          
         SR    R6,R6               RESET INDICATOR                              
*                                                                               
QTRVCONT DS    0H                                                               
*                                                                               
         LA    R1,1(R1)            BUMP INPUT POINTER                           
         BCT   R0,QTRVLOOP         CONTINUE ANALYSIS                            
*                                                                               
QTRVDONE DS    0H                                                               
*                                                                               
*        HANDLE LAST FIELD                                                      
*                                                                               
         LR    RF,R1               USE CURRENT POSITION                         
*                                                                               
         SR    RF,RE               LENGTH OF NUMBER                             
         BNP   QTRVNVE                                                          
*                                                                               
         LPR   RF,RF               'Q1' PRODUCES NEGATIVE LENGTH                
*                                                                               
         CH    RF,=H'2'            MAX 2 DIGITS ALLOWED                         
         BH    QTRVNVE                                                          
*                                                                               
         BCTR  RF,0                DECREMENT FOR EXECUTE                        
         EX    RF,*+8                                                           
         B     *+10                                                             
         PACK  DUB,0(0,RE)         PACK NUMBER                                  
*                                                                               
         CVB   RE,DUB              CVB                                          
*                                                                               
         LTR   RF,RF               ASSUME YEAR IF LENGTH IS 2                   
         BZ    QTRVDN20                                                         
*                                                                               
         CH    RE,=H'70'           STORE NEXT CENTURY AS +100                   
         BH    *+8                                                              
         LA    RE,100(RE)                                                       
*                                                                               
         L     RF,WORK             STORE IN YEAR BUCKET                         
         STC   RE,0(RF)                                                         
*                                                                               
         B     QTRVDN30                                                         
*                                                                               
QTRVDN20 DS    0H                                                               
*                                                                               
         LTR   RE,RE               MUST HAVE A QUARTER                          
         BNP   QTRVNVE                                                          
*                                                                               
         CH    RE,=H'4'            QUARTER HAS MAX OF 4                         
         BH    QTRVNVE                                                          
*                                                                               
         L     RF,WORK+4           STORE IN QTR  BUCKET                         
         STC   RE,0(RF)                                                         
*                                                                               
QTRVDN30 DS    0H                                                               
*                                                                               
         LA    RF,WORK+16          POINT TO START OF QUARTERS                   
*                                                                               
         CLI   1(RF),0             MUST HAVE A STARTING QUARTER                 
         BE    QTRVNVE                                                          
*                                                                               
         CLI   0(RF),0             IF NO YEAR GIVEN                             
         BNE   *+10                                                             
         MVC   0(1,RF),2(RF)          COPY ENDING YEAR                          
*                                                                               
         CLI   0(RF),0             MUST HAVE A YEAR                             
         BE    QTRVNVE                                                          
*                                                                               
         BAS   RE,QTRVRTN          RETURN QUARTERS                              
*                                                                               
         L     RF,4(R5)            START OF TABLE                               
         SR    R3,RF               LENGTH OF TABLE                              
         SRL   R3,1                DIVIDE BY ENTRY LENGTH                       
         STC   R3,4(R5)            RETURN NUMBER OF ENTRIES                     
*                                                                               
         B     QTRVALX                                                          
*                                                                               
*        INVALID QUARTER                                                        
*                                                                               
QTRVNVE DS     0H                                                               
*                                                                               
         CLI   0(R5),0             DONE IF NOT A SCREEN FIELD                   
         BNE   QTRVALX                                                          
*                                                                               
         ST    R2,FADDR            SAVE A(FIELD IN ERROR)                       
         STC   R4,FADDR            NUMBER OF FLD IN ERROR IN 1ST BYTE           
*                                                                               
         MVI   ERROR,SUPPLIED      INDICATE OUR ERROR MESSAGE                   
         MVC   CONHEAD(L'INVQTR),INVQTR INVALID QUARTER                         
         GOTO1 VMYCURS                                                          
*                                                                               
QTRVALX  DS    0H                                                               
         XIT1                                                                   
*                                                                               
INVQTR   DC    C'ERROR - INVALID QUARTER EXPRESSION'                            
*                                                                               
         TITLE 'VALIDATE QUARTERS - RETURN QUARTERS - QTRVRTN'                  
***********************************************************************         
*                                                                     *         
*        ROUTINE TO RETURN QUARTERS TO CALLER                         *         
*                                                                     *         
*NTRY    WORK+16(4) - START AND END QUARTERS                          *         
*        R3 ==>   CURRENT ENTRY IN QUARTERS TABLE                     *         
*                                                                     *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
         DS    0D                                                               
QTRVRTN  NTR1 LABEL=*                                                           
*                                                                               
         LA    RF,WORK+16          START AND END QUARTERS                       
*                                                                               
         SR    RE,RE                                                            
         ICM   RE,1,0(RF)          GET STARTING YEAR                            
*                                                                               
         SR    R1,R1                                                            
         ICM   R1,1,1(RF)          GET STARTING QUARTER                         
*                                                                               
QTRVLPQL DS    0H                                                               
*                                                                               
         LR    R0,RE               COPY YEAR                                    
*&&DO                                                                           
         CH    R0,=H'100'          MAKE 2 DIGITS IF NECESSARY                   
         BL    *+8                                                              
         SH    R0,=H'100'                                                       
*&&                                                                             
         STC   R0,0(R3)            RETURN YEAR                                  
*                                                                               
         STC   R1,1(R3)            RETURN QUARTER                               
*                                                                               
         LA    R3,2(R3)            POINT TO NEXT SLOT IN TABLE                  
         XC    0(2,R3),0(R3)       INIT NEXT SLOT IN QUARTER TABLE              
*                                                                               
QTRVLPQC DS    0H                                                               
*                                                                               
         OC    2(2,RF),2(RF)       DONE IF NOT A RANGE OF QTRS                  
         BZ    QTRVLPQD                                                         
*                                                                               
         LA    R1,1(R1)            BUMP TO NEXT QUARTER                         
*                                                                               
         CLM   R1,1,=AL1(4)        IF PAST FOURTH QTR                           
         BNH   *+12                                                             
         LA    R1,1                   SET TO FIRST                              
         LA    RE,1(RE)               BUMP TO NEXT YEAR                         
*                                                                               
         CLM   RE,1,2(RF)          CONTINUE IF NOT PAST END QTR                 
         BH    QTRVLPQD                                                         
         BL    QTRVLPQ9                                                         
*                                                                               
         CLM   R1,1,3(RF)                                                       
         BH    QTRVLPQD                                                         
*                                                                               
QTRVLPQ9 DS    0H                                                               
*                                                                               
         B     QTRVLPQL                                                         
*                                                                               
QTRVLPQD DS    0H                                                               
*                                                                               
QTRVRTNX DS    0H                                                               
         XIT1  REGS=(R3)                                                        
*                                                                               
         LTORG                                                                  
*                                                                               
         TITLE 'VALIDATE SPOT LENGTHS - SLNVAL'                                 
***********************************************************************         
*                                                                     *         
*        VALIDATE SPOT LENGTHS                                        *         
*                                                                     *         
*NTRY    P0 - 0   AL1(SLN LIST LENGTH)  0 IMPLIES SCREEN FIELD        *         
*        P0 - 1-3 AL3(SPOT LENGTHS)                                   *         
*                                                                     *         
*        P1 -     A(SPOT LENGTHS SAVEAREA LIST)                       *         
*                 ENTRY XL2(SPOT LENGTH) - BINARY                     *         
*                       X'80' INDICATES NUMBER OF MINUTES INSTEAD OF  *         
*                             SECONDS.                                *         
*                                                                     *         
*EXIT            SPOT LENGTH LIST FILLED IN AT P1 - EOT = NULL ENTRY  *         
*                                                                     *         
*       P1 - 0   AL1(NUMBER OF ENTRIES IN LIST)                       *         
*                                                                     *         
*NOTE: ROUTINE USES WORK                                              *         
*                   FLDH                                              *         
*                   FLD                                               *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
         DS    0D                                                               
SLNVAL   NTR1 LABEL=*,BASE=*                                                    
*                                                                               
         LR    R5,R1               SAVE PARAMETERS POINTER                      
*                                                                               
         L     R2,0(R5)            POINT TO SPOT LENS COMING IN                 
*                                                                               
         CLI   0(R5),0             IF LENGTH NOT GIVEN THEN SCREEN FLD          
         BE    SLNVSRX                                                          
*                                  ELSE                                         
         XC    FLDH,FLDH           CREATE DUMMY SCREEN FIELD                    
         XC    FLD,FLD                                                          
*                                                                               
         SR    RF,RF                                                            
         IC    RF,0(R5)            GET FIELD LENGTH                             
         STC   RF,FLDH+5           SET FIELD LENGTH                             
         BCTR  RF,0                DECREMENT FOR EXECUTE                        
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   FLD(0),0(R2)                                                     
*                                                                               
         LA    R2,FLDH             POINT TO DUMMY SCREEN FIELD                  
*                                                                               
SLNVSRX DS     0H                                                               
*                                                                               
         GOTO1 ANY                                                              
*                                                                               
         L     R3,4(R5)            POINT TO SPOT LENS RETURN AREA               
         XC    0(2,R3),0(R3)       INIT RETURN AREA                             
*                                                                               
         SR    R0,R0                                                            
         ICM   R0,1,5(R2)          GET INPUT LENGTH                             
         BZ    SLNVALX             NOTHING ENTERED                              
*                                                                               
         LA    R4,1                INIT FIELD COUNTER FOR ERRORS                
         SR    R6,R6               INIT SECONDS/MINUTES INDICATOR               
         LA    R1,8(R2)            POINT TO INPUT                               
         LR    RE,R1               SAVE START OF FIELD                          
*                                                                               
SLNVLOOP DS    0H                                                               
*                                                                               
         CLI   0(R1),C'0'          OKAY IF NEXT DIGIT IS NUMERIC                
         BL    SLNVLP10                                                         
         CLI   0(R1),C'9'                                                       
         BNH   SLNVCONT                                                         
*                                                                               
SLNVLP10 DS    0H                                                               
*                                                                               
         CLI   0(R1),C','          OKAY IF END OF FIELD                         
         BE    SLNVLP15                                                         
*                                                                               
         LTR   R6,R6               SKIP TO COMMA IF WE KNOW SECS/MINS           
         BNZ   SLNVCONT            (THEY MAY HAVE SPELLED 'MINS')               
*                                                                               
         CLI   0(R1),C'M'          OKAY IF MINUTES                              
         BE    *+8                                                              
         CLI   0(R1),C'S'          OKAY IF SECONDS                              
         BNE   SLNVNVE                                                          
*                                                                               
         LR    R6,R1               SAVE SECONDS/MINUTES INDICATOR               
         B     SLNVCONT            GO FIND COMMA                                
*                                                                               
SLNVLP15 DS    0H                                                               
*                                                                               
         LTR   RF,R6               POINT TO SECOND/MINUTES INDICATOR            
         BNZ   *+6                                                              
         LR    RF,R1               IF NONE USE COMMA POSITION                   
*                                                                               
         SR    RF,RE               LENGTH OF NUMBER                             
         BNP   SLNVNVE                                                          
*                                                                               
         BCTR  RF,0                DECREMENT FOR EXECUTE                        
         EX    RF,*+8                                                           
         B     *+10                                                             
         PACK  DUB,0(0,RE)         PACK SPOT LENGTH                             
         CVB   RF,DUB              CVB                                          
*                                                                               
         STCM  RF,3,0(R3)          SAVE IN TABLE                                
*                                                                               
         LTR   R6,R6               IF SECS/MIN INDICATOR PRESENT                
         BZ    SLNVLP20                                                         
*                                                                               
         CLI   0(R6),C'M'          IF MINUTES                                   
         BNE   *+8                                                              
         OI    0(R3),X'80'            FLAG                                      
*                                                                               
SLNVLP20 DS    0H                                                               
*                                                                               
         LA    R4,1(R4)            BUMP NUMBER OF ENTRIES                       
         LA    RE,1(R1)            POINT TO START OF NEXT FIELD                 
         SR    R6,R6               RESET SECS/MIN INDICATOR                     
         LA    R3,2(R3)            BUMP TABLE POINTER                           
         XC    0(2,R3),0(R3)       INIT NEXT TABLE ENTRY                        
*                                                                               
SLNVCONT DS    0H                                                               
*                                                                               
         LA    R1,1(R1)            BUMP INPUT POINTER                           
         BCT   R0,SLNVLOOP         CONTINUE ANALYSIS                            
*                                                                               
SLNVDONE DS    0H                                                               
*                                                                               
*        HANDLE LAST FIELD                                                      
*                                                                               
         LTR   RF,R6               POINT TO SECOND/MINUTES INDICATOR            
         BNZ   *+6                                                              
         LR    RF,R1               IF NONE USE COMMA POSITION                   
*                                                                               
         SR    RF,RE               LENGTH OF NUMBER                             
         BP    SLNVDN10            THERE IS A LAST SPOT LENGTH                  
*                                                                               
         LTR   R6,R6               ERROR IF SECS/MIN ONLY ENTERED               
         BNZ   SLNVNVE                                                          
*                                                                               
         B     SLNVDNX             NO LAST SPOT LENGTH                          
*                                                                               
SLNVDN10 DS    0H                                                               
*                                                                               
         BCTR  RF,0                DECREMENT FOR EXECUTE                        
         EX    RF,*+8                                                           
         B     *+10                                                             
         PACK  DUB,0(0,RE)         PACK SPOT LENGTH                             
         CVB   RF,DUB              CVB                                          
*                                                                               
         CH    RF,=H'127'          CHECK AGAINST MAX NUMBER                     
         BH    SLNVNVE                                                          
*                                                                               
         STCM  RF,3,0(R3)          SAVE IN TABLE                                
*                                                                               
         LTR   R6,R6               IF SECS/MIN INDICATOR PRESENT                
         BZ    SLNVDN20                                                         
*                                                                               
         CLI   0(R6),C'M'             IF MINUTES                                
         BNE   *+8                                                              
         OI    0(R3),X'80'               FLAG                                   
*                                                                               
SLNVDN20 DS    0H                                                               
*                                                                               
         LA    R3,2(R3)            BUMP TABLE POINTER                           
         XC    0(2,R3),0(R3)       INIT NEXT TABLE ENTRY                        
*                                                                               
SLNVDNX  DS    0H                                                               
*                                                                               
         L     RF,4(R5)            START OF TABLE                               
         SR    R3,RF               LENGTH OF TABLE                              
         SRL   R3,1                DIVIDE BY ENTRY WIDTH = 2                    
         STC   R3,4(R5)            RETURN NUMBER OF ENTRIES                     
*                                                                               
         B     SLNVALX                                                          
*                                                                               
*        INVALID SPOT LENGTH                                                    
*                                                                               
SLNVNVE DS     0H                                                               
*                                                                               
         CLI   0(R5),0             DONE IF NOT A SCREEN FIELD                   
         BNE   SLNVALX                                                          
*                                                                               
         ST    R2,FADDR            SAVE A(FIELD IN ERROR)                       
         STC   R4,FADDR            NUMBER OF FLD IN ERROR IN 1ST BYTE           
*                                                                               
         MVI   ERROR,SUPPLIED      INDICATE OUR ERROR MESSAGE                   
         MVC   CONHEAD(L'INVSLN),INVSLN INVALID SPOT LENGTH                     
         GOTO1 VMYCURS                                                          
*                                                                               
SLNVALX  DS    0H                                                               
         XIT1                                                                   
*                                                                               
INVSLN   DC    C'INVALID SPOT LENGTH.'                                          
*                                                                               
         LTORG                                                                  
*                                                                               
         TITLE 'VALIDATE RATE CODE - RTCDVAL'                                   
***********************************************************************         
*                                                                     *         
*        VALIDATE RATE CODE                                           *         
*                                                                     *         
*NTRY    P0 - 0   AL1(RATE CODE LENGTH) 0 IMPLIES SCREEN FIELD        *         
*        P0 - 1-3 AL3(RATE CODE)                                      *         
*                                                                     *         
*        P1 - 0   AL1(RATE CODE DESC LENGTH) 0 IMPLIES SCREEN FIELD   *         
*        P1 - 1-3 AL3(RATE CODE DESCRIPTION)                          *         
*                                                                     *         
*        P2 -     A(RATE CODE SAVEAREA)                               *         
*                                                                     *         
*        P3 -     A(RATE CODE YEAR/LENGTH/QUARTERS TABLE)             *         
*                 TABLE ENTRY IS AL1(YEAR) - BINARY                   *         
*                                AL1(SPOT LENGTH)  - BINARY           *         
*                                X'80' - FIRST  QUARTER               *         
*                                X'40' - SECOND QUARTER               *         
*                                X'20' - THIRD  QUARTER               *         
*                                X'10' - FOURTH QUARTER               *         
*                                                                     *         
*                                                                     *         
*EXIT            RATE CODE DESCRIPTION FIELD FILLED IN AND TRANSMITTED*         
*                RATE CODE SAVED AT P2                                *         
*                RATE CODE TABLE FILLED IN AT P3 - EOT = NULL ENTRY   *         
*                                                                     *         
*       P3 - 0   AL1(NUMBER OF ENTRIES IN TABLE)                      *         
*                                                                     *         
*NOTE: ROUTINE USES WORK                                              *         
*                   FLDH                                              *         
*                   FLD                                               *         
*                   PARAS                                             *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
         DS    0D                                                               
RTCDVAL  NTR1 LABEL=*,BASE=*                                                    
*                                                                               
         LR    R5,R1               SAVE PARAMETERS POINTER                      
         MVC   PARAS(16),0(R5)     SAVE PARAMETER LIST                          
*                                                                               
         XC    WORK,WORK           INIT WORKAREA                                
*                                                                               
         ICM   R2,15,4(R5)         POINT TO DESCRIPTION RETURN AREA             
         BZ    RTCDV10             NONE GIVEN                                   
*                                                                               
         LR    R1,R2               ASSUME A NON-SCREEN FIELD                    
*                                                                               
         SR    RF,RF                                                            
         ICM   RF,1,4(R5)          GET FIELD LENGTH                             
         BNZ   RTCDV05             NON-SCREEN FIELD                             
*                                                                               
         LA    R1,8(R2)            POINT TO SCREEN FIELD                        
*                                                                               
         ICM   RF,1,0(R2)          GET SCREEN FIELD LENGTH                      
         BZ    RTCDV10             NONE GIVEN                                   
*                                                                               
         SH    RF,=H'8'            DECREMENT FOR SCREEN HEADER                  
         TM    1(R2),X'02'         IF THERE IS AN EXTENDED HEADER               
         BNO   *+8                                                              
         SH    RF,=H'8'               DECREMENT FOR EXTENDED HEADER             
*                                                                               
         OI    6(R2),X'80'         TRANSMIT FIELD                               
*                                                                               
RTCDV05  DS    0H                                                               
*                                                                               
         ST    RF,WORK             SAVE LENGTH AND ADDRESS OF AREA              
         ST    R1,WORK+4                                                        
*                                                                               
         BCTR  RF,0                DECREMENT FOR EXECUTE                        
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R1),SPACES      INIT OUTPUT DESCRIPTION                      
*                                                                               
RTCDV10  DS    0H                                                               
*                                                                               
         L     R2,0(R5)            POINT TO RATE CODE COMING IN                 
*                                                                               
         CLI   0(R5),0             IF LENGTH NOT GIVEN THEN SCREEN FLD          
         BE    RTCDVSRX                                                         
*                                  ELSE                                         
         XC    FLDH,FLDH           CREATE DUMMY SCREEN FIELD                    
         XC    FLD,FLD                                                          
*                                                                               
         SR    RF,RF                                                            
         IC    RF,0(R5)            GET FIELD LENGTH                             
         STC   RF,FLDH+5           SET FIELD LENGTH                             
         BCTR  RF,0                DECREMENT FOR EXECUTE                        
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   FLD(0),0(R2)                                                     
*                                                                               
         LA    R2,FLDH             POINT TO DUMMY SCREEN FIELD                  
*                                                                               
RTCDVSRX DS    0H                                                               
*                                                                               
         CLI   5(R2),0             INPUT REQUIRED                               
         BE    RTCDVNEE                                                         
*                                                                               
         L     R3,8(R5)            POINT TO RATE CODE RETURN AREA               
         MVC   0(L'RARTKCOD,R3),SPACES INIT RETURN AREA                         
*                                                                               
         SR    RF,RF                                                            
         ICM   RF,1,5(R2)          GET INPUT LENGTH                             
         BZ    RTCDVALX            NOTHING ENTERED                              
         CH    RF,=Y(L'RARTKCOD)   CHECK FOR MAX LENGTH                         
         BH    RTCDVNVE                                                         
*                                                                               
         BCTR  RF,0                DECREMENT FOR EXECUTE                        
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R3),8(R2)       LEFT JUSTIFY RATE CODE                       
*                                                                               
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING RARTKEY,R4          ESTABLISH RATE CODE RECORD KEY               
*                                                                               
         MVI   RARTKTYP,X'3E'      SET RECORD TYPE                              
         MVC   RARTKREP,AGENCY     SET REP CODE                                 
         MVC   RARTKCOD,0(R3)      SET RATE CODE                                
*                                                                               
         GOTO1 HIGH                READ FOR RECORD                              
*                                                                               
         MVC   0(16,R5),PARAS      RESTORE PARAMETER LIST                       
*                                                                               
         CLC   RARTKEY,KEYSAVE     MUST FIND RECORD                             
         BNE   RTCDVNVE                                                         
*                                                                               
         GOTO1 GETREC              READ IN RECORD                               
*                                                                               
         MVC   0(16,R5),PARAS      RESTORE PARAMETER LIST                       
*                                                                               
         L     R4,AIO              POINT TO FOUND RECORD                        
*                                                                               
         SR    RF,RF                                                            
         LA    R6,RARTPEL          ESTABLISH 1ST ELM AS DESCRIPTION ELM         
*                                                                               
         CLI   RARTPCOD,0          DONE IF END OF RECORD REACHED                
         BE    RTCDVALX            NONE FOUND                                   
         CLI   RARTPCOD,X'01'      FIND DESCRIPTION ELEMENT                     
         BE    *+16                                                             
         IC    RF,RARTPLEN         GET ELEMENT LENGTH                           
         LA    R6,RARTPEL(RF)      POINT TO NEXT ELEMENT                        
         B     *-24                                                             
*                                                                               
         USING RARTPEL,R6          ESTABLISH DESCRIPTION ELEMENT                
*                                                                               
         L     RF,WORK             RESTORE DESC LENGTH AND ADDRESS              
         L     R1,WORK+4                                                        
*                                                                               
         CH    RF,=Y(L'RARTCOMM)   DEFAULT TO DESC LENGTH                       
         BNH   *+8                                                              
         LH    RF,=Y(L'RARTCOMM)                                                
*                                                                               
         BCTR  RF,0                DECREMENT FOR EXECUTE                        
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R1),RARTCOMM    RETURN DESCRIPTION                           
*                                                                               
         DROP  R6                                                               
*                                                                               
*        BUILD YEAR/LENGTH/QUARTER TABLE                                        
*                                                                               
         SR    RF,RF                                                            
         LA    R6,RARTPEL          POINT TO FIRST ELEMENT IN RECORD             
         USING RALQELEM,R6         ESTABLISH ELM AS LENGTH/QTR ELM              
*                                                                               
         ICM   R3,15,12(R5)        POINT TO TABLE RETURN AREA                   
         BZ    RTCDVQTX            NOT WANTED                                   
         XC    0(4,R3),0(R3)       INIT FIRST ELEMENT IN TABLE                  
*                                                                               
RTCDVQLP DS    0H                                                               
*                                                                               
         CLI   RALQCODE,0          DONE IF END OF RECORD REACHED                
         BE    RTCDVQDN                                                         
*                                                                               
         CLI   RALQCODE,X'02'      FIND LENGTH/QTR ELEMENT                      
         BNE   RTCDVQCN                                                         
*                                                                               
         MVC   0(4,R3),RALQYEAR    RETURN YEAR/LENGTH/QUARTERS                  
         LA    R3,4(R3)            BUMP TO NEXT ENTRY IN TABLE                  
         XC    0(4,R3),0(R3)       INIT                                         
*                                                                               
RTCDVQCN DS    0H                                                               
*                                                                               
         IC    RF,RALQLN           GET ELEMENT LENGTH                           
         LA    R6,RALQELEM(RF)     POINT TO NEXT ELEMENT                        
         B     RTCDVQLP                                                         
*                                                                               
RTCDVQDN DS    0H                                                               
*                                                                               
         L     RF,12(R5)           START OF TABLE                               
         SR    R3,RF               LENGTH OF TABLE                              
         SR    RE,RE                                                            
         LR    RF,R3                                                            
         D     RE,=F'4'            GET NUMBER OF ENTRIES IN TABLE               
         STC   RF,12(R5)           RETURN NUMBER OF ENTRIES                     
*                                                                               
RTCDVQTX DS    0H                                                               
*                                                                               
         B     RTCDVALX                                                         
*                                                                               
         DROP  R6                                                               
*                                                                               
*        INVALID RATE CODE                                                      
*                                                                               
RTCDVNEE DS    0H                                                               
*                                                                               
         CLI   0(R5),0             DONE IF NOT A SCREEN FIELD                   
         BNE   RTCDVALX                                                         
*                                                                               
         ST    R2,FADDR            SAVE A(FIELD IN ERROR)                       
         MVI   FADDR,1             NUMBER OF FLD IN ERROR IN 1ST BYTE           
*                                                                               
         MVI   ERROR,MISSING       INPUT REQUIRED                               
*                                                                               
         GOTO1 VMYCURS                                                          
*                                                                               
RTCDVNVE DS    0H                                                               
*                                                                               
         CLI   0(R5),0             DONE IF NOT A SCREEN FIELD                   
         BNE   RTCDVALX                                                         
*                                                                               
         ST    R2,FADDR            SAVE A(FIELD IN ERROR)                       
         MVI   FADDR,1             NUMBER OF FLD IN ERROR IN 1ST BYTE           
*                                                                               
         MVI   ERROR,SUPPLIED      INDICATE OUR ERROR MESSAGE                   
         MVC   CONHEAD(L'INVRTCD),INVRTCD INVALID RATE CODE                     
         GOTO1 VMYCURS                                                          
*                                                                               
RTCDVALX DS    0H                                                               
         XIT1                                                                   
*                                                                               
INVRTCD  DC    C'RATE CODE NOT ON FILE.'                                        
*                                                                               
         LTORG                                                                  
*                                                                               
***********************************************************************         
*                                                                               
* 2-CHARACTER BOOKTYPES DISPLAY SUPPORT                                         
* INPUT PARAM 1 = HIGH-ORD BYTE INTERNAL BOOKTYPE                               
* OUTPUT PARM 1 = HIGH-ORD LENGTH OF OUTPUT BOOK, BYTE3-4 BOOKTYPE              
*                                                                               
***********************************************************************         
         SPACE 2                                                                
         DS    0D                                                               
GETBKTP  NTR1 LABEL=*,BASE=*                                                    
         MVC   BYTE,DMCB                                                        
         CLI   BYTE,0                                                           
         BNE   GTBK10                                                           
         XC    DMCB,DMCB                                                        
         B     GTBKX                                                            
*                                                                               
GTBK10   DS    0H                                                               
         L     R3,ACOMFACS                                                      
         SPACE                                                                  
         L     RF,CDEMTABS-COMFACSD(,R3)                                        
         GOTO1 (RF),DMCB,SPBOOKTB                                               
         ICM   RF,15,0(R1)         A(TABLE) RETURNED IN P1                      
         BNZ   *+6                                                              
         DC    H'0'                                                             
         L     R0,4(R1)            L'TABLE ENTRY RETURNED IN P2                 
         USING SPBKTYPD,RF                                                      
GTBK20   DS    0H                                                               
         CLI   0(RF),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLC   BYTE,SPBKTYPN                                                    
         BE    *+10                                                             
         AR    RF,R0                                                            
         B     GTBK20                                                           
*                                                                               
         XC    DMCB,DMCB                                                        
         MVC   DMCB+2(2),SPBKTYPA    BOOK TYPE                                  
         MVI   DMCB,1                                                           
         CLI   SPBKTYPA+1,0                                                     
         BE    GTBKX                                                            
         CLI   SPBKTYPA+1,C' '                                                  
         BE    GTBKX                                                            
         MVI   DMCB,2                                                           
         DROP  RF                                                               
         EJECT                                                                  
GTBKX    DS    0H                                                               
         XIT1                                                                   
         LTORG                                                                  
       ++INCLUDE RERMPPROF                                                      
       ++INCLUDE RERESWRK                                                       
         EJECT                                                                  
* CTGENFILE - DSECT TO COVER TWX ADDRESSEE RECORDS AND A WHOLE                  
* LOT MORE                                                                      
         PRINT OFF                                                              
       ++INCLUDE CTGENFILE                                                      
         PRINT ON                                                               
         SPACE 2                                                                
* DDCOMFACS - COMMON FACILITIES DSECT                                           
*                                                                               
         PRINT OFF                                                              
       ++INCLUDE DDCOMFACS                                                      
         PRINT ON                                                               
         SPACE 2                                                                
*                                                                               
* FAFACTS - SYSTEM INFO BLOCK                                                   
*                                                                               
         PRINT OFF                                                              
       ++INCLUDE FAFACTS                                                        
         PRINT ON                                                               
         SPACE 2                                                                
* FATIOB -  TRANSLATOR I/O BLOCK                                                
*                                                                               
         PRINT OFF                                                              
       ++INCLUDE FATIOB                                                         
         PRINT ON                                                               
         SPACE                                                                  
* DDGLOBEQUS - GLOBBER EQUATES                                                  
*                                                                               
         PRINT OFF                                                              
       ++INCLUDE DDGLOBEQUS                                                     
         PRINT ON                                                               
* DDGLVXCTLD - GLOBBER XCTL DSECT                                               
*                                                                               
         PRINT OFF                                                              
       ++INCLUDE DDGLVXCTLD                                                     
         PRINT ON                                                               
* DEDEMTABD                                                                     
*                                                                               
         PRINT OFF                                                              
       ++INCLUDE DEDEMTABD                                                      
         PRINT ON                                                               
         EJECT                                                                  
*                                                                               
*        DEMO RECORD IUN DSECT FOR USE BY FIXPAV                                
*                                                                               
IUNREC   DSECT                                                                  
UPREC    DS    0F                                                               
***********************************************************************         
*                                  ORIGINAL BOOK VALUES               *         
***********************************************************************         
OLDUNV   DS    (NUMVALS)F          UNIVERSES                          *         
OLDUNVX  EQU   *                                                      *         
***********************************************************************         
OLDRTG   DS    (NUMVALS)F          RATINGS                            *         
         ORG   OLDRTG+(DISPHOM*4)                                               
UORHOMES DS    F                                                      *         
         ORG                                                                    
OLDIMP   DS    (NUMVALS)F          IMPRESSIONS                        *         
OLDRTGX  EQU   *                                                      *         
***********************************************************************         
OLDHPT   DS    (NUMVALS)F          HUTS/PUTS                          *         
         ORG   OLDHPT+(DISPHOM*4)                                               
UOPHOMES DS    F                                                      *         
         ORG                                                                    
OLDTOT   DS    (NUMVALS)F          TSA TOTALS                         *         
         ORG   OLDTOT+(DISPHOM*4)                                               
UOQHOMES DS    F                                                      *         
         ORG                                                                    
OLDHPTX  EQU   *                                                      *         
***********************************************************************         
*                                  NEW VALUES                         *         
NEWUNV   EQU   OLDTOT              DEFINE ORIGIN FOR REGETIUN CALL    *         
*                                                                     *         
***********************************************************************         
NEWRTG   DS    (NUMVALS)F          RATINGS                            *         
         ORG   NEWRTG+(DISPHOM*4)                                               
UNRHOMES DS    F                                                      *         
         ORG                                                                    
NEWIMP   DS    (NUMVALS)F          IMPRESSIONS                        *         
NEWRTGX  EQU   *                                                      *         
***********************************************************************         
NEWHPT   DS    (NUMVALS)F          HUTS/PUTS                          *         
         ORG   NEWHPT+(DISPHOM*4)                                               
UNPHOMES DS    F                                                      *         
         ORG                                                                    
NEWTOT   DS    (NUMVALS)F          TSA TOTALS                         *         
NEWHPTX  EQU   *                                                      *         
***********************************************************************         
*                                  OTHER VALUES                       *         
***********************************************************************         
HOMSHR   DS    3F                  ORIGINAL HOMES SHARES              *         
HOMSHRX  EQU   *                                                      *         
HOMSHRLN EQU   *-HOMSHR                                               *         
***********************************************************************         
LUNV     DS    (NUMVALS)F          LOONEYVERSES                       *         
LUNVX    EQU   *                                                      *         
***********************************************************************         
UPRECX   DS    0F                                                               
*                                                                               
NUMVALS  EQU   32                                                               
DISPHOM  EQU   20                                                               
LENVALS  EQU   NUMVALS*4                                                        
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'020RERES00   04/15/09'                                      
         END                                                                    
