*          DATA SET CTSF11A    AT LEVEL 133 AS OF 05/01/02                      
*PHASE TA0A11A                                                                  
*                                                                               
***********************************************************************         
*                                                                     *         
*  TITLE: TA0A11 - SOON MAINTENANCE                                   *         
*                                                                     *         
*  CALLED FROM: SFM CONTROLLER (TA0A00), WHICH CALLS                  *         
*               DDGENCON (T00A30) WHICH CALLS THIS.                   *         
*                                                                     *         
*  INPUTS: SCREENS CTSFMFE1 (TA0AE1) -- MAINTENANCE                   *         
*                                                                     *         
*  OUTPUTS: UPDATED SOON RECORDS                                      *         
*                                                                     *         
*  LOCALS: REGISTER USAGE                                             *         
*          R0 - WORK                                                  *         
*          R1 - WORK                                                  *         
*          R2 - POINTER TO SCREEN FLDH, ERREX WILL POSITION CURSOR    *         
*          R3 - GETEL REGISTER                                        *         
*          R4 - BROADCAST MESSAGE RECORD                              *         
*          R5 - WORK                                                  *         
*          R6 - WORK                                                  *         
*          R7 - WORK                                                  *         
*          R8 - SECOND BASE                                           *         
*          R9 - SYSD                                                  *         
*          RA - TWA                                                   *         
*          RB - FIRST BASE                                            *         
*          RC - GEND                                                  *         
*          RD - SYSTEM                                                *         
*          RE - SYSTEM                                                *         
*          RF - SYSTEM                                                *         
*                                                                     *         
*                                                                               
         TITLE 'TA0A11 SOON MAINTENANCE'                                        
TA0A11   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,*TA0A11*,R8                                                    
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA      BASE SCREEN FOR SYSTEM + THIS PROG           
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
*                                                                               
*                                                                               
         GOTO1 GETFACT,DMCB,0      GET A(FACTSD)                                
         L     R1,0(R1)                                                         
         USING FACTSD,R1                                                        
         MVC   ASYSLST,FASYSLST    A(TABLE OF FACPAK SYSTEM)                    
         DROP  R1                                                               
*                                                                               
         CLI   MODE,VALKEY         VALIDATE KEY                                 
         BNE   *+8                                                              
         BAS   RE,VK                                                            
         CLI   MODE,VALREC         VALIDATE RECORD                              
         BNE   *+12                                                             
         BAS   RE,VR                                                            
         BAS   RE,DR                                                            
         CLI   MODE,DISPREC        DISPLAY RECORD                               
         BNE   *+8                                                              
         BAS   RE,DR                                                            
*        CLI   MODE,XRECADD        AFTER ADDREC                                 
*        BE    XRA                                                              
*        CLI   MODE,XRECPUT        AFTER PUTREC                                 
*        BE    XRP                                                              
         CLI   MODE,DISPKEY        DISPLAY KEY                                  
         BNE   *+8                                                              
         BAS   RE,DK                                                            
         CLI   MODE,LISTRECS       ONLINE LIST RECORDS                          
         BNE   *+8                                                              
         BAS   RE,LR                                                            
*                                                                               
         XIT1                                                                   
         EJECT                                                                  
* VALIDATE KEY ROUTINE                                                          
*                                                                               
VK       NTR1                                                                   
         LA    R4,KEY                                                           
         USING SOONKEYD,R4                                                      
*                                                                               
         LA    R2,SFMSYSH          REQUEST SYSTEM                               
         CLI   ACTNUM,ACTLIST      ACTION LIST?                                 
         BNE   NOTLIST                                                          
         LA    R2,SFLSYSH          DSECT FOR LIST SCREEN                        
*                                                                               
NOTLIST  CLI   5(R2),0             ANY DATA?                                    
         BNE   *+14                                                             
         MVC   GERROR,=AL2(MISSING)                                             
         B     SFMERROR            REQUIRED                                     
*                                                                               
         MVC   SYSTYPE,8(R2)       REQUEST SYSTEM                               
         L     R5,ASYSLST                                                       
         USING SYSLSTD,R5                                                       
         LH    R6,0(R5)            LENGTH OF TABLE ENTRY                        
         L     R7,2(R5)            A(END OF TABLE)                              
         LA    R5,6(R5)            A(FIRST ENTRY)                               
         CLC   SYSLRPLT,SYSTYPE    VALID SYSTEM MATCH?                          
         BE    VK10                                                             
         BXLE  R5,R6,*-10          TRY AGAIN FOR MATCH                          
         MVC   GERROR,=AL2(INVSYS)                                              
         B     SFMERROR            INVALID MESSAGE TYPE                         
*                                                                               
VK10     CLI   ACTNUM,ACTLIST      IS ACTION A LIST                             
         BNE   VKPRGM                                                           
         LA    R2,SFLPGMH                                                       
         MVC   PROGRAM,8(R2)                                                    
         LA    R2,SFLMODH                                                       
         ZIC   R5,5(R2)                                                         
         ST    R5,MODCOUNT       STORE NO. OF CHARS.                            
         MVC   MODULE,8(R2)      LOAD MODULE INPUT                              
         B     VKEND                                                            
VKPRGM   LA    R2,SFMPGMH          PROGRAM                                      
*                                                                               
         CLI   5(R2),0             REQUIRED                                     
         BNE   *+14                                                             
         MVC   GERROR,=AL2(MISSING)                                             
         B     SFMERROR                                                         
*                                                                               
* TEST FOR 2 ALPHANUMERICS IF NOT A LIST                                        
         CLI   5(R2),2             IS LENGTH 2                                  
         BE    VKX                                                              
         MVC   GERROR,=AL2(INVPROG)                                             
         B     SFMERROR                                                         
*                                                                               
* BUILD THE KEY                                                                 
*                                                                               
VKX      XC    KEY,KEY             BUILD KEY                                    
         MVI   SOONKTYP,SOONKTY2   SOON TYPE                                    
         MVC   SOONKSYS,SYSTYPE    SYSTEM TYPE                                  
         MVC   SOONKPRG,SFMPGM     PROGRAM TYPE                                 
VKEND    XIT1                                                                   
         EJECT                                                                  
*                                                                               
* CODE TO VALIDATE RECORD                                                       
*                                                                               
VR       NTR1                                                                   
         L     R4,AIO             ADDRESS OF SOON RECORD                        
         USING SOONKEY,R4                                                       
         MVI   ELCODE,SNDSCCDQ    DESCRIPTION ELEMENT                           
         GOTO1 REMELEM            REMOVES ELEMENT                               
         XC    ELEM,ELEM                                                        
         LA    R3,ELEM                                                          
         USING SNDSCD,R3                                                        
         MVI   SNDSCCD,SNDSCCDQ   ELEMENT CODE 10                               
         LA    R2,SFMDESCH        DESCRIPTION FIELD                             
         CLI   5(R2),0            ANY DESCRIPTION INPUT                         
         BNE   *+14                                                             
         MVC   GERROR,=AL2(MISSING)                                             
         B     SFMERROR                                                         
         ZIC   R1,5(R2)           GET LENGTH OF INPUT                           
         BCTR  1,R0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   SNDSCTXT(0),SFMDESC  DESCRIPTION TEXT LINE                       
         LA    R6,SNDSCOV         OVERHEAD LENGTH                               
         LA    R1,1(R6,R1)        TOTAL LENGTH OF ELEMENT                       
         STC   R1,SNDSCLEN        ELEMENT OVERHEAD LENGTH                       
         GOTO1 ADDELEM            ADD THE DESCRIPTION ELEMENT                   
         CLI   DMCB+12,0                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
*  REMOVE AND REBUILD THE ATTRBIUTES ELEMENT                                    
VR10     MVI   ELCODE,SNATTCDQ                                                  
         GOTO1 REMELEM            REMOVE ELEMENT 20                             
         LA    R2,SFMMODH         MOD NAME HEADER                               
         CLI   5(R2),0            ANY MOD NAME                                  
         BNE   *+14                                                             
         MVC   GERROR,=AL2(MISSING)                                             
         B     SFMERROR                                                         
         XC    ELEM,ELEM                                                        
         LA    R3,ELEM                                                          
         USING SNATTCD,R3                                                       
         MVI   SNATTCD,SNATTCDQ   ELEMENT CODE OF 20                            
         MVC   SNATTLM,SFMMOD     LOAD MOD NAME                                 
         LA    R2,SFMSECH         TIME LIMIT IN SECONDS                         
         CLI   5(R2),0            ANY TIME INPUT                                
         BNE   *+14                                                             
         MVC   GERROR,=AL2(MISSING)                                             
         B     SFMERROR                                                         
         TM    4(R2),X'08'         NUMERIC?                                     
         BO    *+14                YES                                          
         MVC   GERROR,=AL2(NOTNUM)                                              
         B     SFMERROR                                                         
         ZIC   R5,5(R2)            LENGTH OF SECONDS                            
         BCTR  R5,0                                                             
         EX    R5,*+8                                                           
         B     *+10                                                             
         PACK  DUB,8(0,R2)                                                      
         CVB   R5,DUB                                                           
         STCM  R5,15,SNATTTIM         TIME LIMIT IN SECONDS                     
         LA    R2,SFMPRTYH         REQUEST PRIORITY                             
         CLI   5(R2),0             ANY PRIORITY                                 
         BNE   *+14                                                             
         MVC   GERROR,=AL2(MISSING)                                             
         B     SFMERROR                                                         
         TM    4(R2),X'08'         NUMERIC?                                     
         BO    *+14                YES                                          
         MVC   GERROR,=AL2(NOTNUM)                                              
         B     SFMERROR                                                         
         MVC   GERROR,=AL2(INVALID)                                             
         MVC   SNATTPRT,SFMPRTY                                                 
         MVI   SNATTLEN,SNATTLNQ   ELEMENT LENGTH                               
         GOTO1 ADDELEM            ADD THE ATTRIBUTES ELEMENT                    
         CLI   DMCB+12,0                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
* CONTROL CARD ELEMENTS - COULD BE ANY AMOUNT                                   
         MVI   TXTFOUND,C'N'         NO TEXT FOUND YET                          
         MVI   SEQNUM,0                                                         
         MVI   ELCODE,SNCRDCDQ      ELEMENT CODE OF 30                          
         GOTO1 REMELEM                                                          
         LA    R3,ELEM                                                          
         USING SNCRDCD,R3          CONTROL CARD ELEMENT                         
         LA    R2,SFMCC1H          1ST CONTROL CARD INPUT                       
VRLOOP   LA    RF,SFMENDH          END OF SCREEN                                
         CR    R2,RF               END OF SCREEN?                               
         BNL   VRTXTE                                                           
         XC    ELEM,ELEM                                                        
         ZIC   RF,SEQNUM          INCREMENT SEQ NUMBER                          
         LA    RF,1(RF)                                                         
         STC   RF,SEQNUM                                                        
         MVI   SNCRDCD,SNCRDCDQ   ELEMENT CODE 30                               
         MVC   SNCRDSEQ,SEQNUM                                                  
         CLI   5(R2),0            LENGTH OF INPUT                               
         BE    VRTXTE             NO                                            
         MVI   TXTFOUND,C'Y'       SOME TEXT WAS FOUND                          
         ZIC   R1,5(R2)           GET LENGTH OF INPUT                           
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   SNCRDTXT(0),8(R2)  LINE OF TEXT                                  
         LA    R6,SNCRDOV         OVERHEAD LENGTH                               
         LA    R1,1(R6,R1)        TOTAL LENGTH OF ELEMENT                       
         STC   R1,SNCRDLEN        STORE TEXTLEN                                 
         GOTO1 ADDELEM            ADD THE ATTRIBUTES ELEMENT                    
         CLI   DMCB+12,0                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
* BUMP TO NEXT FIELD                                                            
         ZIC   R0,0(R2)            LENGTH OF CARD FIELD                         
         AR    R2,R0               NOW AT SEQUENCE FIELD                        
         ZIC   R0,0(R2)            LENGTH OF SEQUENCE FIELD                     
         AR    R2,R0               NOW AT NEXT CARD FIELD                       
         B     VRLOOP                                                           
VRTXTE   CLI   TXTFOUND,C'Y'       WAS ANY TEXT FOUND                           
         BE    *+14                                                             
         MVC   GERROR,=AL2(NOTEXT)                                              
         B     SFMERROR                                                         
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
* DISPLAY THE KEY                                                               
DK       NTR1                                                                   
         L     R4,AIO                                                           
         USING SOONKEY,R4                                                       
         MVC   SFMSYS,SOONKSYS      SYSTEM                                      
         OI    SFMSYSH+6,X'80'                                                  
         MVC   SFMPGM,SOONKPRG      PROGRAM                                     
         OI    SFMPGMH+6,X'80'                                                  
         DROP  R4                                                               
         XIT1                                                                   
*                                                                               
* DISPLAY RECORD                                                                
         EJECT                                                                  
*                                                                               
* CLEAR OUT FIELDS SO OLD INPUT IS NOT LEFT ON SCREEN                           
DR       NTR1                                                                   
         LA    R7,SFMDESCH                                                      
         TWAXC (R7)                                                             
         LA    R7,SFMSEQH                                                       
         TWAXC (R7),PROT=Y      CLEAR OLD SEQUENCE NUMBERS                      
         L     R3,AIO                                                           
         USING SNDSCCD,R3                                                       
         MVI   ELCODE,SNDSCCDQ                                                  
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         ZIC   R1,SNDSCLEN         LENGTH OF ELEMENT                            
         LA    R6,SNDSCOV          OVERHEAD LENGTH                              
         SR    R1,R6                                                            
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   SFMDESC(0),SNDSCTXT     DESCRIPTION                              
         OI    SFMDESCH+6,X'80'                                                 
         DROP  R3                                                               
*                                                                               
* ATTRIBUTES ELEMENT                                                            
         L     R3,AIO                                                           
         USING SNATTD,R3                                                        
         MVI   ELCODE,SNATTCDQ  ELEMENT CODE 20                                 
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   SFMMOD,SNATTLM           LOAD MODULE NAME                        
         OI    SFMMODH+6,X'80'                                                  
         EDIT  SNATTTIM,(5,SFMSEC),ALIGN=LEFT     SECONDS                       
         OI    SFMSECH+6,X'80'                                                  
         MVC   SFMPRTY,SNATTPRT      PRIORITY                                   
         OI    SFMPRTYH+6,X'80'                                                 
         DROP  R3                                                               
*                                                                               
* CONTROL CARD ELEMENTS                                                         
         L     R3,AIO             A(FIRST ELEMENT)                              
         USING SNCRDD,R3                                                        
         LA    R2,SFMCC1H         FIRST CONTROL CARD FIELD                      
         MVI   ELCODE,SNCRDCDQ    ELEMENT CODE 30                               
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
* FILL IN TEXT FROM CONTROL CARD AND SEQUENCE NUMBERS                           
DR100    ZIC   R1,SNCRDLEN         LENGTH OF ELEMENT                            
         LA    R6,SNCRDOV          OVERHEAD LENGTH                              
         SR    R1,R6               LENGTH OF CONTROL CARD TEXT                  
         BCTR  R1,0                                                             
         LTR   R1,R1               IS THERE CARD INPUT                          
         BZ    ENDCARD             SHOULD BE CARD INPUT                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   8(0,R2),SNCRDTXT    TEXT LINE                                    
         OI    6(R2),X'80'                                                      
*                                                                               
* FILL IN SEQUENCE LINE                                                         
         ZIC   R0,0(R2)           NEXT FIELD - SEQUENCE NO.                     
         AR    R2,R0              SEQUENCE NUMBER FIELD                         
         LR    R4,R2              STORE SEQUENCE ADDRESS                        
         LA    R4,8(R4)           SEQUENCE NUMBER LINE                          
         EDIT  SNCRDSEQ,(3,(R4)),ALIGN=LEFT     SECONDS                         
         OI    6(R2),X'80'                                                      
         DROP  R3                                                               
*                                                                               
* GET NEXT TEXT FIELDS                                                          
         ZIC   R0,0(R2)            NEXT TEXT FIELD                              
         AR    R2,R0          ADD FIELD LENGTH TO GET TO CONTROL FIELD          
         LA    RF,SFMENDH          PF KEY LINE                                  
         CR    R2,RF               END OF SCREEN                                
         BNL   ENDCARD                                                          
         BAS   RE,NEXTEL           GET NEXT CONTROL CARD                        
         BE    DR100                                                            
ENDCARD  XIT1                                                                   
         EJECT                                                                  
*                                                                               
* ONLINE LIST ROUTINE                                                           
LR       NTR1                                                                   
         LA    R4,KEY                                                           
         USING SOONKEY,R4                                                       
         OC    KEY(SOONKLNQ),KEY   IS IT FIRST TIME THROUGH?                    
         BNZ   LR10                NO                                           
*                                                                               
         MVI   SOONKTYP,SOONKTY2   TYPE 32                                      
         MVC   SOONKSYS,SYSTYPE    SYSTEM TYPE                                  
         MVC   SOONKPRG,PROGRAM    PROGRAM REQUESTED                            
         MVC   SAVEKEY,KEY                                                      
*                                                                               
LR10     GOTO1 HIGH                FIRST RECORD                                 
*                                                                               
LR20     CLC   KEY(10),SAVEKEY   ANY MORE RECORDS OF SAME TYPE + SYS            
         BNE   LREND                                                            
         MVC   LISTAR,MYSPACES   CLEAR OUTPUT LINE                              
         GOTO1 GETREC                                                           
         L     R4,AIO            SOON RECORD                                    
*                                                                               
         MVC   LSTPGM,SOONKPRG                                                  
         LA    R3,SOON1STL       ADDR OF FIRST ELEMENT                          
         USING SNDSCCD,R3        DESCRIPTION  ELEMENT                           
         MVI   ELCODE,SNDSCCDQ   ELEMENT CODE 10                                
         BAS   RE,FIRSTEL                                                       
         BE    *+6                                                              
         DC    H'0'                                                             
         ZIC   R1,SNDSCLEN         LENGTH OF ELEMENT                            
         LA    R6,SNDSCOV          OVERHEAD LENGTH                              
         SR    R1,R6                                                            
         C     R1,=F'40'           IS DESCR. GREATER THAN 40 CHARS?             
         BE    LR40                                                             
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   LSTDESCR(0),SNDSCTXT     DESCRIPTION                             
         B     *+10                                                             
LR40     MVC   LSTDESCR,SNDSCTXT       TRUNCATED MOVE                           
*                                                                               
*  ATTRIBUTES ELEMENT                                                           
         LA    R3,SOON1STL                                                      
         USING SNATTD,R3                                                        
         MVI   ELCODE,SNATTCDQ         ELEMENT CODE 20                          
         BAS   RE,FIRSTEL              MUST BE THERE                            
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   LSTMOD,SNATTLM      MODULE NAME TO OUPUT                         
         L     R1,MODCOUNT                                                      
         C     R1,=F'0'            DID USER ENTER MODULE INPUT?                 
         BE    LR30                                                             
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   SNATTLM(0),MODULE    IS THIS THE MODULE THE USER WANTS?          
         BNE   LR100             DID NOT PASS FILTER TEST                       
LR30     MVC   LSTPRTY,SNATTPRT  MOVE PRIORITY TO OUTPUT                        
         EDIT  SNATTTIM,(5,LSTTIME),ALIGN=LEFT     SECONDS                      
         GOTO1 LISTMON           SEND OUPUT TO SCREEN                           
*                                                                               
LR100    GOTO1 SEQ               NEXT RECORD                                    
         LA    R4,KEY            POINT R4 BACK TO KEY                           
         B     LR20                                                             
         DROP  R4                                                               
LREND    XIT1                                                                   
         EJECT                                                                  
SFMERROR GOTO1 SFMERR                                                           
         SPACE 3                                                                
         GETEL R3,42,ELCODE                                                     
         SPACE 3                                                                
MYSPACES DC    CL132' '                                                         
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
       ++INCLUDE FASYSLSTD                                                      
         EJECT                                                                  
       ++INCLUDE CTGENSOON              DSECT FOR SOON RECORDS                  
         EJECT                                                                  
       ++INCLUDE DDSPOOLD               SPOOL DSECT                             
         PRINT OFF                                                              
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE FACTRY                                                         
       ++INCLUDE FASELIST                                                       
       ++INCLUDE FASYSFAC                                                       
       ++INCLUDE FAFACTS                                                        
       ++INCLUDE FAPGMLST                                                       
       ++INCLUDE DDPERVALD                                                      
       ++INCLUDE DDCOMFACS                                                      
         EJECT                                                                  
       ++INCLUDE CTSFMFFD                                                       
         PRINT ON                                                               
         ORG   CONTAGH                                                          
         EJECT                                                                  
       ++INCLUDE CTSFME1D                                                       
         ORG   CONTAGH                                                          
         EJECT                                                                  
       ++INCLUDE CTSFME6D                                                       
         EJECT                                                                  
       ++INCLUDE CTSFMWORKD                                                     
         SPACE 5                                                                
* PUT MY STORAGE DSECT HERE IF NEEDED                                           
*                                                                               
         ORG   SYSSPARE                                                         
SYSTYPE  DS    X                   SYSTEM REQUESTED                             
MODULE   DS    CL8                 LOAD MOD NAME FILTER                         
PROGRAM  DS    CL2                 PROGRAM NAME                                 
MODCOUNT DS    F                   COUNT OF MODULE INPUT CHARS                  
ASELIST  DS    A                   A(SELIST TABLE)                              
ASYSLST  DS    A                   A(SYSLST TABLE)                              
ASEPGMS  DS    A                   A(PROGRAMS TABLE)                            
MSGNUM   DS    H                   MESSAGE NUMBER                               
SAVEKEY  DS    XL32                BROADCAST MESSAGE KEY                        
SEQNUM   DS    XL1                 TEXT LINE NUMBER                             
TXTFOUND DS    CL1                 'Y' IF TEXT LINE WAS FOUND                   
MYWORK   DS    CL17                WORK FOR EDIT                                
*                                                                               
         CNOP  0,4                                                              
COMTABL  DS    0AL4                SCREDIT COMMAND AREAS                        
ACOMI    DS    A(0)                                                             
ACOMD    DS    A(0)                                                             
ACOMM    DS    A(0)                                                             
ACOMB    DS    A(0)                                                             
ACOMA    DS    A(0)                                                             
ACOMO    DS    A(0)                                                             
ACOMC    DS    A(0)                                                             
COMTABX  EQU   *                                                                
         SPACE 5                                                                
* ONLINE LIST LINE                                                              
*                                                                               
GEND     DSECT                                                                  
         ORG   LISTAR                                                           
LSTPGM   DS    CL2                                                              
         DS    CL2                                                              
LSTDESCR DS    CL40                                                             
         DS    CL2                                                              
LSTMOD   DS    CL8                                                              
         DS    CL2                                                              
LSTTIME  DS    CL5                                                              
         DS    CL2                                                              
LSTPRTY  DS    CL1                                                              
         DS    CL1                                                              
         SPACE 3                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'133CTSF11A   05/01/02'                                      
         END                                                                    
