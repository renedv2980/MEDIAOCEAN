*          DATA SET CTSFM57    AT LEVEL 060 AS OF 05/01/02                      
*PHASE TA0A57A                                                                  
*INCLUDE SORTER                                                                 
*                                                                               
***********************************************************************         
*                                                                     *         
*  TITLE:        CTSFM57 -- SCROLLER DATATYPE MAINTENANCE/LIST/REPORT *         
*                                                                     *         
*  COMMENTS:     MAINTAINS SCROLLER DATATYPE RECORDS ON GENDIR/GENFIL *         
*                                                                     *         
*  CALLED FROM:  SFM CONTROLLER (TA0A00), WHICH CALLS                 *         
*                GEGENCON (T00A30), WHICH CALLS THIS.                 *         
*                                                                     *         
*  INPUTS:       SCREENS CTSFMA7 (MAINTENANCE)                        *         
*                        CTSFMB7 (LIST)                               *         
*                        CTSFMC9 (REPORT)                             *         
*                                                                     *         
*  OUTPUTS:      UPDATED DATATYPE RECORDS, LIST, OR REPORT.           *         
*                                                                     *         
*  REGISTERS:    R0 -- WORK                                           *         
*                R1 -- WORK                                           *         
*                R2 -- SCREEN FIELD HEADER                            *         
*                R3 -- WORK                                           *         
*                R4 -- WORK                                           *         
*                R5 -- WORK                                           *         
*                R6 -- GETEL REGISTER                                 *         
*                R7 -- SECOND BASE                                    *         
*                R8 -- SPOOLD                                         *         
*                R9 -- SYSD                                           *         
*                RA -- TWA                                            *         
*                RB -- FIRST BASE                                     *         
*                RC -- GEND                                           *         
*                RD -- SYSTEM                                         *         
*                RE -- SYSTEM                                         *         
*                RF -- SYSTEM                                         *         
*                                                                     *         
***********************************************************************         
         TITLE 'TA0A57 - SCROLLER DATATYPE RECORD MAINT/LIST/REPORT'            
TA0A57   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**0A57**,R7,RR=R3                                              
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     R8,ASPOOLD          GENERAL PRINT AREAS                          
         USING SPOOLD,R8                                                        
         L     RA,ATWA                                                          
         USING TA0AFFD,RA          BASE SCREEN FOR SYSTEM + THIS PROG           
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         ST    R3,RELO                                                          
*                                                                               
         CLI   MODE,VALKEY         VALIDATE RECORD KEY                          
         BE    VK                                                               
         CLI   MODE,VALREC         VALIDATE RECORD                              
         BE    VR                                                               
         CLI   MODE,DISPKEY        DISPLAY KEY                                  
         BE    DK                                                               
         CLI   MODE,LISTRECS       LIST RECORDS                                 
         BE    LR                                                               
         CLI   MODE,PRINTREP       PRINT RECORDS                                
         BE    PR                                                               
         CLI   MODE,XRECPUT        RECORD WAS CHANGED,DISPLAY IT                
         BE    DR                                                               
         CLI   MODE,XRECADD        RECORD WAS ADDED, DISPLAY IT                 
         BE    DR                                                               
         CLI   MODE,XRECDEL        IF THIS IS A DELETE, DO DISP                 
         BE    DR                                                               
         CLI   THISLSEL,C'D'       IF DELETE FROM LIST, SKIP DISP               
         BE    XIT                                                              
         CLI   MODE,DISPREC        DISPLAY RECORD                               
         BE    DR                                                               
*                                                                               
XIT      XIT1                                                                   
         EJECT                                                                  
* ********************************************************************          
* VALIDATE KEY - PMT=MAINT SCREEN      PLS=LIST SCREEN                          
* ********************************************************************          
*                                                                               
VK       CLI   ACTEQU,ACTREP       REPORT ACTION?                               
         BE    VKRPT15                                                          
*                                                                               
         CLI   ACTEQU,ACTLIST      LIST ACTION?                                 
         BE    VKLST20                                                          
*                                                                               
*---------------------------------------------------------------------          
*VALIDATION OF KEY: FOR MAINTENANCE SCREEN (PMT..)                              
*---------------------------------------------------------------------          
         OI    PMTSYSMH+6,X'80'     RETRANSMIT FIELD                            
         OI    PMTPROGH+6,X'80'     RETRANSMIT FIELD                            
*                                                                               
         LA    R2,PMTSYSMH           VALIDATE SYSTEM FIELD                      
         GOTO1 ANY                   SYSTEM FIELD REQUIRED                      
         BAS   RE,FAKVAL             VALIDATE SYSTEM AGAINST FACPK TBL          
         BZ    *+14                  VALID INPUT                                
         MVC   GERROR,=AL2(INVSYS)                                              
         B     VSFMERR                                                          
         MVC   8(L'SYSNAM,R2),SYSNAM                                            
*                                                                               
         LA    R2,PMTPROGH           PROGRAM NAME REQUIRED                      
         GOTO1 ANY                   FIELD REQUIRED                             
         CLI   PMTPROGH+5,3          WAS 'ALL' ENTERED?                         
         BNE   VKMNT10               NOT ALL, JUST LOAD PRG NAME                
         CLC   PMTPROG,=C'ALL'                                                  
         BNE   VKMNT10               NO, LOAD PROG NAME INTO PGNAME             
         XC    WORK,WORK             ALL=X'00' -> CLEAR WORK                    
VKMNT10  MVC   PGNAME,WORK           SAVE  PRG NAME                             
         OC    PMTPROG,SPACES        BLANK PAD PROG NAME FIELD                  
*                                                                               
         B     VKKEY30               GO BUILD KEY                               
         EJECT                                                                  
* ---------------------------------------------                                 
* VALIDATION OF KEY: FOR REPORT SCREEN (PRPT..)                                 
* ---------------------------------------------                                 
VKRPT15  XC    PGNAME,PGNAME         INITIALIZE KEY VARS TO BLANK               
         XC    SYSNAM,SYSNAM         INITIALIZE KEY VARS TO BLANK               
         XC    STAFID,STAFID         INITIALIZE OPTIONS  TO BLANK               
         OI    PRPTSYSH+6,X'80'      RE-TRANSMIT  FIELD                         
*                                                                               
         CLI   PRPTSYSH+5,0          ANY INPUT IN SYSTEM FIELD?                 
         BE    VKRPT20               NO, PRINT RPT FOR ALL SYSTEMS              
         CLI   PRPTSYSH+5,3          TEST IF 'ALL' WAS ENTERED                  
         BNE   *+14                  NO, JUST TAKE INPUT AND VALIDATE           
         CLC   =C'ALL',PRPTSYS       'ALL'=BLANK                                
         BE    VKRPT20               BUILD BLANK KEY                            
         LA    R2,PRPTSYSH           PT TO SYSTEM FIELD                         
         BAS   RE,FAKVAL             VALIDATE SYSTEM CODE                       
         BZ    *+14                  VALID INPUT                                
         MVC   GERROR,=AL2(INVSYS)                                              
         B     VSFMERR                                                          
         MVC   8(L'SYSNAM,R2),SYSNAM                                            
*                                                                               
VKRPT20  MVI   RPTYPE,1              SET DEFAULT TYPE TO RPT TYPE 1             
         OI    PRPTYPH+6,X'80'       RE-TRANSMIT FIELD                          
         LA    R2,PRPTYPH            PT TO REPORT TYPE HEADER                   
         GOTO1 ANY                   INPUT REQUIRED                             
         CLI   PRPTYP,C'Y'           WAS RPT TYPE TWO SELECTED?                 
         BNE   *+12                  NO, SEE IF RPT TYPE 2 SELECTED.            
         MVI   RPTYPE,2              YES, SET TO RPT 2                          
         B     VKRPT25               PROCESS NEXT FIELD                         
         CLI   PRPTYP,C'N'           WAS RPT TYPE ONE SELECTED?                 
         BNE   ERR10                 INPUT NOT Y/N                              
*                                                                               
VKRPT25  MVI   REMK,C'N'             DEFAULT- NO RMKS PRINTED                   
         LA    R2,PRPTRMKH           PT TO REPORT TYPE HEADER                   
         OI    PRPTRMKH+6,X'80'      RE-TRANSMIT FIELD                          
         CLI   PRPTRMKH+5,0          ANY INPUT IN RMK OPTION?                   
         BE    VKRPT30               NO, USE THE DEFAULT                        
         CLI   PRPTRMK,C'N'          REQUESTED NO REMARKS?                      
         BE    VKRPT30               NO,BUILD KEY                               
         CLI   PRPTRMK,C'Y'          WAS INPUT A 'Y'                            
         BNE   ERR10                 NO, INPUT MUST BE Y/N                      
         XC    REMK,REMK             CLEAR RMK INDICATOR=PRINT THEM             
*                                                                               
VKRPT30  LA    R2,PRPTSTFH           PT TO STAFF ID HEADER                      
         OI    PRPTSTFH+6,X'80'      RE-TRANSMIT FIELD                          
         CLI   5(R2),0               ANY INPUT IN THIS FIELD?                   
         BE    VKRPT40               NO STAFF FILTER, GO BUILD KEY              
         CLI   RPTYPE,1              FIELD INPUT VALID FOR RPT 1 ONLY           
         BNE   ERR11                 RPT 2 WAS RQSTD- ERROR                     
         BAS   RE,VALSTF             MAKE SURE IT'S IN THE VALID FORMAT         
         LA    R4,KEY                BUILD KEY FOR STAFF RECS                   
         USING MSTFKEYD,R4           USE STAFF RECORD DSCECT                    
         MVI   MSTFSYS,MSTFSYSQ      PUT IN STAFF RECORD CODE                   
         MVI   MSTFTYP,MSTFTYPQ      PUT IN STAFF RECORD CODE                   
         MVC   MSTFID,PRPTSTF        SEARCH FOR SPECIFIC ID                     
         GOTO1 READ                  IF WE COME BACK,STAFF ID EXISTS            
         MVC   STAFID,MSTFID         SAVE VALIDATED STAFF ID                    
         DROP R4                     DONE WITH STAFF RECORD DSECT               
*                                                                               
VKRPT40  XC    DPTFLT,DPTFLT         CLEAR DEPTS SELECTED FILTER                
         LA    R1,DPTFLT-1           PT 1 BYTE BEFORE DPT FILTER                
         LA    R2,PRPTPRGH           PT TO SYSTEMS FILTER FIELD                 
         BAS   RE,VKRPT50            IF FIELD='Y',SET BIT                       
         LA    R2,PRPTDOCH           PT TO TECH WR FILTER FIELD                 
         BAS   RE,VKRPT50            IF FIELD='Y',SET BIT                       
         LA    R2,PRPTCSVH           PT TO CLT SRV FILTER FIELD                 
         BAS   RE,VKRPT50            IF FIELD='Y',SET BIT                       
         LA    R2,PRPTCSPH           PT TO CAL SUP FILTER FIELD                 
         BAS   RE,VKRPT50            IF FIELD='Y',SET BIT                       
         CLI   RPTYPE,1              IF THIS IS RPT TYPE 1, DPTFLT=0 OK         
         BE    VKKEY30               ALL INPUTS ARE OKAY, GO BUILD KEY          
         OC    DPTFLT,DPTFLT         MAKE SURE THERE WAS INPUT-FOR RPT2         
         BNZ   VKKEY30               AT LEAST ONE WAS ENTERED,BUILD KEY         
         LA    R2,PRPTPRGH           PT TO FIRST FIELD IN LIST                  
         OI    PRPTPRGH+6,X'40'      POSITION CURSOR                            
         B     ERR13                 GIVE AT LEAST 1 FIELD REQD MSG             
*                                                                               
VKRPT50  LA    R1,1(R1)              BUMP FLAG SAVE                             
         OI    6(R2),X'80'           RE-TRANSMIT FIELDS                         
         CLI   5(R2),0               IS THERE ANY INPUT IN THIS FIELD           
         BER   RE                    NO, RETURN TO CALLER                       
         CLI   RPTYPE,2              INPUT ALLOWED FOR RPT2 ONLY                
         BNE   ERR12                 THIS WAS RPT 1 - INVALID                   
         CLI   8(R2),C'Y'            WAS DEPT REQUESTED?                        
         BNE   *+10                  NO, SEE IF VALID INPUT (Y/N)               
         MVI   0(R1),1               TURN ON BYTE FOR THAT DEPT                 
         BR    RE                    RETURN TO CALLER                           
         CLI   8(R2),C'N'            WAS NO ENTERED?                            
         BER   RE                    VALID INPUT, JUST RETURN                   
         B     ERR10                 MUST BE Y/N                                
*                                                                               
         EJECT                                                                  
*                                                                               
* ---------------------------------------------                                 
* VALIDATION OF KEY:  FOR LIST SCREEN (PLST..)                                  
* ---------------------------------------------                                 
VKLST20  XC    FLAG,FLAG             FLAG INDICATES WHAT COMBINATION            
*                                    OF INPUT FIELDS WERE SELECTED              
*                                    SYS=X'01' PGNAME=X'02' STAF=X'04'          
*                                                                               
         OI    PLSTSYMH+6,X'80'      RETRANSMIT FIELD                           
         OI    PLSTPRGH+6,X'80'      RETRANSMIT FIELD                           
         OI    PLSTSTFH+6,X'80'      RETRANSMIT FIELD                           
         XC    SYSNAM,SYSNAM         INIT SYSTEM FIELD OF KEY TO NULLS          
         CLI   PLSTSYMH+5,0          SYSTEM FIELD EMPTY?                        
         BE    VKPRG22               IF SO, SYST=NULL='ALL'                     
         CLI   PLSTSYMH+5,3          SYSTEM FIELD EMPTY?                        
         BNE   VKLST21               'ALL'NOT ENTERD                            
         CLC   PLSTSYM(3),=C'ALL'    WAS 'ALL' ENTERED                          
         BE    VKPRG22               YES, SAME AS NOTHING ENTERED               
VKLST21  LA    R2,PLSTSYMH           POINT TO SYSTEM FIELD                      
         BAS   RE,FAKVAL             VALIDATE SYSTEM AGAINST FAKPK TBL          
         BZ    *+14                  VALID INPUT                                
         MVC   GERROR,=AL2(INVSYS)                                              
         B     VSFMERR                                                          
         MVC   8(L'SYSNAM,R2),SYSNAM                                            
         OI    FLAG,X'01'            SET BIT TO INDICATE SYS WAS SLECTD         
*                                                                               
VKPRG22  XC    PGNAME,PGNAME         INIT PROG NAME OF KEY TO NULLS             
         CLI   PLSTPRGH+5,0          PROG NAME START AT PROVIDED?               
         BE    VKFLT25               NO, LIST ALL PROG NAMES FROM TOP           
         OC    PLSTPRG,SPACES        APPEND WITH TRAILING BLANKS                
         MVC   PGNAME,PLSTPRG        PUT PRG NAME IN PGNAME                     
         OI    FLAG,X'02'            SET PRG NAME BIT-PGNAME START @            
*                                                                               
VKFLT25  XC    STFLTR,STFLTR         INIT STAF FILTER TO NULLS                  
         CLI   PLSTSTFH+5,0          STAFF FILTER GIVEN?                        
         BE    VKFLG27               NO, TEST FLAG BITS                         
         LA    R2,PLSTSTFH           R2=STAFF FILTER HEADER                     
         BAS   RE,VALSTF             VALIDATE STAFF ID                          
         MVC   STFLTR,PLSTSTF        STAFF ID OKAY, SAVE FOR LATER USE          
         OI    FLAG,X'04'            SET STFLTR BIT- STFLTR SELECTED            
*                                                                               
VKFLG27  CLI   FLAG,X'07'            ALL FIELDS INPUT?                          
         BNE   VKKEY30               IF NOT, THEN INPUT COMBINATION OK          
         LA    R2,PLSTSYMH                                                      
         MVC   GERROR,=AL2(INVALID)                                             
         B     VSFMERR                                                          
         EJECT                                                                  
* --------------------------------------------------------                      
* VALATION OF KEY:  BUILD THE KEY FROM EQU'S,SYSTEM,PGNAME                      
* --------------------------------------------------------                      
VKKEY30  LA    R4,KEY                                                           
         USING PAKEYD,R4             BUILD KEY                                  
         XC    KEY,KEY               CLEAR THE KEY                              
         MVI   PASYS,PASYSQ          RECORD CODE                                
         MVI   PATYP,PATYPQ                                                     
         MVC   PASYSTM,SYSNAM                                                   
         MVC   PAPGNM,PGNAME                                                    
*                                                                               
         B     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
* --------------------------------------------------------------------          
* FAKVAL : VALIDATES SYSTEM CODE AGAINST FAKPAK TABLE.                          
*     ROUTINE GETS CALLED WITH R2 = SYSTEM'S FIELD HEADER.                      
*     IF THE INPUT IS VALID, THAT FIELD WILL CONTAIN THE LONG FORM OF           
*     THE SYSTEM NAME AND THE LENGTH OF THE FIELD (IN THE HEADER) WILL          
*     BE SET TO 7=MAX LENGTH.                                                   
* -------------------------------------------------------------------           
FAKVAL   NTR1                      SAVE REGISTERS                               
*                                                                               
         LA    R5,SYSTAB           LIST OF VALID SYSTEMS                        
         ZIC   R3,5(R2)            LENGTH OF INPUT FIELD                        
         BCTR  R3,0                                                             
FKVAL10  EXCLC R3,8(R2),0(R5)                                                   
         BE    FKVAL25                                                          
         LA    R5,L'SYSTAB(R5)     BUMP TO NEXT ENTRY                           
         CLI   0(R5),X'FF'                                                      
         BNE   FKVAL10                                                          
         LA    R1,1                SET TO NOT FOUND                             
         B     FKVALX                                                           
*                                                                               
FKVAL25  SR    R1,R1               NO ERROR                                     
         MVC   SYSNAM,0(R5)        SAVE ENTIRE SYSTEM NAME                      
*                                                                               
FKVALX   LTR   R1,R1               SET CONDITION CODE 0=OKAY 1=ERROR            
         XIT1                                                                   
         EJECT                                                                  
* --------------------------------------------------------------------          
* STAFF ID SET TO 8 ENTIRE CHARACTERS- BLANK PADDED                             
* PROCEDURE CALLED WITH R2=HEADER OF STAFID FIELD ON SCREEN.                    
* --------------------------------------------------------------------          
*                                                                               
VALSTF   CLI   5(R2),0               ANY INPUT?                                 
         BE    ERRSTF                NO, ERROR                                  
*                                                                               
         MVC   TMPWRK,SPACES         TEMP HOLDS STAFF ID                        
         ZIC   RF,5(R2)              LENGTH OF INPUT                            
         BCTR  RF,0                  REDUCE FOR EXMVC                           
         EXMVC RF,TMPWRK,8(R2)       SAVE INPUT CHARS OF STAFF ID               
         MVC   8(8,R2),TMPWRK        PUT BLANK PADDED ID BACK IN FLD            
         MVI   5(R2),8               RESET LENGTH TO 8                          
         BR    RE                    RETURN TO CALLER                           
*                                                                               
ERRSTF   MVC   GERROR,=AL2(INVALID) INVALID INPUT                               
         B     VSFMERR                                                          
         EJECT                                                                  
*                                                                               
* ********************************************************************          
* VALIDATE RECORD                                                               
* ********************************************************************          
VR       L     R4,AIO              BUILD RECORD                                 
         USING PAKEYD,R4                                                        
         MVC   SYSNAM,PMTSYSM       SAVE SYSTEM FOR LATER USE WITH LIST         
         MVC   PGNAME,PMTPROG       SAVE PROG     "        "    "               
*                                                                               
         MVI   ELCODE,PADSCELQ      DESCRIPTION ELEMENT                         
         GOTO1 REMELEM                                                          
         LA    R6,ELEM                                                          
         USING PADSCD,R6                                                        
         XC    ELEM,ELEM                                                        
         MVI   PADSCEL,PADSCELQ                                                 
         LA    R2,PMTDESCH           POINT TO DESCRIPTION ON SCREEN             
         GOTO1 ANY                                                              
         ZIC   R3,PMTDESCH+5         LENGTH OF NAME IN R3                       
         BCTR  R3,0                  DECR FOR EX                                
         EXMVC R3,PADSCR,PMTDESC     STORE VAR LENGTH DESCRIPTION               
         LA    R3,PADSCOVQ+1(R3)     TOTAL LENGTH OF ELEMENT                    
         STC   R3,PADSCLN            STORE LENGTH IN ELEMENT                    
         GOTO1 ADDELEM                                                          
         DROP  R6                                                               
*                                                                               
         LA    R5,1                 BEGIN WITH 1ST REMARK LINE                  
         LA    R2,PMTLIN1H          POINT TO 1ST LINE OF REMARK                 
         MVI   ELCODE,PARMKELQ      REMARKS ELEMENT                             
         GOTO1 REMELEM                                                          
         XC    HLF,HLF              INIT TO 0 BEFORE STORING RMK MAX            
         MVI   HLF+1,RMKMAXQ         HLF=MAX # RMK LINES ON SCREEN              
RMKS     CH    R5,HLF               HAVE WE PROCESSED ALL REMARKS?              
         BH    AUTELMT              YES, DO AUTHORS ELEMENTS                    
         LA    R6,ELEM                                                          
         USING PARMKD,R6                                                        
         XC    ELEM,ELEM                                                        
         MVI   PARMKEL,PARMKELQ                                                 
         CLI   5(R2),0               OPTIONAL FIELD EMPTY?                      
         BE    AUTELMT               IF NO MORE REMARKS, BREAK OUT              
         ZIC   R3,5(R2)              LENGTH OF REMARK IN R3                     
         BCTR  R3,0                  DECR FOR EX                                
         EXMVC R3,PARMK,8(R2)        MOVE RMK INTO ELEMENT                      
         LA    R3,PARMKOVQ+1(R3)     TOTAL LENGTH OF ELEMENT                    
         STC   R3,PARMKLN            STORE LENGTH IN ELEMENT                    
         STC   R5,PARMKLIN           STORE LINE NUMBER                          
         GOTO1 ADDELEM                                                          
         LA    R2,PMTLIN2H-PMTLIN1H(R2)  BUMP TO NEXT LINEH                     
         LA    R5,1(R5)              INCR LINE COUNTER                          
         B     RMKS                  LOOP BACK TO DO ALL RMKS LINES             
         DROP  R6                                                               
*                                                                               
AUTELMT  MVI   ELCODE,PAAUTELQ        AUTHOR FIELDS ELEMENT(S)                  
         GOTO1 REMELEM                                                          
*                                                                               
         LA    R6,ELEM               INITIALIZE FOR LOOP:                       
         USING PAAUTD,R6                                                        
         LA    R2,PMTPGMRH           POINT TO 1ST AUTHOR ON SCREEN              
         LA    R1,PMTENDLH           ADDR OF END OF INPUT-AUTH LIST             
         ST    R1,ENDADR                                                        
         LA    R5,1                  TYPE OF AUTHOR BEING ANALYZED              
*                                                                               
* BUILD NEW KEY FOR STAFF RECS.                                                 
         MVC   OLDKEY,KEY            SAVE THIS ELEMENTS KEY                     
         MVC   OLDKEYSV,SAVEKEY      SAVE THIS ELEMENTS KEYSAVE                 
         XC    KEY,KEY               BUILD NEW KEY FOR STAFF RECS               
         XC    SAVEKEY,SAVEKEY       CLEAR KEY AND SAVEKEY                      
*                                                                               
         LA    R3,KEY                BUILD NEW KEY FOR STAFF RECS               
         USING MSTFKEYD,R3           PLACE STAFF RECS DSECT ON KEY              
         MVI   MSTFSYS,MSTFSYSQ      PUT IN STAFF RECORD CODE                   
         MVI   MSTFTYP,MSTFTYPQ                                                 
*                                                                               
         LA    R2,PMTPGMRH           PROGRAMMER NAME REQUIRED                   
         GOTO1 ANY                   FIELD REQUIRED                             
*                                                                               
AUTLP    XC    ELEM,ELEM                                                        
         C     R2,ENDADR             END OF LIST YET?                           
         BNL   RESKEY                IF SO, RESTORE KEY                         
         CLI   5(R2),0               ANY INPUT FOR THIS AUTH FIELD?             
         BE    BUMP                  IF NOT, BUMP TO NEXT AUTH FIELD            
         BAS   RE,VALSTF             VALIDATE STAF ID INPUT FORMAT              
*                                                                               
         MVC   MSTFID,8(R2)          STAFF FMT IS VALID.PUT INTO KEY            
         MVC   AIO,AIO2              POINT TO NEW AREA FOR STAFF REC            
         GOTO1 READ                  SEARCH FOR BUILT KEY ON STAFF RECS         
*                                    IF WE COME BACK,STAFF ID IS VALID.         
         MVC   AIO,AIO1              RESTORE OLD AIO                            
         MVI   PAAUTEL,PAAUTELQ      SAVE ELEMENT CODE                          
         MVC   PAAUTSTF,8(R2)        SAVE AUTHOR ID                             
         STC   R5,PAAUTYP            SAVE TYPE OF AUTHOR                        
         MVI   PAAUTLN,PAAUTLNQ      SAVE FIXED LENGTH                          
         GOTO1 ADDELEM               ADD AUTHOR ELEMENT                         
*                                                                               
BUMP     LA    R2,AUTDISPQ(R2)       BUMP TO NEXT INPUT FIELD                   
         LA    R5,1(R5)              INCREMENT AUTH TYPE                        
         B     AUTLP                 LOOP BACK FOR ALL RECS                     
*                                                                               
RESKEY   MVC   KEY,OLDKEY            RESTORE KEY FIELD                          
         MVC   SAVEKEY,OLDKEYSV      RESTORE OLD KEYSAVE                        
         DROP  R6                                                               
         DROP  R3                                                               
*                                                                               
VR20     B     XIT                   DISPLAY THE RECORD                         
         DROP  R4                                                               
         EJECT                                                                  
* ********************************************************************          
* DISPLAY RECORD                                                                
* ********************************************************************          
DR       LA    R0,PMTTAGH            LAST FIELD ON SCREEN                       
         LA    R2,PMTDESCH           1ST FIELD OF RECORD INFO(NONKY)            
* ------------------                                                            
* CLEAR FIELDS FIRST                                                            
* ------------------                                                            
DR10     ZIC   R1,0(R2)              LENGTH OF FIELD + HEADER                   
         SH    R1,=H'17'             MINUS HEADER, EXTEN, AND 1 FOR EX          
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   8(0,R2),SPACES        BLANK OUT FIELD                            
         OI    6(R2),X'80'           TRANSMIT                                   
*                                                                               
DR20     ZIC   R1,0(R2)              RESTORE LENGTH                             
         AR    R2,R1                 NEXT SCREEN FIELD                          
         CR    R2,R0                 END OF SCREEN?                             
         BE    *+16                                                             
         TM    1(R2),X'20'           NO -- FIELD IS PROTECTED?                  
         BZ    DR10                  NO -- CLEAR IT                             
         B     DR20                  YES -- BUMP TO NEXT FIELD                  
         SPACE 3                                                                
* -------------------------------------------------                             
* CLEAR PROTECTED FIELDS: FULL NAME, EXTN, HOME TEL                             
* -------------------------------------------------                             
DR30     LA    R0,PMTENDLH           END OF PROTECTED FIELD ERASE SECT          
         LA    R2,PMTPGMRH           BASE TO PUT ERASE OFFSETS ON               
*                                                                               
DR40     MVC   NAMDISPQ+8(L'PMTNAME,R2),SPACES CLEAR FULL NAME                  
         MVI   NAMDISPQ+6(R2),X'80'  TRANSMIT                                   
         MVC   EXTDISPQ+8(L'PMTEXTN,R2),SPACES CLEAR EXTENSION                  
         MVI   EXTDISPQ+6(R2),X'80'  TRANSMIT                                   
         MVC   HOMDISPQ+8(L'PMTHOME,R2),SPACES CLEAR HOME TEL                   
         MVI   HOMDISPQ+6(R2),X'80'  TRANSMIT                                   
         LA    R2,AUTDISPQ(R2)       BUMP TO NEXT AUTHOR LINE                   
         CR    R2,R0                 END OF AUTHORS LIST?                       
         BL    DR40                  NO, LOOP BACK TO CLR ALL AUTHS             
*                                                                               
* DISPLAY CONTENTS OF ELMNTS THEN GO TO STAFF RECS TO DISP NAME & #'S           
* DESCRIPTION ELEMENT:                                                          
         L     R6,AIO                                                           
         MVI   ELCODE,PADSCELQ     DESCRIPTION ELEMENT                          
         BAS   RE,GETEL              FIND ELEMENT                               
         BE    *+6                   IF THERE, CONTINUE, ELSE BUG               
         DC    H'0'                  THIS ELEMENT IS REQUIRED                   
         USING PADSCD,R6                                                        
         ZIC   R3,PADSCLN            GET LENGTH OF ELEMENT                      
         LA    R4,PADSCOVQ+1         MINUS OVERHEAD MINUS 1 FOR EX              
         SR    R3,R4                 GET LENGTH OF NAME FIELD                   
         EXMVC R3,PMTDESC,PADSCR                                                
         OI    PMTDESCH+6,X'80'      TRANSMIT                                   
         DROP  R6                                                               
         SPACE 3                                                                
* ----------------                                                              
* REMARK ELEMENTS:                                                              
* ----------------                                                              
DRMK     L     R6,AIO              REMARKS ELEMENTS                             
         LA    R5,PMTLIN1H           POINT TO FIRST REMARK LINE                 
         MVI   ELCODE,PARMKELQ       SET CODE                                   
         BAS   RE,GETEL              FIND ELEMENT                               
DRMKLP   BNE   DAUT                  NOT THERE, DO AUTH DISPLAY                 
         USING PARMKD,R6                                                        
         ZIC   R3,PARMKLN            GET LENGTH OF ELEMENT                      
         LA    R4,PARMKOVQ+1         MINUS OVERHEAD MINUS 1 FOR EX              
         SR    R3,R4                 GET LENGTH OF NAME FIELD                   
         EXMVC R3,8(R5),PARMK                                                   
         OI    6(R5),X'80'           TRANSMIT                                   
         LA    R5,PMTLIN2H-PMTLIN1H(R5)  BUMP TO NEXT LINE DOWN                 
         BAS   RE,NEXTEL             GET NEXT ELEMENT                           
         B     DRMKLP                GET ALL RMK ELEMENTS                       
         DROP  R6                                                               
         EJECT                                                                  
*                                                                               
         SPACE 3                                                                
DAUT     LA    R1,PMTENDLH           FIELD AFTER LAST AUTH TYPE                 
         ST    R1,ENDADR             SAVE TERMINATION ADDRESS                   
*                                                                               
         L     R6,AIO                                                           
         USING PAAUTD,R6                                                        
         MVI   ELCODE,PAAUTELQ                                                  
         BAS   RE,GETEL                                                         
         BNE   DRX                                                              
*                                                                               
         L     R6,AIO                AUTHORS ELEMENTS                           
         MVI   ELCODE,PAAUTELQ       SET CODE                                   
         BAS   RE,GETEL              FIND ELEMENT                               
DAUTLP   BNE   DRX                   IF NOT THERE, EXIT DR                      
         USING PAAUTD,R6                                                        
         ST    R6,PRVELMT            SAVE PTR TO ELEMENT FOR NEXTEL             
         LA    R5,PMTPGMRH         POINT TO FIRST AUTHOR LINE(PRGRMR)           
         XC    HLF,HLF               CLEAR DISP 1/2 WORD FOR MULT               
         MVI   HLF+1,AUTDISPQ        DISPLACEMENT TO NEXT AUTH ROW              
         ZIC   R2,PAAUTYP            PUT TYPE OF AUTH IN R2                     
         BCTR  R2,0                  DECR TO ADJUST ROW # FOR MULT              
         MH    R2,HLF                CALCULATE OFFSET ON R5                     
         AR    R5,R2                 BUMP R5 TO CORRECT PSTN ON SCREEN          
         C     R5,ENDADR             SEE IF WE'RE >=END OF LIST                 
         BNL   DRX                   IF R5 > ENDADR EXIT LOOP                   
*                                                                               
         MVC   8(8,R5),PAAUTSTF      DISPLAY STAFF ID IN FIELD                  
         OI    6(R5),X'80'           TRANSMIT                                   
         MVC   STAFID,PAAUTSTF       SAVE STAFID FOR NEW KEY                    
         DROP  R6                                                               
*                                                                               
         MVC   AIO,AIO2              POINT TO NEW AREA FOR NEW KEY              
         MVC   OLDKEY,KEY            SAVE THIS ELEMENTS KEY                     
         MVC   OLDKEYSV,SAVEKEY      SAVE THIS ELEMENTS SAVEKEY                 
*                                                                               
         XC    KEY,KEY               BUILD NEW KEY FOR STAFF RECS               
         XC    SAVEKEY,SAVEKEY       CLEAR KEY AND SAVEKEY                      
         LA    R2,KEY                POINT TO KEY                               
         USING MSTFKEYD,R2           PLACE STAFF RECS DSECT ON KEY              
         MVI   MSTFSYS,MSTFSYSQ      PUT IN STAFF RECORD CODE                   
         MVI   MSTFTYP,MSTFTYPQ                                                 
         MVC   MSTFID,STAFID         LOOK FOR STAFF RECORD                      
         MVC   SAVEKEY,KEY           SAVE KEY WE'RE LOOKING FOR                 
         GOTO1 READ                  FIND STAFF REC IN DIRECTORY                
         GOTO1 GETREC                GET RECORD--IN AI0                         
*                                                                               
         SPACE 3                                                                
*EXTRACT NAME FROM STAFF RECORD                                                 
*                                                                               
         L     R6,AIO                POINT TO STAFF RECORD                      
         MVI   ELCODE,MSNAMELQ       SEARCH FOR NAME ELEMENT                    
         BAS   RE,GETEL              FIND ELEMENT                               
         BE    *+6                   IF THERE, CONTINUE, ELSE BUG               
         DC    H'0'                  BUG IF NAME ELEMENT MISSING                
         USING MSNAMD,R6                                                        
         ZIC   R3,MSNAMLN            GET LENGTH OF ELEMENT                      
         LA    R4,MSNAMOVQ+1         MINUS OVERHEAD MINUS 1 FOR EX              
         SR    R3,R4                 GET LENGTH OF NAME FIELD                   
         EXMVC R3,NAMDISPQ+8(R5),MSNAME                                         
         OI    NAMDISPQ+6(R5),X'80'           TRANSMIT                          
         DROP  R6                                                               
*                                                                               
         SPACE 3                                                                
* EXTRACT TEL NUMBERS FROM STAFF REC                                            
*                                                                               
         L     R6,AIO                POINT TO STAFF RECORD                      
         MVI   ELCODE,MSNUMELQ       SEARCH FOR NUMBER ELEMENT                  
         BAS   RE,GETEL              FIND ELEMENT                               
         BE    *+6                   IF THERE, CONTINUE, ELSE BUG               
         DC    H'0'                  BUG IF NUMBER ELEMENT MISSING              
         USING MSNUMD,R6                                                        
         MVC   EXTDISPQ+8(L'MSNUMOF,R5),MSNUMOF      OFFICE EXTENSION           
         OI    EXTDISPQ+6(R5),X'80'        TRANSMIT                             
         MVC   HOMDISPQ+8(L'MSNUMHM,R5),MSNUMHM   HOME TELEPHONE NUMBER         
         OI    HOMDISPQ+6(R5),X'80'        TRANSMIT                             
         DROP  R6                                                               
*                                                                               
         SPACE 3                                                                
*                                                                               
RESKEY2  MVC   AIO,AIO1              RESTORE OLDKEY                             
         MVC   KEY,OLDKEY            RESTORE KEY FIELD                          
         MVC   SAVEKEY,OLDKEYSV      RESTORE OLD KEYSAVE                        
         SPACE 3                                                                
*                                                                               
DBUMP    MVI   ELCODE,PAAUTELQ       SET CODE                                   
         L     R6,PRVELMT            RESTORE PTR TO PASS AUTH ELEMENT           
         BAS   RE,NEXTEL             LOOP BACK FOR ALL RECS                     
         B     DAUTLP                                                           
         SPACE 3                                                                
*                                                                               
DRX      B     XIT                                                              
         EJECT                                                                  
* ********************************************************************          
* DISPLAY KEY                                                                   
* ********************************************************************          
DK       L     R4,AIO              SELECTED RECORD                              
         USING PAKEYD,R4                                                        
         MVC   PMTSYSM,PASYSTM     SYSTEM NAME                                  
         OI    PMTSYSMH+6,X'80'                                                 
         MVC   PMTPROG,PAPGNM      PROGRAM NAME                                 
         OI    PMTPROGH+6,X'80'                                                 
         B     XIT                                                              
         EJECT                                                                  
* ********************************************************************          
* ON-SCREEN LIST                                                                
* ********************************************************************          
LR       LA    R4,KEY                                                           
         USING PAKEYD,R4                                                        
         OC    KEY,KEY             FIRST TIME THROUGH?                          
         BNZ   LR10                                                             
*                                                                               
         MVI   PASYS,PASYSQ        SYSTEM                                       
         MVI   PATYP,PATYPQ        RECORD TYPE                                  
         MVC   PASYSTM,SYSNAM      SYSTEM NAME                                  
         MVC   PAPGNM,PGNAME       PROGRAM START AT NAME                        
         MVC   SAVEKEY,KEY                                                      
*                                                                               
LR10     GOTO1 HIGH                FIRST RECORD                                 
         CLI   DMCB+8,0                                                         
         BE    LR30                                                             
         DC    H'0'                                                             
*                                                                               
LR20     LA    R4,KEY              NEXT RECORD                                  
         GOTO1 SEQ                                                              
         CLI   DMCB+8,0                                                         
         BE    LR30                                                             
         DC    H'0'                                                             
*                                                                               
LR30     CLC   KEY(2),SAVEKEY       SAME SYSTEM/PROGRAM?                        
         BNE   LRX                  NO MORE DATATYPES TO LIST                   
*                                                                               
         GOTO1 GETREC               GET THE SCROLLER DATATYPE RECORD            
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R4,AIO                                                           
*                                                                               
         OC    SYSNAM,SYSNAM         SPECIFIC SYSTEM REQUESTED?                 
         BZ    LFLTR                 NO, TEST STAF FILTER                       
         CLC   PASYSTM,SYSNAM        DOES RECORD SYSTEM MATCH?                  
         BNE   LRX                   NO-> NO MORE DATATYPES                     
*                                                                               
LFLTR    OC    STFLTR,STFLTR         SPECIFIC STAF ID  REQUESTED?               
         BZ    LBLD                  NO, SKIP STAFID SEARCH                     
*                                                                               
         L     R6,AIO                SEARCH THROUGH AUTHS ELMNTS FOR ID         
         MVI   ELCODE,PAAUTELQ                                                  
         BAS   RE,GETEL              FIND AN AUTH ELEMENT                       
LAUTH    BNE   LR20                  NO MATCH, NEXT RECORD                      
         USING PAAUTD,R6             GOT AUTH ELEMENT                           
         CLC   PAAUTSTF,STFLTR       MATCH ON STAFID?                           
         BE    LBLD                  YES, PROCESSES OUTPUT FIELDS               
         BAS   RE,NEXTEL             GET NEXT AUTHOR ELEMENT                    
         B     LAUTH                                                            
         DROP  R6                    FOUND MATCH, PROCESSES OUTPUT              
*                                                                               
* BUILD LIST LINE                                                               
LBLD     MVC   LISTAR,SPACES        CLEAR LIST LINE                             
         MVC   LSTSYS,PASYSTM       PLACE SYSTEM ON LIST LINE                   
         MVC   LSTPRG,PAPGNM        PLACE PROGRAM NAME ON LIST LINE             
* LIST DESCR ELEMENT                                                            
         L     R6,AIO                                                           
         MVI   ELCODE,PADSCELQ       DESCRIPTION ELEMENT                        
         BAS   RE,GETEL                                                         
         BE    *+6                   DESCR ELEMENT REQUIRED                     
         DC    H'0'                  IF NOT THERE, BUG!!                        
         USING PADSCD,R6                                                        
         ZIC   R3,PADSCLN            LENGTH OF ENTIRE ELMNT IN R3               
         LA    R5,PADSCOVQ+1         #FIXED BYTES BEFORE CHAR STRING            
         SR    R3,R5                 GET CORRECT LENGTH OF DESCR IN R3          
         EXMVC R3,LSTDSCR,PADSCR     PLACE ON LIST LINE                         
         DROP  R6                                                               
*FIND PROGRAMMERS STAFF ID                                                      
         L     R6,AIO                FIND PROGRAMMERS ID                        
         MVI   ELCODE,PAAUTELQ                                                  
         BAS   RE,GETEL              FIND AN AUTH ELEMENT                       
LPRG     BNE   LR70                  PRG NOT FOUND-PRINT WHAT WE HAVE           
         USING PAAUTD,R6             GOT AUTH ELEMENT                           
         CLI   PAAUTYP,PAAUTPRQ      IS THIS THE PROGRAMMERS ID ELMNT?          
         BNE   *+14                  NO, GET NEXT ELEMENT                       
         MVC   LSTPRGMR,PAAUTSTF     PUT STAFF ID ON LIST LINE                  
         B     LPRG10                LOOK UP EXTENSION                          
         BAS   RE,NEXTEL             GET NEXT AUTHOR ELEMENT                    
         B     LPRG                                                             
         DROP  R6                    FOUND MATCH, PROCESSES OUTPUT              
* READ STAFF RECORDS TO GET PROGRAMMERS EXTENSION                               
LPRG10   MVC   AIO,AIO2              POINT TO NEW AREA FOR NEW KEY              
         MVC   OLDKEY,KEY            SAVE THIS ELEMENTS KEY                     
         MVC   OLDKEYSV,SAVEKEY      SAVE THIS ELEMENTS KEYSAVE                 
*                                                                               
         XC    KEY,KEY               BUILD NEW KEY FOR STAFF RECS               
         LA    RF,KEY                POINT TO KEY                               
         USING MSTFKEYD,RF           PLACE STAFF RECS DSECT ON KEY              
         MVI   MSTFSYS,MSTFSYSQ      PUT IN STAFF RECORD CODE                   
         MVI   MSTFTYP,MSTFTYPQ                                                 
         MVC   MSTFID,LSTPRGMR       LOOK FOR STAFF RECORD                      
         DROP  RF                                                               
         GOTO1 HIGH                  FIND STAFF REC IN DIRECTORY                
         CLC   KEY(32),KEYSAVE                                                  
         BNE   LR68                  ** OBVIOUSLY FIRED **                      
         GOTO1 GETREC                GET RECORD--IN AI02                        
*                                                                               
         L     R6,AIO                POINT TO STAFF RECORD                      
         MVI   ELCODE,MSNUMELQ       SEARCH FOR NUMBER ELEMENT                  
         BAS   RE,GETEL              FIND ELEMENT                               
         BE    *+6                   IF THERE, CONTINUE, ELSE BUG               
         DC    H'0'                  BUG IF NUMBER ELEMENT MISSING              
         MVC   LSTEXTN,MSNUMOF-MSNUMD(R6)   MOVE IN EXTENSION                   
*                                                                               
LR68     MVC   AIO,AIO1              RESTORE OLDKEY                             
         XC    KEY,KEY               CLEAR KEY                                  
         MVC   KEY,OLDKEY            RESTORE KEY FIELD                          
         MVC   SAVEKEY,OLDKEYSV      RESTORE OLD KEYSAVE                        
         GOTO1 HIGH                  RESTORE DISK POINTER                       
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 GETREC                RESTORE DISK POINTER                       
*                                                                               
LR70     GOTO1 LISTMON               SEND RECORD TO SCREEN                      
         B     LR20                  NEXT RECORD                                
*                                                                               
LRX      B     XIT                                                              
         EJECT                                                                  
* ********************************************************************          
* OFFLINE PRINT                                                                 
* ********************************************************************          
*                                                                               
PR       MVC   RCSUBPRG,RPTYPE     COPY REPT TYPE TO PICK HDSPECS               
         LA    R1,HEDSPECS         REPORT TYPE 1 HEADLINE SPECS                 
         ST    R1,SPECS                                                         
         LA    R1,HDHOOK           HEAD HOOK                                    
         ST    R1,HEADHOOK                                                      
*                                                                               
PR10     LA    R4,KEY                                                           
         USING PAKEYD,R4           DSECT FOR KEY                                
         XC    KEY,KEY                                                          
         MVI   PASYS,PASYSQ        RECORD CODE                                  
         MVI   PATYP,PATYPQ        RECORD CODE                                  
         MVC   PASYSTM,SYSNAM      SYSTEM                                       
         MVC   PAPGNM,PGNAME       PROGRAM                                      
         MVC   SAVEKEY,KEY                                                      
*                                                                               
         CLI   RPTYPE,1            IS THIS REPT TYPE 1?                         
         BE    PR15                YES                                          
         GOTO1 =V(SORTER),DMCB,SORTCARD,RECCARD   OPEN SORTER FOR RPT2          
         LTR   R1,R1               DUMMY--------                                
*                                                                               
PR15     GOTO1 HIGH                FIRST RECORD                                 
         CLI   DMCB+8,0                                                         
         BE    PR30                                                             
         DC    H'0'                                                             
*                                                                               
PR20     MVC   KEYSAVE,KEY                                                      
         LA    R4,KEY              NEXT RECORD                                  
         GOTO1 SEQ                                                              
         CLI   DMCB+8,0                                                         
         BE    PR30                                                             
         DC    H'0'                                                             
*                                                                               
PR30     CLC   KEY(2),SAVEKEY      PASS RECORD?                                 
         BNE   PRX                 NO MORE TO REPORT                            
*                                                                               
         OC    SYSNAM,SYSNAM       ANY SYSTEM FILTER?                           
         BZ    *+14                                                             
         CLC   SYSNAM,PASYSTM      YES - MATCH ON SYSTEM                        
         BNE   PRX                 NO                                           
*                                                                               
         GOTO1 GETREC              GET THE SCROLLER DATATYPE RECORD             
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R4,AIO                                                           
*                                                                               
         CLI   RPTYPE,2               IS THIS RPT TYPE2?                        
         BE    PRPT2                 YES, PROCESS RECD DIFFERENTLY              
*                                                                               
* -------------------------------------------------------------------           
* REPORT TYPE 1: SORT BY SYSTEM/PROGRAM                                         
* 1. FOR EACH RECORD READ IN, IF THERE WAS A SYSTEM FILTER, DETERMINE           
*    IF RECORD IS IN THE SAME SYSTEM, IF NOT, DISCARD. DETERMINE IF             
*    THERE WAS A STAFF FILTER, IF SO, SEARCH THRU AUTHOR ELEMENTS TO            
*    SEE IF THAT RECORD CONTAINS THAT AUTHOR, IF NOT DISCARD.                   
* 2. IF THE RECORD IS VALID, DISPLAY. BRANCH BACK TO GET ALL RECORDS.           
* -------------------------------------------------------------------           
*                                                                               
         OC    STAFID,STAFID         IS THERE A STAFID FILTER?                  
         BZ    PR30B                 NO, SKIP FILTERING                         
         L     R6,AIO                YES                                        
         USING PAAUTD,R6             SEARCH THRU AUTH ELMNTS FOR STAFID         
         MVI   ELCODE,PAAUTELQ       AUTHORS ELEMENT CODE                       
         BAS   RE,GETEL              GET FIRST AUTHOR ELEMENT                   
         BNE   PR20                  NO AUTH ELEMENTS, SKIP THIS RECORD         
*                                                                               
PR30A    CLC   PAAUTSTF,STAFID       DOES THE ELMNT STAFID MATCH?               
         BE    PR30B                 YES, KEEP THIS RECORD                      
         BAS   RE,NEXTEL             NO, LOOK AT NEXT AUTH ELMNT                
         BE    PR30A                 IF THERE WAS ANOTHER, LP BACK              
         B     PR20                  NO MORE-NO MATCH-NO RECORD                 
*                                                                               
PR30B    CLC   PASYSTM,(PASYSTM-PAKEYD)+KEYSAVE                                 
         BE    *+8                                                              
         MVI   FORCEHED,C'Y'       EJECT PAGE ON SYSTEM/PROGRAM CHANGE          
*                                                                               
         MVC   PRTPROG,PAPGNM      PUT PROGRAM IN PRINT LIN                     
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,PADSCELQ     DESCRIPTION ELEMENT                          
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                THIS ELEMENT IS REQUIRED                     
         USING PADSCD,R6                                                        
         ZIC   R3,1(R6)            ELEMENT LENGTH                               
         SH    R3,=Y(PADSCOVQ)     SUBTRACT FIXED OVERHEAD                      
         BCTR  R3,0                AND ONE MORE FOR EX                          
         EXMVC R3,PRTDESC,PADSCR   PUT DESCRIPTION IN PRINT LINE                
         DROP  R6                                                               
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,PAAUTELQ       STAFF INFO ELEMENTS                        
         BAS   RE,GETEL                                                         
         BNE   PR33                                                             
PR31     DS    0H                                                               
         USING PAAUTD,R6                                                        
         CLI   PAAUTYP,1             PROGRAMMER STAFF ID                        
         BNE   PRT31A                                                           
         LA    R3,PRTPGM             PT TO OUTPUT FIELD                         
         B     PR31SUB               OUTPUT NAME, '-X', AND EXTN                
PRT31A   CLI   PAAUTYP,2             TECH. PUB. STAFF ID                        
         BNE   PRT31B                                                           
         LA    R3,PRTDOC             PT TO OUTPUT FIELD                         
         B     PR31SUB               OUTPUT NAME, '-X', AND EXTN                
PRT31B   CLI   PAAUTYP,3             CLIENT SERVICE STAFF ID                    
         BNE   PRT31C                                                           
         LA    R3,PRTSER             PT TO OUTPUT FIELD                         
         B     PR31SUB               OUTPUT NAME, '-X', AND EXTN                
PRT31C   CLI   PAAUTYP,4             CALL SUPPORT STAFF ID                      
         BNE   PRT31D                                                           
         LA    R3,PRTCAL             PT TO OUTPUT FIELD                         
         B     PR31SUB               OUTPUT NAME, '-X', AND EXTN                
PRT31D   CLI   PAAUTYP,5             CALL SUPPORT STAFF ID                      
         BNE   PRT31E                                                           
         LA    R3,PRTBAK             PT TO OUTPUT FIELD                         
         B     PR31SUB               OUTPUT NAME, '-X', AND EXTN                
*                                                                               
PR31SUB  MVC   0(8,R3),PAAUTSTF      DISPLAY STAFF ID IN FIELD                  
*                                                                               
*READ STAFF RECS TO EXTRACT EXTN                                                
         MVC   AIO,AIO2              POINT TO NEW AREA FOR NEW KEY              
         MVC   OLDKEY,KEY            SAVE THIS ELEMENTS KEY                     
         MVC   OLDKEYSV,SAVEKEY      SAVE THIS ELEMENTS SAVEKEY                 
         LR    R5,R6                 SAVE PTR TO ELEMENTS FOR NEXTEL            
*                                                                               
         XC    KEY,KEY               BUILD NEW KEY FOR STAFF RECS               
         XC    SAVEKEY,SAVEKEY       CLEAR KEY AND SAVEKEY                      
         LA    R2,KEY                POINT TO KEY                               
         USING MSTFKEYD,R2           PLACE STAFF RECS DSECT ON KEY              
         MVI   MSTFSYS,MSTFSYSQ      PUT IN STAFF RECORD CODE                   
         MVI   MSTFTYP,MSTFTYPQ                                                 
         MVC   MSTFID,PAAUTSTF       LOOK FOR STAFF RECORD                      
         MVC   SAVEKEY,KEY           SAVE KEY WE'RE LOOKING FOR                 
         GOTO1 READ                  FIND STAFF REC IN DIRECTORY                
         GOTO1 GETREC                GET RECORD--IN AI0                         
*                                                                               
         L     R6,AIO                POINT TO STAFF RECORD                      
         MVI   ELCODE,MSNUMELQ       SEARCH FOR NUMBER ELEMENT                  
         BAS   RE,GETEL              FIND ELEMENT                               
         BE    *+6                   IF THERE, CONTINUE, ELSE BUG               
         DC    H'0'                  BUG IF NUMBER ELEMENT MISSING              
         USING MSNUMD,R6                                                        
         MVC   10(L'MSNUMOF,R3),MSNUMOF   MOVE IN EXTENSION                     
         DROP  R6                                                               
*                                                                               
         MVC   AIO,AIO1              RESTORE OLDKEY                             
         MVC   KEY,OLDKEY            RESTORE KEY FIELD                          
         MVC   SAVEKEY,OLDKEYSV      RESTORE OLD KEYSAVE                        
         LR    R6,R5                 RESTORE PTR TO ELEMENTS FOR NEXTEL         
*                                                                               
PRT31E   BAS   RE,NEXTEL                                                        
         BE    PR31                                                             
*                                                                               
PR33     CLI   REMK,C'N'           SUPRESS PRINTING REMARKS?                    
         BE    PR40                YES, DON'T PRINT THEM                        
*                                                                               
         L     R6,AIO                                                           
         BAS   RE,REMARKS          PROCESS REMARKS ELEMENTS                     
         GOTO1 SPOOL,DMCB,(R8)     PRINT RECORD                                 
*                                                                               
PR40     DS    0H                                                               
         GOTO1 SPOOL,DMCB,(R8)     PRINT A BLANK LINE BETWEEN PRGMS             
         MVC   KEYSAVE,KEY                                                      
         LA    R4,KEY              NEXT RECORD                                  
         GOTO1 HIGH                                                             
         CLI   DMCB+8,0                                                         
         BE    PR20                                                             
         DC    H'0'                                                             
         EJECT                                                                  
*                                                                               
* --------------------------------------------------------------------          
* REPORT TYPE 2: SORT BY DEPT/STAFFID                                           
* 1.INPUT RECDS. FOR EACH RECORD READ IN, SEARCH THRU AUTHORS ELEMENTS          
*   TO SEE IF THERE IS A MATCH BY DEPT REQUESTED. IF SO, THEN FOR EACH          
*   OF THOSE DEPTS, BUILD A KEY WITH THE DEPT FIRST,AND SEND TO SORTER.         
*   SEARCH THRU ALL AUTHOR ELEMENTS FOR THE RECORD. THEN RETURN TO GET          
*   THE NEXT RECORD.                                                            
* 2.AFTER ALL RECDS READ IN, GET THEM BACK SORTED FROM SORTER AND               
*   OUTPUT THEM TO THE PRINTER.                                                 
* --------------------------------------------------------------------          
*                                                                               
PRPT2    LA    R5,DPTFLT-1         PT TO 1 BYTE BEFORE DEPT FILTERS             
         L     R6,AIO              PT TO RECORD READ IN                         
         USING PAAUTD,R6           WE'LL BE PRIMARLY INTRSTED IN AUTHS          
         MVI   ELCODE,PAAUTELQ     AUTHORS ELEMENT CODE                         
         BAS   RE,GETEL            GET FIRST AUTHOR'S ELEMENT                   
         BNE   PR20                NONE EXIST-BR BACK TO GET NXT RECD           
*                                                                               
PRT210   ZIC   R1,PAAUTYP          WHAT DEPT DOES AUTH BELONG TO?               
         AR    R1,R5               PT TO THAT DEPT ON DEPT FILTER               
         CLI   0(R1),1             WAS THIS DEPT REQUESTED?                     
         BE    PRT220              YES, ADD TO SORTER                           
*                                                                               
PRT10A   BAS   RE,NEXTEL           LOOK AT NEXT AUTH ELMNT                      
         BE    PRT210              MORE AUTH ELMNTS EXIST, LOOP                 
         B     PR20                NO MORE AUTHS, GET NEXT RECD                 
*                                                                               
PRT220   MVC   SRTDPT,PAAUTYP      SAVE DEPT IN SORT KEY                        
         MVC   SRTSTF,PAAUTSTF     SAVE STAFF IN SORT KEY                       
         MVC   SRTSYS,PASYSTM      SAVE SYSTEM IN SORT KEY                      
         MVC   SRTPRG,PAPGNM       SAVE PROGRAM IN SORT KEY                     
* GIVE ALL DATA TO SORTER -- SAVE ALL ELEMENTS                                  
         LA    R0,SRTDATA          PT TO DESTN IN SORT REC FOR DATA             
         LH    R1,PARECLN          RECORD LENGTH                                
         SH    R1,=Y(PAFRST-PAKEYD)  MINUS DISPL TO 1ST BYTE OF DATA            
         LA    R3,SRTOVQ(R1)       LENGTH OF ENTIRE RECORD IN SRTREC            
         STH   R3,SRTLN            SAVE VARIABLE LENGTH                         
         LA    RE,PAFRST           ADDRESS OF FIRST ELEMENT                     
         LR    RF,R1               # BYTES TO MOVE FROM REC->SORTER             
         MVCL  R0,RE               MOVE ALL ELEMENTS TO SORTER                  
*                                                                               
         GOTO1 =V(SORTER),DMCB,=C'PUT',SRTREC   GIVE RECORD TO SORTER           
         B     PRT10A              PROCESS THE OTHER DPTS RQSTD                 
*                                                                               
* --------------------------------------------------------------------          
PRX      CLI   RPTYPE,1           TYPE ONE REPORT?                              
         BE    PRXF               YES, EXIT PRINT ROUTINE                       
*                                                                               
*REPORT2: GET RECS FROM SORTER AND OUTPUT THEM                                  
         XC    SAVDPT,SAVDPT      CLEAR SAVED DPT FOR PG-BRK                    
         XC    SAVSTF,SAVSTF      CLEAR SAVED STAFF FOR PG-BRK                  
*                                                                               
PRT230   GOTO1 =V(SORTER),DMCB,=C'GET' GET FIRST RECORD                         
         ICM   R3,15,4(R1)        RETURN IF NULL ADDRESS                        
         BZ    PRX10              DONE PRINTING                                 
         LA    R0,SRTREC          PT TO DESTN -> SORT REC                       
         LH    R1,0(R3)           # BYTES TO MOVE=RECORD LENGTH                 
         LR    RE,R3              ADDRESS OF SOURCE                             
         LR    RF,R1              # BYTES TO MOVE FROM REC->SRTREC              
         MVCL  R0,RE              MOVE ALL ELEMENTS TO SRTREC                   
*                                                                               
         CLI   SAVDPT,0           IF NULL THEN THIS IS 1ST GETREC CALL          
         BNE   PRT235             IF NOT, TEST FOR PG-BRK                       
         MVC   SAVDPT,SRTDPT      SAVE DEPT FOR LATER PG-BRK TEST               
         MVC   SAVSTF,SRTSTF      SAVE STAFF FOR LATER PG-BRK TEST              
         MVC   SAVSYS,SRTSYS      SAVE STAFF FOR LATER SPACING TEST             
         B     PRT240             SKIP PAGE BREAK TESTS                         
*                                                                               
PRT235   CLC   SAVDPT,SRTDPT      IS THIS REC FOR A DIFF DPT                    
         BE    *+18               NO, SAVE DPT, TEST STAFF                      
         MVI   FORCEHED,C'Y'      YES, DIFF DPT, PAGE BREAK                     
         MVC   SAVSYS,SRTSYS      SAVE SYSTEM TO AVOID EXTRA BLNK LINE          
         B     *+14               PAGE BROKEN, FORGET OTHER TESTS               
         CLC   SAVSTF,SRTSTF      DIFF STAFF?                                   
         BNE   *-20               YES, FORCE A PAGE BREAK                       
*                                                                               
         CLC   SAVSYS,SRTSYS      IS THIS THE SAME SYSTEM AS PREV REC?          
         BE    PRT240             YES, NO BLANK LINE                            
         L     R5,ABOX             A(BOX DSECT)                                 
         USING BOXD,R5                                                          
         ZIC   R1,LINE                                                          
         LA    R1,BOXROWS-1(R1)    PRINT HORIZONTAL LINE                        
         MVI   0(R1),C'M'                                                       
         MVI   BOXINIT,0                                                        
         GOTO1 SPOOL,DMCB,(R8)    DIFF SYSTEM, LEAVE A BLANK LINE               
         DROP  R5                                                               
*                                                                               
PRT240   CLI   SRTDPT,1           CONVERT AUTHOR CODE TO CHARS                  
         BNE   *+10               NOT SYSTEMS                                   
         MVC   PRT2DPT,=CL12'PROGRAMMER'                                        
         CLI   SRTDPT,2           TECH WRITER?                                  
         BNE   *+10               NOT SYSTEMS                                   
         MVC   PRT2DPT,=CL12'TECH WRITER '                                      
         CLI   SRTDPT,3           CLIENT SERVICE?                               
         BNE   *+10               NOT SYSTEMS                                   
         MVC   PRT2DPT,=CL12'CLT. SERVICE'                                      
         CLI   SRTDPT,4           CALL SUPPORT?                                 
         BNE   *+10               NOT SYSTEMS                                   
         MVC   PRT2DPT,=CL12'CALL SUPPORT'                                      
*                                                                               
         MVC   PRT2SYS,SRTSYS     PUT SYSTEM ON REPT LINE                       
         MVC   PRT2PRG,SRTPRG     PUT PROGRAM ON REPT LINE                      
*                                                                               
         LA    R6,SRTREC           PT TO BEG OF RECORD                          
         USING PADSCD,R6           PLACE DESCRIPTION ELMNT DSCECT               
         MVC   DATADISP,=Y(SRTOVQ) DISPLACEMENT TO FIRST ELEMENT                
         MVI   ELCODE,PADSCELQ     GET DESCRIPTION ELEMENT                      
         BAS   RE,GETEL            R6 WILL PT TO DESCR ELEMNT                   
         BE    *+6                 IF THERE CONTINUE                            
         DC    H'0'                ELSE BUG- THIS ELMNT IS REQUIRED             
         ZIC   R3,PADSCLN          GET LENGTH OF DESCR ELEMENT                  
         SH    R3,=Y(PADSCOVQ)     MINUS DSRC ELMNT OVERHEAD                    
         BCTR  R3,0                -1 FOR EXMVC                                 
         EXMVC R3,PRT2DSC,PADSCR   MOVE IN DESCRIPTION TO SORT                  
         DROP  R6                  DROP DESCRIPTION DSCECT                      
*                                                                               
         CLI   REMK,C'N'           SUPRESS PRINTING REMARKS?                    
         BE    PRT245              YES, DON'T PRINT THEM                        
         LA    R6,SRTREC           PT TO BEG OF RECORD                          
         BAS   RE,REMARKS          PROCESS REMARKS ELEMENTS                     
         BNE   PRT245              IF COND SET <>,NO RMKS, NO BLK LN            
         GOTO1 SPOOL,DMCB,(R8)     PRINT RECORD                                 
*                                                                               
PRT245   GOTO1 SPOOL,DMCB,(R8)    PRINT THE RECORD/BLANK LINE                   
         MVC   SAVDPT,SRTDPT      SAVE DPT FOR NEXT REC TEST                    
         MVC   SAVSTF,SRTSTF      SAVE FOR NEXT REC TESTS                       
         MVC   SAVSYS,SRTSYS      SAVE FOR NEXT REC TESTS                       
         B     PRT230             LOOP TO GET ALL RECS                          
*                                                                               
PRX10    MVC   DATADISP,=H'42'     RESTORE DATADISP FOR GENFIL                  
         GOTO1 =V(SORTER),DMCB,=C'END' CLOSE SORTER                             
*                                                                               
PRXF     B     XIT                                                              
         EJECT                                                                  
* ******************************************************************            
* PRINT REMARKS ON OUTPUT LINE P2, P3, P4 RETURN TO CALLER (RE)                 
* ******************************************************************            
*                                                                               
REMARKS  NTR1                                                                   
         MVI   ELCODE,PARMKELQ     REMARKS ELEMENT                              
         BAS   RE,GETEL                                                         
         BNE   XIT                 CONDITION CODE SET TO NOT EQUAL              
         USING PARMKD,R6                                                        
REMK34   ZIC   R5,PARMKLN          LENGTH OF ELEMENT                            
         SH    R5,=Y(PARMKOVQ)     LENGTH OF FIXED                              
         BCTR  R5,0                ONE MORE FOR THE EX                          
         EXMVC R5,WORK,PARMK       TMP PUT RMK IN WORK TO:X'40'->00             
         LA    R3,1(R5)            R3=LOOP CNTR THRU STRING                     
         LA    R1,WORK             PT TO STRING                                 
REMK34A  CLI   0(R1),C' '          IS THIS A SPACE CHARACTER?                   
         BNE   *+8                 NO, SKIP CONVERSION                          
         MVI   0(R1),0             REPLACE X'40' WITH X'00                      
         LA    R1,1(R1)            BUMP TO NEXT CHARACTER                       
         BCT   R3,REMK34A          DECREMENT LOOP COUNTER AND BRANCH            
*                                                                               
         LA    R3,PRTRMK1          PT TO POSITION FOR 1ST REMARK                
         CLI   RPTYPE,1            IS THIS TYPE 1 REPORT DISPLACEMENTS          
         BE    *+8                                                              
         LA    R3,PRT2DISQ(R3)     BUMP TO RIGHT PSTN FOR RMK IN RPT2           
         CLI   PARMKLIN,1          IS THIS THE FIRST RMK?                       
         BNE   REMK35              NO                                           
         EXMVC R5,0(R3),WORK       YES, MOVE INTO 2ND PRINT LINE                
REMK35   CLI   PARMKLIN,2          IS THIS THE 2ND RMK?                         
         BNE   REMK37              NO                                           
         EXMVC R5,132(R3),WORK     YES, MOVE INTO 2ND PRINT LINE                
REMK37   CLI   PARMKLIN,3          IS THIS THE 3RD RMK?                         
         BNE   REMK38              NO                                           
         EXMVC R5,132+132(R3),WORK YES, MOVE INTO 3RD PRINT LINE                
REMK38   BAS   RE,NEXTEL           GET NEXT REMARK ELEMENT                      
         BE    REMK34              GOT ANOTHER ONE, LOOP BACK                   
         DROP  R6                                                               
         CR    R1,R1               SET CONDITION CODE TO = (WAS 1 RMK)          
         B     XIT                 RETURN TO CALLER                             
*                                                                               
         EJECT                                                                  
* ******************************************************************            
* HEAD HOOK ROUTINE                                                             
* ******************************************************************            
*                                                                               
HDHOOK   NTR1                                                                   
         MVC   H1+50(25),=C'PROGRAM ASSIGNMENT REPORT'                          
         MVI   H2+50,X'BF'         UNDERLINE CHARACTER                          
         MVC   H2+51(24),H2+50                                                  
         CLI   RPTYPE,2            REPORT TYPE 2?                               
         BE    HDHOOKA             YES                                          
         MVC   H4+8(L'PASYSTM),PASYSTM    SYSTEM                                
         B     HDHOOKB                                                          
HDHOOKA  MVC   H4+6(L'PRT2DPT),PRT2DPT    DEPARTMENT                            
         MVC   H4+27(L'SRTSTF),SRTSTF     STAFF ID                              
HDHOOKB  ICM   RF,15,ABOX          DO WE HAVE A(BOXES)?                         
         BZ    HDHOOKX             NO                                           
*                                                                               
         USING BOXD,RF             DEFINE BOX AREA                              
         MVC   BOXROWS,SPACES                                                   
         MVI   BOXROWS+5,C'T'                                                   
         MVI   BOXROWS+7,C'M'                                                   
         MVI   BOXROWS+57,C'B'                                                  
         MVC   BOXCOLS,SPACES                                                   
         MVI   BOXCOLS,C'L'                                                     
*                                                                               
         CLI   RPTYPE,1            IS THIS IS THIS RPT TYPE 1?                  
         BNE   HDHOOK2                                                          
         MVI   BOXCOLS+9,C'C'                                                   
         MVI   BOXCOLS+50,C'C'                                                  
         MVI   BOXCOLS+65,C'C'                                                  
         MVI   BOXCOLS+80,C'C'                                                  
         MVI   BOXCOLS+95,C'C'                                                  
         MVI   BOXCOLS+110,C'C'                                                 
         MVI   BOXCOLS+125,C'R'                                                 
         B     HDHOOK3                                                          
*                                                                               
HDHOOK2  MVI   BOXCOLS+8,C'C'      SYSTEM-PROGRAM COLUMN                        
         MVI   BOXCOLS+17,C'C'     PROGRAM-DESCRIPTION COLUMN                   
         MVI   BOXCOLS+78,C'R'     RIGHT MARGIN COLUMN (AFTER DESCR)            
*                                                                               
HDHOOK3  MVI   BOXWT,1                                                          
         MVI   BOXYORN,C'Y'                                                     
         MVI   BOXINIT,0                                                        
         MVI   BOXOFF,0                                                         
         DROP  RF                                                               
*                                                                               
HDHOOKX  B     XIT                                                              
         EJECT                                                                  
* ********************************************************************          
* HEDSPECS- FOR REPORTS 1 AND 2 DEPENDING ON RPTYPE->RCSUBPRG                   
* ********************************************************************          
HEDSPECS SPROG 1,THRU,2                                                         
         SSPEC H1,1,AGYNAME                                                     
         SSPEC H2,1,AGYADD                                                      
         SSPEC H1,95,REPORT                                                     
         SSPEC H1,114,REQUESTOR                                                 
         SSPEC H2,95,RUN                                                        
         SSPEC H2,121,PAGE                                                      
* REPT 1                                                                        
         SPROG 1                                                                
         SSPEC H4,1,C'SYSTEM:'                                                  
         SSPEC H7,2,C'PROGRAM  DESCRIPTION/REMARKS'                             
         SSPEC H7,52,C'PROGRAMMER     TECH WRITER    CLT. SERVICE'              
         SSPEC H7,97,C'CALL SUPPORT   BACKUP PRGMR'                             
         SPACE 3                                                                
* REPT 2                                                                        
         SPROG 2                                                                
         SSPEC H4,1,C'DEPT:'                                                    
         SSPEC H4,20,C'STAFF:'                                                  
         SSPEC H7,2,C'SYSTEM  PROGRAM  DESCRIPTION'                             
         DC    X'00'                                                            
         EJECT                                                                  
* *********************************************************************         
* ERROR MESSAGES:                                                               
* *********************************************************************         
ERR10    XC    CONHEAD,CONHEAD       CLEAR HEADER                               
         MVC   CONHEAD(20),=C'INPUT MUST BE Y OR N'                             
         GOTO1 ERREX2                                                           
*                                                                               
ERR11    XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(36),=C'FIELD NOT USED WHEN SORTING BY STAFF'             
         GOTO1 ERREX2                                                           
*                                                                               
ERR12    XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(37),=C'FIELD NOT USED WHEN SORTING BY SYSTEM'            
         GOTO1 ERREX2                                                           
*                                                                               
ERR13    XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(40),=C'AT LEAST ONE DEPARTMENT MUST BE SELECTED'         
         GOTO1 ERREX2                                                           
* *********************************************************************         
* FIELD DECLARATIONS, DSECTS, SCREENS (ALL THE ++INCLUDEES)                     
* *********************************************************************         
VSFMERR  GOTO1 SFMERR                                                           
         SPACE 2                                                                
         GETEL R6,DATADISP,ELCODE                                               
         SPACE 2                                                                
RELO     DS    F                                                                
* SORTKEY VARIABLES- SORT AT COL 1, FOR A LENGTH OF 24, IN ASCND ORD            
RECCARD  DC    CL80'RECORD TYPE=V,LENGTH=2104'                                  
SORTCARD DC    CL80'SORT FIELDS=(5,24,A),FORMAT=BI,WORK=1'                      
         SPACE 3                                                                
*******************************************************************             
* SYSTAB -     TABLE OF VALID SYSTEM NAMES                                      
*******************************************************************             
SYSTAB   DS    0CL7                                                             
         DC    CL7'ACCOUNT'                                                     
         DC    CL7'CONTROL'                                                     
         DC    CL7'CPP    '                                                     
         DC    CL7'GAMES  '                                                     
         DC    CL7'MBASE  '                                                     
         DC    CL7'MPL    '                                                     
         DC    CL7'NETWORK'                                                     
         DC    CL7'PERSON '                                                     
         DC    CL7'PRINT  '                                                     
         DC    CL7'REP    '                                                     
         DC    CL7'SECURE '                                                     
         DC    CL7'SERVICE'                                                     
         DC    CL7'SPOT   '                                                     
         DC    CL7'TALENT '                                                     
         DC    X'FFFFFF'                                                        
*                                                                               
         SPACE 3                                                                
*******************************************************************             
* LITERALS                                                                      
*******************************************************************             
         LTORG                                                                  
         EJECT                                                                  
* DDBIGBOX                                                                      
* DDSPOOLD                                                                      
* DDSPLWORKD                                                                    
* CTSFMA7D                                                                      
         PRINT OFF                                                              
       ++INCLUDE FAFACTS                                                        
         EJECT                                                                  
       ++INCLUDE FASYSLSTD                                                      
         EJECT                                                                  
       ++INCLUDE DDBIGBOX                                                       
         EJECT                                                                  
       ++INCLUDE DDSPOOLD                                                       
         EJECT                                                                  
       ++INCLUDE DDSPLWORKD                                                     
         EJECT                                                                  
       ++INCLUDE CTSFMFFD                                                       
         EJECT                                                                  
         PRINT ON                                                               
         ORG   CONTAGH                                                          
       ++INCLUDE CTSFMA7D                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE CTSFMB7D                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE CTSFMC9D                                                       
         EJECT                                                                  
       ++INCLUDE DDGENTWA                                                       
         EJECT                                                                  
       ++INCLUDE CTPRGASSD                                                      
         EJECT                                                                  
AUTDISPQ EQU  PMTDOCMH-PMTPGMRH                                                 
NAMDISPQ EQU  PMTNAMEH-PMTPGMRH                                                 
EXTDISPQ EQU  PMTEXTNH-PMTPGMRH                                                 
HOMDISPQ EQU  PMTHOMEH-PMTPGMRH                                                 
       ++INCLUDE CTMSTAFFD                                                      
         EJECT                                                                  
       ++INCLUDE CTSFMWORKD                                                     
         EJECT                                                                  
*                                                                               
*                                                                               
* START OF SAVED STORAGE (6144)                                                 
*                                                                               
         ORG   SYSSPARE                                                         
STAFID   DS    CL(L'MSTFID)          SYSTEM ID                                  
SYSNAM   DS    CL7                   SYSTEM CODE                                
PGNAME   DS    CL8                   PROGRAM NAME                               
FLAG     DS    XL1                   LIST SCREEN FIELDS INPUT                   
REMK     DS    CL1                   PRINT/SURPRESS REMARKS                     
RPTYPE   DS    CL1                   REPORT TYPE->SETS RCSUBPRG                 
STFLTR   DS    CL8                   HOLDS STAFILTER FOR LIST SCREEN            
DPTFLT   DS    CL4                   HOLDS DPT FILTER FOR REPORT2               
TMPWRK   DS    CL8                   TEMPORARY WORKING VARIABLE                 
ENDADR   DS    A                     ADDRESS OF TABLE END                       
HLF      DS    H                     FOR MULTIPLY ETC INSTR                     
*                                                                               
PRT2DPT  DS    CL12                  CHAR STRING EQUIV OF DPT CODE              
SAVDPT   DS    X                     SAVED DPT FOR RPT PG-BRK TEST              
SAVSTF   DS    CL8                   SAVED STAFF ID FOR RPT PG-BRK TEST         
SAVSYS   DS    CL7                   SAVED SYSTEM FOR RPT SPACING TEST          
*                                                                               
SYSPROG  DS    0CL5                  SYSTEM/PROGRAM                             
SYSCODE  DS    CL2                                                              
PROGCODE DS    CL3                                                              
DATATYPE DS    CL7                   DATATYPE NAME                              
OLDKEYSV DS    CL(L'KEY)             HOLDS SAVEKEY FOR ELEMENT                  
OLDKEY   DS    CL(L'KEY)             HOLDS KEY FOR ELEMENT                      
SAVEKEY  DS    CL(L'KEY)             GENFILE KEY                                
PRVELMT  DS    A                                                                
*                                                                               
* ADDITIONAL ERROR MESSAGES CONCERNING INVALID INPUT FIELDS                     
TOOMNY   EQU   19                    TOO MANY FIELD INPUT                       
INVSTF   EQU   154                   INVALID USER ID                            
*                                                                               
         SPACE 5                                                                
SRTREC   DS    2104X               SORT AREA                                    
         ORG   SRTREC                                                           
SRTLN    DS    H                   RECORD LENGTH                                
         DS    H'0'                FOR IBM SORT UTILITY USE                     
SRTKEY   DS    0C                  BEG OF OUR KEY                               
SRTDPT   DS    XL1                 SORT KEY- DEPT                               
SRTSTF   DS    CL8                 SORT KEY- STAFF                              
SRTSYS   DS    CL7                 SORT KEY- SYSTEM                             
SRTPRG   DS    CL8                 SORT KEY- PROGRAM                            
SRTOVQ   EQU   *-SRTREC            LENGTH OF OVERHEAD (INCL REC LN)             
SRTKEYQ  EQU   *-SRTKEY            LENGTH OF SORT KEY (EXCL REC LN)             
SRTDATA  DS    0X                  DATA- ELEMENTS FROM RECORD                   
*                                                                               
         SPACE 5                                                                
* ON-SCREEN LIST LINE                                                           
*                                                                               
GEND     DSECT                                                                  
         ORG   LISTAR                                                           
         DS    CL3                                                              
LSTSYS   DS    CL7                 SYSTEM NAME                                  
         DS    CL3                                                              
LSTPRG   DS    CL8                 PROGRAM NAME                                 
         DS    CL2                                                              
LSTPRGMR DS    CL8                 PROGRAMMER STAFF ID                          
         DS    CL4                                                              
LSTEXTN  DS    CL4                 PROGRAMMER'S EXTENSION                       
         DS    CL2                                                              
LSTDSCR  DS    CL32                DESCRIPTION                                  
*                                                                               
         SPACE 5                                                                
* ***************************                                                   
* PRINT LINE -- REPORT TYPE 1                                                   
* ***************************                                                   
*                                                                               
SPOOLD   DSECT                                                                  
         ORG   P                                                                
         DS    C                   LEFT BOX MARGIN                              
PRTPROG  DS    CL8                 PROGRAM                                      
         DS    C                                                                
PRTDESC  DS    CL40                DESCRIPTION                                  
         DS    C                                                                
PRTPGM   DS    CL8                 PROGRAMMER                                   
         DS    CL2                 '-X' BEFORE EXTENSION                        
         DS    CL4                 EXTN                                         
         DS    C                                                                
PRTDOC   DS    CL8                 TECHNICAL WRITER                             
         DS    CL2                 '-X' BEFORE EXTENSION                        
         DS    CL4                 EXTN                                         
         DS    C                                                                
PRTSER   DS    CL8                 CLIENT SERVICE                               
         DS    CL2                 '-X' BEFORE EXTENSION                        
         DS    CL4                 EXTN                                         
         DS    C                                                                
PRTCAL   DS    CL8                 CALL SUPPORT                                 
         DS    CL2                 '-X' BEFORE EXTENSION                        
         DS    CL4                 EXTN                                         
         DS    C                                                                
PRTBAK   DS    CL8                 PRGMR BACKUP                                 
         DS    CL2                 '-X' BEFORE EXTENSION                        
         DS    CL4                 EXTN                                         
         DS    C                                                                
         SPACE 3                                                                
*REMARK LINE ONE:                                                               
         ORG   P2                                                               
         DS    CL12                LEFT BOX, PROGRAM                            
PRTRMK1  DS    CL60                REMARK1                                      
         SPACE 3                                                                
*REMARK LINE TWO:                                                               
         ORG   P3                                                               
         DS    CL12                LEFT BOX, PROGRAM                            
PRTRMK2  DS    CL60                REMARK1                                      
         SPACE 3                                                                
*REMARK LINE THREE:                                                             
         ORG   P4                                                               
         DS    CL12                LEFT BOX, PROGRAM                            
PRTRMK3  DS    CL60                REMARK1                                      
         SPACE 3                                                                
* ***************************                                                   
* PRINT LINE -- REPORT TYPE 2                                                   
* ***************************                                                   
         ORG   P                                                                
         DS    C                   LEFT BOX MARGIN                              
PRT2SYS  DS    CL7                 SYSTEM                                       
         DS    C                                                                
PRT2PRG  DS    CL8                 PROGRAM                                      
         DS    C                                                                
PRT2DSC  DS    CL60                DESCRIPTION                                  
         DS    C                   RIGHT BOX MARGIN                             
         SPACE 3                                                                
PRT2DISQ EQU   (PRT2DSC-P)-(PRTRMK1-P2)+2                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'060CTSFM57   05/01/02'                                      
         END                                                                    
