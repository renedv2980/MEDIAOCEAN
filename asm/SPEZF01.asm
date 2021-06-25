*          DATA SET SPEZF01    AT LEVEL 021 AS OF 12/01/15                      
*PHASE T23001A                                                                  
         TITLE 'T23001 - AGENCY NAME RECORD'                                    
***********************************************************************         
*                                                                     *         
*  TITLE: T23001 - EASI AGENCY NAME RECORDS                           *         
*  COMMENTS: THIS PROGRAM DOES MAINT AND LIST FOR AGENCY NAME RECS    *         
*            WHICH ARE STORED ON GENDIR/GENFIL, AND ONLY MAINTAINED   *         
*            BY DATA CONTROL.                                         *         
*  OUTPUTS: UPDATED AGENCY NAME RECORDS                               *         
*  LOCALS: REGISTER USAGE                                             *         
*          R2 - POINTER TO SCREEN FLDH, ERREX WILL POSITION CUSOR     *         
*          R3 -                                                       *         
*          R4 - WORK REG & KEY DSECT POINTER                          *         
*          R5 - WORK REG & POINTER TO PROGRAM/UNIT TABLES             *         
*          R6 - USED FOR GETEL ELEMENT DSECT POINTER                  *         
*          R7 - NOT USED                                              *         
*          R8 - POINTER TO SPOOLD                                     *         
*          R9 - POINTER TO SYSD                                       *         
*          RA - POINTER TO ATWA                                       *         
*          RB - FIRST BASE                                            *         
*          RC - POINTER TO GEND                                       *         
* AIO USAGE - AIO1 - VALI RTNS IN CONTROLLER (SPEZF00-T23000)         *         
*                  - IN AIO FROM HYPER CONTROLLER (T00A30)            *         
*             AIO2 -                                                  *         
*             AIO3 -                                                  *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
***********************************************************************         
*                                                                     *         
*  LEV 08    APR04/90 SHOW AGENCY SIGNON IN LIST                      *         
*  LEV 09    NOV07/91 CHANGE FASWITCH                                 *         
*  LEV 10    FEB05/92 ADD ALLOW MULTI MEDIAS                          *         
*  LEV 11    FEB17/92 FIX BUG - NOT SHOWING AGENCY NAME               *         
*  LEV 12-13 APR01/92 SHOW BLANK AS ALL                               *         
*  LEV 14    MAY13/92 FIX OFFLINE LIST                                *         
*  LEV 15    FEB11/93 ALLOW ALL, BUT DO NOT USE VALISTA IF ALL        *         
*  LEV 16    APR13/94 ONLY ALLOW DDS TERMINALS                        *         
*  LEV 17    MAY20/94 FIX SCANNER FOR OPTIONS                         *         
*  LEV 18    FEB13/96 ALLOW OFFLINE WITHOUT DDS TEST                  *         
*                                                                     *         
***********************************************************************         
         EJECT                                                                  
T23001   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**3001**                                                       
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     R8,ASPOOLD          GENERAL PRINT AREAS                          
         USING SPOOLD,R8                                                        
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA      BASE SCREEN FOR SYSTEM + THIS PROG           
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         MVC   AIO,AIO1                                                         
         SPACE                                                                  
         CLI   MODE,VALKEY         VALIDATE RECORD KEY                          
         BE    VKEY                                                             
         CLI   MODE,VALREC         VALIDATE RECORD                              
         BE    VREC                                                             
         CLI   MODE,DISPKEY        DISPLAY RECORD                               
         BE    DKEY                                                             
         CLI   MODE,DISPREC        DISPLAY RECORD                               
         BE    DREC                                                             
         CLI   MODE,LISTRECS       LIST RECORDS                                 
         BE    LIST                                                             
         CLI   MODE,PRINTREP       PRINT RECORDS                                
         BE    LIST                                                             
*                                                                               
XIT      XIT1                                                                   
         EJECT                                                                  
* VALIDATE KEY ROUTINE                                                          
         SPACE                                                                  
VKEY     EQU   *                                                                
         CLI   OFFLINE,C'Y'        IF OFFLINE, NO DDS TEST                      
         BE    *+12                                                             
         CLI   1(RA),C'*'          ONLY DDS TERMINALS                           
         BNE   INVALER                                                          
         SPACE                                                                  
         CLI   ACTNUM,ACTLIST      IGNORE IF NOT LIST                           
         BE    VK300                                                            
         LA    R2,DANANMH          NAME MANDATORY                               
         CLI   5(R2),0                                                          
         BNE   VK100                                                            
         XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(25),=C'* ERROR * NAME REQUIRED *'                        
         B     MYERR                                                            
*                                                                               
VK100    XC    KEY,KEY             SET UP KEY/SVKEY                             
         MVC   KEY(2),=C'ZA'                                                    
         MVC   KEY+7(25),SPACES                                                 
         ZIC   RE,5(R2)                                                         
         BCTR  RE,0                                                             
         EX    RE,MVCNAM                                                        
         MVC   SVKEY,KEY                                                        
         B     VKXIT                                                            
*                                                                               
VK300    LA    R2,DANSRTH                                                       
         SPACE                                                                  
         XC    OPTNAM,OPTNAM       START AGY NAME                               
         CLI   5(R2),0             ANY INPUT                                    
         BE    VK400                                                            
         SPACE                                                                  
         MVC   OPTNAM,8(R2)                                                     
*                                                                               
VK400    LA    R2,DANOPTH                                                       
         SPACE                                                                  
         XC    CUUID,CUUID         USER ID FILTER                               
         XC    CUSTA,CUSTA         STATION FILTER                               
         SPACE                                                                  
         CLI   5(R2),0             ANY INPUT                                    
         BE    VKXIT                                                            
         SPACE                                                                  
         CLI   8(R2),C'?'          NEED HELP                                    
         BE    OPTHLP                                                           
         SPACE                                                                  
         MVI   ERRFLD,1                                                         
         LA    R6,2                                                             
         LA    R4,WORK                                                          
         XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(26),=C'* ERROR * INVALID OPTION *'                       
         GOTO1 SCANNER,DMCB,(R2),(2,(R4)),0                                     
         CLI   4(R1),0                                                          
         BE    MYERR                                                            
         ZIC   R6,4(R1)            GET NUMBER OF BLOCKS                         
*                                                                               
VK500    CLI   0(R4),0                                                          
         BE    VKXIT                                                            
         CLI   0(R4),1                                                          
         BNE   MYERR                                                            
*                                                                               
*                                  USER CODE                                    
         CLI   12(R4),C'U'                                                      
         BNE   VK600                                                            
         XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(27),=C'* ERROR * INVALID USER ID *'                      
         CLI   1(R4),8                                                          
         BH    VKERR                                                            
         MVC   CUUID,22(R4)                                                     
         B     VK700                                                            
*                                                                               
*                                  STATION NO REAL VALIDATION                   
VK600    CLI   12(R4),C'S'                                                      
         BNE   VKERR                                                            
         XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(27),=C'* ERROR * INVALID STATION *'                      
         CLI   1(R4),7                                                          
         BH    VKERR                                                            
         MVC   CUSTA(4),22(R4)                                                  
         MVI   CUSTA+4,C' '                                                     
         LA    RE,25(R4)                                                        
         CLI   0(RE),C'-'                                                       
         BNE   VK620                                                            
         LA    RE,1(RE)                                                         
         MVI   CUSTA+3,C' '                                                     
         B     *+8                                                              
VK620    LA    RE,2(RE)                                                         
         CLI   0(RE),C'T'                                                       
         BE    VK700                                                            
         MVC   CUSTA+4(1),0(RE)                                                 
*                                                                               
*                                  UP ERROR FIELD & NEXT BLOCK                  
VK700    ZIC   RE,ERRFLD                                                        
         LA    RE,1(RE)                                                         
         STC   RE,ERRFLD                                                        
         LA    R4,32(R4)                                                        
         BCT   R6,VK500                                                         
         MVC   CONHEAD,SPACES                                                   
*                                                                               
VKXIT    B     XIT                                                              
*                                                                               
MVCUID   MVC   KEY+15(0),8(R2)                                                  
MVCNAM   MVC   KEY+7(0),8(R2)                                                   
*                                                                               
OPTHLP   XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'OPTHELP),OPTHELP                                       
         B     MYERR                                                            
VKERR    OI    ERRFLD,X'F0'                                                     
         MVC   CONHEAD+17(5),=C'FIELD'                                          
         MVC   CONHEAD+23(1),ERRFLD                                             
MYERR    LA    R2,DANOPTH                                                       
MYERR2   MVI   GENSTAT2,USMYOK                                                  
         GOTO1 ERREX2                                                           
         EJECT                                                                  
*              VALIDATE RECORD ROUTINE                                          
         SPACE                                                                  
VREC     EQU   *                                                                
*        LA    R5,32               MAX SCREEN ENTRIES                           
         LA    R5,33               MAX SCREEN ENTRIES                           
         L     R6,AIO1                                                          
         USING EZANMD,R6                                                        
         MVC   EZAKTYP(32),SVKEY   KEY                                          
         MVC   EZALEN,=H'43'       LENGTH = 42(KEY) +1(END 0)                   
         SPACE                                                                  
         MVI   ELCODE,02                                                        
         GOTO1 REMELEM                                                          
         SPACE                                                                  
         LA    R2,DANUSRH          VALIDATE USER ID                             
         CLI   5(R2),0             1ST REQUIRED                                 
         BNE   VR100                                                            
         MVC   CONHEAD(16),=C'USER ID REQUIRED'                                 
         B     MYERR2                                                           
*                                                                               
VR100    CLI   5(R2),0                                                          
         BNE   VR120                                                            
         ZIC   RE,0(R2)                                                         
         AR    R2,RE                                                            
         CLI   5(R2),0                                                          
         BE    VR220                                                            
         MVC   CONHEAD(24),=C'STATION REQUIRES USER ID'                         
         B     MYERR2                                                           
*                                                                               
VR120    CLI   5(R2),0                                                          
         BE    VR220                                                            
*                                                                               
         MVC   AIO,AIO2                                                         
         XC    KEY,KEY                                                          
         MVI   KEY,C'I'                                                         
         MVC   KEY+15(10),SPACES                                                
         ZIC   RE,5(R2)                                                         
         BCTR  RE,0                                                             
         EX    RE,MVCUID                                                        
         GOTO1 DATAMGR,DMCB,=C'DMREAD ',=C'CTFILE ',KEY,AIO                     
         MVC   AIO,AIO1                                                         
         CLI   8(R1),0                                                          
         BE    VR140                                                            
         MVC   CONHEAD(15),=C'INVALID USER ID'                                  
         B     MYERR2                                                           
*                                                                               
VR140    L     R6,AIO2                                                          
         MVC   DATADISP,=H'28'                                                  
         MVI   ELCODE,X'02'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    R4,ELEM                                                          
         USING EZAIDEL,R4                                                       
         XC    ELEM,ELEM                                                        
         MVC   0(2,R4),=X'0213'                                                 
         MVC   EZAUID,KEY+15      SAVE USER ID                                  
         MVC   EZAORIG,2(R6)       BINARY USER ID (ORIGIN)                      
         SPACE                                                                  
         ZIC   RE,0(R2)            NEXT FIELD ON SCREEN                         
         AR    R2,RE                                                            
         SPACE                                                                  
* DOESN'T READ STATION FROM STATION FILE, JUST CHECKS REASONABLE *              
         SPACE                                                                  
         CLI   5(R2),0                                                          
         BE    MISSERR                                                          
         SPACE                                                                  
         MVI   QMED,C'A'           ALLOW ALL MEDIAS                             
         SPACE                                                                  
         CLC   =C'ALL-',8(R2)      THIS ALL STATIONS                            
         BNE   VR160                                                            
*                                                                               
         CLI   12(R2),C'A'         THIS ALL MEDIAS                              
         BE    VR150                                                            
*                                                                               
         LA    R1,12(R2)           MEDIA CHARACTER                              
         ICM   R1,8,=AL1(EZMTMEDQ) LOOK FOR MEDIA                               
         GOTO1 VGETMED                                                          
         BNE   BDMEDERR                                                         
*                                                                               
VR150    MVC   QSTA(4),=C'ALL '                                                 
         MVC   QSTA+4(1),12(R2)                                                 
         CLI   QSTA+4,C'A'       THIS ALL MEDIA'S                               
         BNE   *+8                  NO                                          
         MVI   QSTA+4,C' '       FORCE TO BLANK                                 
         B     VR164                                                            
*                                                                               
VR160    DS    0H                                                               
         GOTO1 VREADSTA                                                         
         BNE   BDSTAERR                                                         
         MVC   QSTA,FLDRDSTA                                                    
*                                                                               
VR164    MVC   EZASTA,QSTA                                                      
         MVC   DATADISP,=H'42'                                                  
         GOTO1 ADDELEM                                                          
         DROP  R4                                                               
         SPACE                                                                  
VR220    ZIC   RE,0(R2)                                                         
         AR    R2,RE                                                            
         BCT   R5,VR100                                                         
*                                                                               
* UPDATING OPTIONS ELEMENT ON AGYNAME RECORD                                    
         MVI   REDRFLAG,C'N'                                                    
         TM    DANREDH+4,X'20'     FIELD VALIDATED?                             
         BO    VR500               YES - JUST SKIP IT                           
*                                                                               
         LA    R2,DANREDH                                                       
         MVC   CONHEAD(15),=CL15'INVALID FIELD'                                 
         CLI   DANREDH+5,0         FIELD CLEARED?                               
         BE    VR300               YES                                          
         CLC   =C'SPOT',DANRED                                                  
         BNE   MYERR2                                                           
         CLI   DANREDH+5,4                                                      
         BNE   MYERR2                                                           
         MVI   REDRFLAG,C'S'                                                    
*                                                                               
VR300    DS    0H                  SEE IF WE HAVE OPTIONS ELEMENT               
         L     R6,AIO                                                           
         MVI   ELCODE,X'07'                                                     
         MVC   DATADISP,=X'002A'   42 FOR GENDIR/GENFIL                         
         BAS   RE,GETEL                                                         
         BNE   VR320               NOT THERE                                    
*                                                                               
* FOUND THE OPTIONS ELEMENT                                                     
         NI    EZOPTFL-EZOPTEL(R6),X'FF'-EZOPTFSQ                               
         CLI   REDRFLAG,C'S'                                                    
         BNE   *+8                                                              
         OI    EZOPTFL-EZOPTEL(R6),EZOPTFSQ                                     
*                                                                               
         OI    DANREDH+4,X'20'     FIELD VALIDATED                              
         B     VR500               DONE                                         
*                                                                               
VR320    DS    0H                  ELEMENT NOT THERE                            
         OI    DANREDH+4,X'20'     FIELD VALIDATED                              
         CLI   REDRFLAG,C'S'       NEED TO ADD IT?                              
         BNE   VR500               NO - WE'RE DONE                              
*                                                                               
         LA    R4,ELEM                                                          
         USING EZOPTEL,R4                                                       
         XC    ELEM,ELEM                                                        
         MVC   0(2,R4),=X'070A'                                                 
         OI    EZOPTFL,EZOPTFSQ                                                 
         GOTO1 ADDELEM                                                          
         DROP  R4                                                               
*                                                                               
VR500    CLI   ACTNUM,ACTADD                                                    
         BE    VRXIT                                                            
         SPACE                                                                  
         MVC   KEY,SVKEY                                                        
         MVC   AIO,AIO2                                                         
         GOTO1 HIGH                                                             
         CLC   KEY(32),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 GETREC                                                           
         MVC   AIO,AIO1                                                         
VRXIT    B     DREC                                                             
         DROP  R6                                                               
         EJECT                                                                  
*                                  DISPLAY KEY                                  
DKEY     EQU   *                                                                
         LA    R2,DANANMH                                                       
         MVC   8(25,R2),KEY+7                                                   
         OI    6(R2),X'80'                                                      
         MVC   SVKEY,KEY                                                        
DKXIT    B     XIT                                                              
         SPACE 2                                                                
* DISPLAY RECORD ROUTINE *                                                      
         SPACE                                                                  
DREC     EQU   *                                                                
         LA    R2,DANUSRH                                                       
         BAS   RE,CLRSCRN                                                       
*                                                                               
         MVC   DANRED,SPACES                                                    
         OI    DANREDH+6,X'80'                                                  
         L     R6,AIO                                                           
         MVI   ELCODE,X'07'        OPTIONS ELEMENT                              
         BAS   RE,GETEL                                                         
         BNE   DR050               NOT THERE - LEAVE FIELD BLANK                
*                                                                               
         TM    EZOPTFL-EZOPTEL(R6),EZOPTFSQ                                     
         BZ    DR050                                                            
         MVC   DANRED(4),=C'SPOT'                                               
*                                                                               
DR050    DS    0H                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,X'02'                                                     
         BAS   RE,GETEL                                                         
         BE    *+8                                                              
DR100    BAS   RE,NEXTEL                                                        
         BNE   DRXIT                                                            
         USING EZAIDEL,R6                                                       
         MVC   8(8,R2),EZAUID                                                   
         OI    6(R2),X'80'                                                      
         ZIC   RE,0(R2)            JUMP TO NEXT FIELD ON SCREEN                 
         AR    R2,RE                                                            
*                                                                               
         OC    EZASTA,EZASTA                                                    
         BZ    DR200                                                            
*                                                                               
         OI    6(R2),X'80'                                                      
*                                                                               
         CLC   =C'ALL ',EZASTA                                                  
         BNE   DR120                                                            
*                                                                               
         MVC   8(5,R2),EZASTA                                                   
         MVI   11(R2),C'-'                                                      
         CLI   12(R2),C' '                                                      
         BH    *+8                                                              
         MVI   12(R2),C'A'         BLANK MEANS ALL MEDIA                        
         B     DR200                                                            
*                                                                               
DR120    DS    0H                                                               
         LA    R1,EZASTA                                                        
         GOTO1 VPRNTSTA                                                         
         MVC   8(L'PRTSTA7C,R2),PRTSTA7C    STATION                             
*                                                                               
DR200    ZIC   RE,0(R2)                                                         
         AR    R2,RE                                                            
         B     DR100                                                            
*                                                                               
DRXIT    B     XIT                                                              
         EJECT                                                                  
*              LIST RECORDS                                                     
LIST     EQU   *                                                                
         CLI   MODE,PRINTREP                                                    
         BNE   LS100                                                            
         LA    R1,HEADING                                                       
         ST    R1,SPECS                                                         
         LA    R1,HDRTN                                                         
         ST    R1,HEADHOOK                                                      
         SPACE                                                                  
LS100    LA    R4,KEY                                                           
         OC    KEY,KEY             WAS A KEY PROVIDED?                          
         BNZ   LS120                                                            
         MVC   KEY(2),=C'ZA'                                                    
         MVC   KEY+7(8),OPTNAM     AGY NAME                                     
*                                                                               
LS120    GOTO1 HIGH                                                             
         CLC   KEY(2),=C'ZA'        KEY ID                                      
         BNE   XIT                                                              
         B     LS220                                                            
         SPACE                                                                  
LS200    GOTO1 SEQ                                                              
         SPACE                                                                  
LS220    CLC   KEY(2),KEYSAVE       KEY ID                                      
         BNE   XIT                                                              
         GOTO1 GETREC                                                           
         L     R6,AIO                                                           
         MVI   ELCODE,X'02'                                                     
         BAS   RE,GETEL                                                         
         B     *+8                                                              
LS300    BAS   RE,NEXTEL                                                        
         BNE   LS200                                                            
         USING EZAIDEL,R6                                                       
*                                                                               
* IF FILTERS MATCH ON FILTER                                                    
*                                                                               
         OC    CUSTA,CUSTA         STATION                                      
         BZ    *+14                                                             
         CLC   EZASTA,CUSTA                                                     
         BNE   LS300                                                            
         OC    CUUID,CUUID         USER ID                                      
         BZ    *+14                                                             
         CLC   EZAUID,CUUID                                                     
         BNE   LS300                                                            
         MVC   LISTAR,SPACES                                                    
         MVC   LNAME,KEY+7         AGENCY NAME                                  
*                                                                               
LS500    L     R6,AIO                                                           
         LA    R5,LUID                                                          
         MVI   ELCODE,X'02'                                                     
         BAS   RE,GETEL                                                         
         BNE   LS550                                                            
         USING EZAIDEL,R6                                                       
LS520    LA    R0,LUID                                                          
         CR    R5,R0               IS THIS THE 1ST ONE                          
         BE    *+12                 YES                                         
         MVI   0(R5),C','                                                       
         LA    R5,1(,R5)                                                        
         SPACE                                                                  
         MVC   0(L'EZAUID,R5),EZAUID                                            
         LA    R1,L'EZAUID-1(,R5)                                               
         SPACE                                                                  
         CLI   0(R1),C' '          FIND END OF USER ID                          
         BH    *+8                                                              
         BCT   R1,*-8                                                           
         SPACE                                                                  
         MVI   1(R1),C'-'                                                       
         LA    R5,2(,R1)                                                        
*                                                                               
         CLC   =C'ALL ',EZASTA                                                  
         BNE   LS530                                                            
* ALL STATIONS HERE                                                             
         MVC   0(L'EZASTA,R5),EZASTA                                            
         MVI   3(R5),C'-'                                                       
         CLI   4(R5),C' '                                                       
         BH    *+8                                                              
         MVI   4(R5),C'A'                                                       
         LA    R5,5(R5)                                                         
         B     LS540                                                            
*                                                                               
* SPECIFIC STATION HERE                                                         
LS530    DS    0H                                                               
         LA    R1,EZASTA                                                        
         GOTO1 VPRNTSTA                                                         
         MVC   0(L'PRTSTA7C,R5),PRTSTA7C    STATION                             
         LA    R5,L'PRTSTA7C-1(,R5)                                             
*                                                                               
LS540    DS    0H                                                               
         CLI   0(R5),C' '                                                       
         BH    *+8                                                              
         BCT   R5,*-8                                                           
         LA    R5,1(R5)                                                         
*                                                                               
         BAS   RE,NEXTEL                                                        
         BNE   LS550                                                            
*                                                                               
         LA    R0,LUID+33                                                       
         CR    R5,R0               IF MORE SPACE IS LEFT                        
         BL    LS520                GET NEXT ELEM                               
         MVC   1(8,R5),=C'..MORE..'                                             
*                                                                               
LS550    CLI   MODE,PRINTREP                                                    
         BE    LS600                                                            
         MVC   DMDSKADD,KEY+36                                                  
         GOTO1 LISTMON                                                          
         B     LS200                                                            
         SPACE                                                                  
LS600    MVC   P+2(80),LISTAR                                                   
         GOTO1 SPOOL,DMCB,(R8)                                                  
         XC    LISTAR,LISTAR                                                    
         B     LS200                                                            
         DROP  R6                                                               
         EJECT                                                                  
HDRTN    NTR1                                                                   
         B     XIT                                                              
         SPACE                                                                  
CLRSCRN  NTR1                                                                   
*                                                                               
*        ROUTINE TO CLEAR THE SCREEN                                            
*        FROM FIELD AT R2                                                       
*                                                                               
         SR    RE,RE                                                            
*                                                                               
CS010    IC    RE,0(R2)                                                         
         SH    RE,=H'9'                                                         
         EX    RE,CSCLC                                                         
         BE    CS020                                                            
         EX    RE,CSOC                                                          
         BZ    CS020                                                            
         EX    RE,CSXC                                                          
         OI    6(R2),X'80'                                                      
*                                                                               
CS020    LA    R2,9(RE,R2)                                                      
         CLI   0(R2),9                                                          
         BH    CS010                                                            
         B     XIT                                                              
*                                                                               
CSCLC    CLC   8(0,R2),SPACES                                                   
CSOC     OC    8(0,R2),8(R2)                                                    
CSXC     XC    8(0,R2),8(R2)                                                    
         SPACE                                                                  
         GETEL R6,DATADISP,ELCODE                                               
         SPACE                                                                  
BDMEDERR XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'BDMEDMS),BDMEDMS                                       
         GOTO1 ERREX2                                                           
BDSTAERR XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'BDSTAMS),BDSTAMS                                       
         GOTO1 ERREX2                                                           
         SPACE                                                                  
INVALER  MVI   ERROR,INVALID                                                    
         LA    R2,CONRECH                                                       
         B     TRAPERR                                                          
         SPACE                                                                  
MISSERR  MVI   ERROR,MISSING                                                    
         SPACE                                                                  
TRAPERR  GOTO1 ERREX                                                            
         DC    H'0'                                                             
         EJECT                                                                  
         LTORG                                                                  
         SPACE                                                                  
OPTHELP  DC    C'OPTIONS= U-USER ID, S-STATION *'                               
BDMEDMS  DC    C'* ERROR * MEDIA MUST BE T, R, N, X, OR A *'                    
BDSTAMS  DC    C'* ERROR * INVALID STATION '                                    
         SPACE                                                                  
HEADING  SSPEC H1,3,REQUESTOR                                                   
         SSPEC H1,48,C'AGENCY NAME RECORDS'                                     
         SSPEC H2,46,C'-----------------------'                                 
         SSPEC H1,93,AGYNAME                                                    
         SSPEC H2,93,AGYADD                                                     
         SSPEC H3,93,REPORT                                                     
         SSPEC H4,93,RUN                                                        
         SSPEC H5,103,PAGE                                                      
         SSPEC H8,3,C'NAME'                                                     
         SSPEC H9,3,C'----'                                                     
         SSPEC H8,30,C'CLIENT/STATION'                                          
         SSPEC H9,30,C'--------------'                                          
         DC    X'00'                                                            
         EJECT                                                                  
* DDSPOOLD                                                                      
* DDSPLWORKD                                                                    
* SPEZFFFD                                                                      
         PRINT OFF                                                              
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE SPEZFFFD                                                       
         PRINT ON                                                               
         ORG   CONTAGH                                                          
* SPEZFFED                                                                      
       ++INCLUDE SPEZFFED                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
* SPEZFEED                                                                      
       ++INCLUDE SPEZFEED                                                       
         EJECT                                                                  
* SPGENSTA                                                                      
       ++INCLUDE SPGENSTA                                                       
         EJECT                                                                  
* SPGENEZ                                                                       
       ++INCLUDE SPGENEZ                                                        
         EJECT                                                                  
*SPEZFWORKD                                                                     
         PRINT OFF                                                              
       ++INCLUDE SPEZFWORKD                                                     
         PRINT ON                                                               
*                                                                               
       ++INCLUDE SPEZDSCTS                                                      
*                                                                               
SYSD     DSECT                                                                  
         ORG   SYSSPARE                                                         
ERRFLD   DS    CL1                 ERROR FIELD                                  
CUUID    DS    CL8                 USER ID FILTER                               
CUSTA    DS    CL5                 STATION FILTER                               
OPTNAM   DS    CL8                 START NAME                                   
SVUID    DS    CL8                 USER ID FROM CONTROL SYSTEM                  
SVORG    DS    CL2                 ORIGIN FROM CONTROL SYSTEM                   
REDRFLAG DS    C                   VALUES=N,S                                   
*                                                                               
WRKFEND  EQU   *                                                                
*                                                                               
*                                                                               
GEND     DSECT                                                                  
         ORG   LISTAR                                                           
LNAME    DS    CL25                                                             
         DS    CL1                                                              
LUID     DS    CL45                                                             
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'021SPEZF01   12/01/15'                                      
         END                                                                    
