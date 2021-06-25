*          DATA SET SPEZF0A    AT LEVEL 030 AS OF 09/26/19                      
*PHASE T2300AA                                                                  
         TITLE 'T2300A - ADVNAM RECORD'                                         
***********************************************************************         
*                                                                     *         
*  TITLE: T2300A - EASI ADVNAM RECORDS                                *         
*  COMMENTS: THIS PROGRAM DOES MAINT AND LIST FOR ADVNAM IDENT RECS   *         
*  OUTPUTS: UPDATED ADVNAM NAME RECORDS                               *         
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
*  LEV  2    NOV05/96 NEW                                             *         
*  LEV  3    MAR12/97 STOP OVERRUN INTO NEXT REC TYPE IN LIST         *         
*  LEV  4    JUN13/97 ADD DF TO BS                                    *         
*  LEV  5    NOV05/97 ADD JW TO FR                                    *         
*  LEV  6    JAN13/98 ADD L TO MEDTBL                                 *         
*  LEV  7    MAR02/99 ADD SJ TO MC AS OKAY TRANSFER                   *         
*  LEV  8    JUL19/00 ADD MEDIA X                                     *         
*  LEV  9    SEP11/00 ADD BT TO PCTABLE1                              *         
*  LEV 10    JAN18/00 ADD H7 TO PCTABLE3 JW/FR/H7                     *         
*  LEV 10    NOV02/01 ADD PCTABLE5 LM/GZ                              *         
*  LEV 11    FEB06/02 ADD MEDIA N                                     *         
*                                                                     *         
***********************************************************************         
         EJECT                                                                  
T2300A   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**300A**                                                       
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
*                                                                               
EQXIT    CR    RB,RB                                                            
         J     EXIT                                                             
NEQXIT   LTR   RB,RB                                                            
EXIT     XIT1                                                                   
*                                                                               
* READ ADVNAM FROM MEDIA MPL FILE                                               
*                                                                               
VKEY     DS   0H                                                                
         LA    R2,DADNAMH                                                       
         CLI   ACTNUM,ACTLIST      UNLESS A LIST NOT MANDATORY                  
         BE    VK200                                                            
         CLI   5(R2),0             NAME MANDATORY                               
         BNE   VK100                                                            
         XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(29),=C'** ERROR ** ADVNAM REQUIRED *'                    
         B     MYERR                                                            
         SPACE                                                                  
VK100    XC    KEY,KEY             SET UP KEY/SVKEY                             
         MVC   KEY(2),=C'ZD'                                                    
         MVC   KEY+2(2),AGENCY                                                  
         MVC   KEY+7(25),SPACES                                                 
         ZIC   RE,5(R2)                                                         
         BCTR  RE,0                                                             
         EX    RE,MVCNAM                                                        
         MVC   SVKEY,KEY                                                        
         B     VK600                                                            
*                                                                               
VK200    LA    R2,DADSRTH                                                       
         XC    OPADV,OPADV                                                      
         CLI   5(R2),0                                                          
         BE    *+10                                                             
         MVC   OPADV,8(R2)                                                      
*                                                                               
         LA    R2,DADOPTH                                                       
         CLI   5(R2),0                                                          
         BE    VK600                                                            
         MVI   ERRFLD,1                                                         
         LA    R6,2                                                             
         LA    R4,WORK                                                          
         XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(28),=C'** ERROR ** INVALID OPTION *'                     
         GOTO1 SCANNER,DMCB,(R2),(2,(R4)),0                                     
         CLI   4(R1),0                                                          
         BE    MYERR                                                            
         SPACE                                                                  
VK300    CLI   0(R4),0                                                          
         BZ    VK600                                                            
         CLI   0(R4),1                                                          
         BNE   MYERR                                                            
         SPACE                                                                  
         CLI   12(R4),C'A'         ADVNAM CODE                                  
         BNE   VK400                                                            
         MVC   CONHEAD(28),=C'** ERROR ** INVALID ADVNAM *'                     
         CLI   1(R4),2                                                          
         BL    VKERR                                                            
         MVC   OPADV,22(R4)                                                     
         SPACE                                                                  
VK400    CLI   12(R4),C'S'         STATION CODE                                 
         BNE   VKERR                                                            
         MVC   CONHEAD(29),=C'** ERROR ** INVALID STATION *'                    
         CLI   1(R4),7                                                          
         BH    VKERR                                                            
         MVC   OPSTA(4),22(R4)                                                  
         MVI   OPSTA+4,C' '                                                     
         LA    RE,25(R4)                                                        
         CLI   0(RE),C'-'                                                       
         BNE   VK420                                                            
         LA    RE,1(RE)                                                         
         MVI   OPSTA+3,C' '                                                     
         B     *+8                                                              
VK420    LA    RE,2(RE)                                                         
         CLI   0(RE),C'T'                                                       
         BE    VK500                                                            
         MVC   OPSTA+4(1),0(RE)                                                 
*                                                                               
*                                  UP ERROR FIELD & NEXT BLOCK                  
VK500    ZIC   RE,ERRFLD                                                        
         LA    RE,1(RE)                                                         
         STC   RE,ERRFLD                                                        
         LA    R4,32(R4)                                                        
         BCT   R6,VK300                                                         
*                                                                               
VK600    MVC   CONHEAD,SPACES                                                   
*                                                                               
VKXIT    B     EXIT                                                             
MVCNAM   MVC   KEY+7(0),8(R2)                                                   
*                                                                               
VKERR    OI    ERRFLD,X'F0'                                                     
         MVC   CONHEAD+30(5),=C'FIELD'                                          
         MVC   CONHEAD+36(1),ERRFLD                                             
MYERR    LA    R2,DADOPTH                                                       
MYERR2   MVI   GENSTAT2,USMYOK                                                  
         GOTO1 ERREX2                                                           
         EJECT                                                                  
*              VALIDATE AND DISPLAY RECORD                                      
VREC     DS    0H                                                               
         MVC   DATADISP,=H'28'                                                  
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING CTIKEY,R4                                                        
         MVI   CTIKTYP,C'I'                                                     
         MVC   CTIKID+8(2),TWAORIG                                              
         DROP  R4                                                               
         L     R6,AIO2                                                          
         ST    R6,AIO                                                           
         SPACE                                                                  
         GOTO1 DATAMGR,DMCB,=C'DMREAD ',=C'CTFILE ',KEY,AIO                     
         MVC   AIO,AIO1                                                         
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         SPACE                                                                  
         MVI   ELCODE,X'02'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING CTDSCD,R6                                                        
         MVC   SVSIGN,CTDSC                                                     
         MVC   DATADISP,=H'42'                                                  
         SPACE                                                                  
         LA    R5,32               MAX SCREEN ENTRIES                           
         L     R6,AIO1                                                          
         MVI   ELCODE,X'02'                                                     
         GOTO1 REMELEM                                                          
         SPACE                                                                  
         USING EZDNMD,R6                                                        
         MVC   EZDKTYP(32),SVKEY   KEY                                          
         MVC   EZDLEN,=H'43'       LENGTH = 42(KEY) +1(END 0)                   
         LA    R4,EZDELS                                                        
         LA    R2,DADIDH          VALIDATE AGENCY ID CODE                       
         CLI   5(R2),0             1ST REQUIRED                                 
         BNE   VR100                                                            
         XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(32),=C'** ERROR ** AGENCY ID REQUIRED *'                 
         B     MYERR2                                                           
         SPACE                                                                  
VR100    DS   0H                                                                
*R100    GOTO1 VALIMED                                                          
         SPACE                                                                  
VR120    CLI   5(R2),0                                                          
         BNE   VR130                                                            
         SPACE                                                                  
         IC    RE,0(R2)                                                         
         AR    R2,RE                                                            
         CLI   5(R2),0                                                          
         BE    VR220                                                            
         XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(38),=C'** ERROR ** STATION REQUIRES ID CODE *'           
         B     MYERR2                                                           
*                                                                               
VR130    DS   0H                   GET AGENCY ID HERE                           
         MVC   DUB,8(R2)                                                        
         OC    DUB,SPACES                                                       
         CLC   SVSIGN,DUB          MUST NOT BE SAME AS THIS SIGNON              
         BE    SAMEIDER                                                         
         SPACE                                                                  
         L     R6,AIO2                                                          
         ST    R6,AIO                                                           
         MVC   DATADISP,=H'28'                                                  
         SPACE                                                                  
         XC    KEY,KEY                                                          
         MVI   KEY,C'I'                                                         
         MVC   KEY+15(10),SPACES                                                
         ZIC   RE,5(R2)                                                         
         BCTR  RE,0                                                             
         EX    RE,MVCUID                                                        
         GOTO1 DATAMGR,DMCB,=C'DMREAD ',=C'CTFILE ',KEY,AIO                     
         MVC   AIO,AIO1                                                         
         CLI   8(R1),0                                                          
         BE    VR134                                                            
         XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(29),=C'** ERROR ** INVALID USER ID *'                    
         B     MYERR2                                                           
MVCUID   MVC   KEY+15(0),8(R2)                                                  
         SPACE                                                                  
VR134    MVI   ELCODE,X'02'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         SPACE                                                                  
         MVC   SVORIG,2(R6)                                                     
         SPACE                                                                  
         L     R6,AIO2                                                          
         MVI   ELCODE,X'06'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   SVAGY,2(R6)                                                      
         CLC   AGENCY,SVAGY                                                     
         BE    VR140                                                            
         SPACE                                                                  
         BAS   RE,VAGY                                                          
         SPACE                                                                  
         SPACE                                                                  
VR140    LA    R6,ELEM                                                          
         SPACE                                                                  
         USING EZDIDEL,R6                                                       
         XC    ELEM,ELEM                                                        
         MVI   EZDIDTY,EZDIDTYQ                                                 
         MVI   EZDIDLN,EZDIDLNQ                                                 
         MVC   EZDUID,KEY+15      SAVE USER ID                                  
         MVC   EZDORIG,SVORIG      BINARY USER ID (ORIGIN)                      
         SPACE                                                                  
         MVC   DATADISP,=H'42'                                                  
         SPACE                                                                  
         ZIC   RE,0(R2)                                                         
         AR    R2,RE                                                            
         SPACE                                                                  
* VALIDATE STATION (DOES NOT READ FROM STATION FILE)                            
         SPACE                                                                  
         CLI   5(R2),0                                                          
         BNE   VR150                                                            
         SPACE                                                                  
         XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(38),=C'** ERROR ** ID REQUIRES STATION CODE *'           
         B     MYERR2                                                           
         SPACE                                                                  
VR150    MVI   QMED,C'A'           ALLOW ALL MEDIAS                             
*                                                                               
         CLC   =C'ALL-',8(R2)      THIS ALL STATIONS                            
         BNE   VR160                                                            
*                                                                               
         CLI   12(R2),C'A'         THIS ALL MEDIAS                              
         BE    VR155                                                            
*                                                                               
         LA    R1,12(R2)           MEDIA CHARACTER                              
         ICM   R1,8,=AL1(EZMTMEDQ) LOOK FOR MEDIA                               
         GOTO1 VGETMED                                                          
         BE    VR155                                                            
*                                                                               
         XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(35),=CL35'** ERROR ** INVALID MEDIA CODE *'              
         B     MYERR2                                                           
*                                                                               
VR155    MVC   QSTA(4),=C'ALL '                                                 
         MVC   QSTA+4(1),12(R2)                                                 
         CLI   QSTA+4,C'A'       THIS ALL MEDIA'S                               
         BNE   *+8                  NO                                          
         MVI   QSTA+4,C' '       FORCE TO BLANK                                 
         B     VR164                                                            
*                                                                               
* SPECIFIC STATION HERE                                                         
VR160    DS    0H                                                               
         GOTO1 VREADSTA                                                         
         BE    VR163                                                            
*                                                                               
         XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(35),=CL35'** ERROR ** INVALID STATION *'                 
         B     MYERR2                                                           
*                                                                               
VR163    MVC   QSTA,FLDRDSTA                                                    
*                                                                               
VR164    MVC   EZDSTA,QSTA                                                      
*                                                                               
VR200    L     R6,AIO                                                           
         MVI   ELCODE,EZDIDTYQ                                                  
         BAS   RE,GETEL                                                         
         B     *+8                                                              
VR210    BAS   RE,NEXTEL                                                        
         BNE   VR216                                                            
         CLC   EZDSTA,ELEM+EZDSTA-EZDIDEL                                       
         BNE   VR210                                                            
         B     EQSTAERR                                                         
         SPACE                                                                  
VR216    GOTO1 ADDELEM                                                          
         SPACE                                                                  
VR220    ZIC   RE,0(R2)                                                         
         AR    R2,RE                                                            
         BCT   R5,VR120                                                         
*                                                                               
VR500    MVI   CURSYST,C'C'        SWITCH TO CONTROL FILE (GENDIR/FIL)          
         GOTO1 VALIFAS             SWITCH                                       
         MVC   SYSDIR,=C'GENDIR  '                                              
         MVC   SYSFIL,=C'GENFIL  '                                              
         MVC   KEY,SVKEY           RESET KEY                                    
         CLI   ACTNUM,ACTADD                                                    
         BE    VRXIT                                                            
         GOTO1 HIGH                CHANGE RE-READ KEY                           
         CLC   KEY(32),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   AIO,AIO2                                                         
         GOTO1 GETREC                                                           
         MVC   AIO,AIO1                                                         
VRXIT    B     EXIT                                                             
         DROP  R6                                                               
         EJECT                                                                  
*                                  DISPLAY KEY                                  
DKEY     DS    0H                                                               
         LA    R2,DADNAMH                                                       
         MVC   8(25,R2),KEY+7                                                   
         OI    6(R2),X'80'                                                      
         MVC   SVKEY,KEY                                                        
DKXIT    B     EXIT                                                             
         EJECT                                                                  
*                                  DISPLAY RECORD                               
DREC     DS    0H                                                               
         MVC   SVKEY,KEY                                                        
         SPACE                                                                  
         LA    R2,DADIDH                                                        
         BAS   RE,CLRSCRN                                                       
         L     R6,AIO                                                           
         MVC   DATADISP,=H'42'                                                  
         MVI   ELCODE,X'02'                                                     
         BAS   RE,GETEL                                                         
         BE    DR110                                                            
         DC    H'0'                                                             
*                                                                               
DR100    BAS   RE,NEXTEL                                                        
         BNE   DR300                                                            
         USING EZDIDEL,R6                                                       
*                                                                               
DR110    MVC   8(8,R2),EZDUID      AGENCY ID CODE                               
         OI    6(R2),X'80'                                                      
         SPACE                                                                  
         ZIC   RE,0(R2)                                                         
         AR    R2,RE                                                            
         OI    6(R2),X'80'                                                      
*                                                                               
         CLC   =C'ALL ',EZDSTA                                                  
         BNE   DR130                                                            
*                                                                               
         MVC   8(5,R2),EZDSTA      STATION                                      
         MVI   11(R2),C'-'                                                      
         CLI   12(R2),C' '                                                      
         BH    *+8                                                              
         MVI   12(R2),C'A'                                                      
         B     DR200                                                            
*                                                                               
* SPECIFIC STATION HERE                                                         
DR130    DS    0H                                                               
         LA    R1,EZDSTA                                                        
         GOTO1 VPRNTSTA                                                         
         MVC   8(L'PRTSTA7C,R2),PRTSTA7C                                        
*                                                                               
DR200    ZIC   RE,0(R2)                                                         
         AR    R2,RE                                                            
         B     DR100                                                            
*                                                                               
DR300    MVI   CURSYST,C'C'        SWITCH TO CONTROL FILE (GENDIR/FIL)          
         GOTO1 VALIFAS                                                          
         MVC   KEY,SVKEY           RESET & READ KEY                             
         GOTO1 HIGH                                                             
         CLC   KEY(32),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
DRXIT    B     EXIT                                                             
         EJECT                                                                  
*              LIST RECORDS                                                     
LIST     DS    0H                                                               
         LA    R2,LADSELH                                                       
         BAS   RE,CLRSCRN                                                       
*                                                                               
         CLI   MODE,PRINTREP                                                    
         BNE   LS100                                                            
*                                                                               
         LA    R1,HEADING                                                       
         ST    R1,SPECS                                                         
         LA    R1,HDRTN                                                         
         ST    R1,HEADHOOK                                                      
*                                                                               
LS100    LA    R4,KEY                                                           
         OC    KEY,KEY             WAS A KEY PROVIDED? (CONTINUATION)           
         BNZ   LS120                                                            
         MVC   KEY(2),=C'ZD'                                                    
         MVC   KEY+2(2),AGENCY                                                  
         MVC   KEY+7(8),OPADV      ADV NAME                                     
*                                                                               
LS120    GOTO1 HIGH                                                             
         B     LS220                                                            
*                                                                               
LS200    GOTO1 SEQ                                                              
*                                                                               
LS220    CLC   KEY(2),=C'ZD'                                                    
         BNE   EXIT                                                             
         CLC   KEY+2(2),AGENCY                                                  
         BNE   EXIT                                                             
*                                                                               
         GOTO1 GETREC                                                           
*                                                                               
         MVC   LISTAR,SPACES                                                    
         LA    R3,LISTAR                                                        
         USING LISTD,R3                                                         
*                                                                               
         MVC   LNAME,KEY+7         ADV NAME                                     
*                                                                               
         LA    R4,LID                                                           
         L     R6,AIO                                                           
         MVI   ELCODE,X'02'                                                     
         BAS   RE,GETEL                                                         
         B     *+8                                                              
LS300    BAS   RE,NEXTEL                                                        
         BNE   LS500                                                            
         USING EZDIDEL,R6                                                       
*                                                                               
* IF FILTERS MATCH ON FILTER                                                    
*                                                                               
         OC    OPSTA,OPSTA         STATION                                      
         BZ    *+14                                                             
         CLC   EZDSTA,OPSTA                                                     
         BNE   LS300                                                            
*                                                                               
         OC    OPID,OPID          AGENCY ID                                     
         BZ    *+14                                                             
         CLC   EZDUID,OPID                                                      
         BNE   LS300                                                            
*                                                                               
         LA    R0,LID                                                           
         CR    R4,R0               END OF LINE                                  
         BE    *+12                                                             
         MVI   0(R4),C','                                                       
         LA    R4,1(R4)                                                         
         MVC   0(8,R4),EZDUID                                                   
         LA    R4,8(R4)                                                         
*                                                                               
         CLI   0(R4),C' '                                                       
         BH    *+8                                                              
         BCT   R4,*-8                                                           
         LA    R4,1(,R4)                                                        
*                                                                               
         OC    EZDSTA,EZDSTA                                                    
         BNZ   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVI   0(R4),C'/'                                                       
         LA    R4,1(R4)                                                         
*                                                                               
         CLC   =C'ALL ',EZDSTA     IF ALL, JUST 1 LETTER                        
         BNE   LS320                                                            
*                                                                               
         MVC   0(L'EZDSTA,R4),EZDSTA                                            
         MVI   3(R4),C'-'                                                       
         CLI   EZDSTA+4,C' '                                                    
         BNE   *+8                                                              
         MVI   4(R4),C'A'                                                       
         LA    R4,5(,R4)                                                        
         B     LS380                                                            
*                                                                               
LS320    DS    0H                                                               
         LA    R1,EZDSTA                                                        
         GOTO1 VPRNTSTA                                                         
         MVC   0(L'PRTSTA7C,R4),PRTSTA7C    STATION                             
         LA    R4,L'PRTSTA7C-1(R4)                                              
         CLI   0(R4),C' '                                                       
         BH    *+8                                                              
         BCT   R4,*-8                                                           
         LA    R4,1(R4)                                                         
*                                                                               
LS380    LA    R0,LID+67                                                        
         CR    R4,R0               END OF LINE                                  
         BL    LS300                                                            
*                                                                               
LS500    CLI   MODE,PRINTREP                                                    
         BE    LS600                                                            
         MVC   DMDSKADD,KEY+36                                                  
         GOTO1 LISTMON                                                          
         B     LS200                                                            
         SPACE                                                                  
LS600    MVC   P+2(46),LISTAR                                                   
         GOTO1 SPOOL,DMCB,(R8)                                                  
         XC    LISTAR,LISTAR                                                    
         B     LS200                                                            
         DROP  R6                                                               
         EJECT                                                                  
VAGY     NTR1                                                                   
         SPACE                                                                  
         CLI   TWAOFFC,C'*'        THIS A DDS TERMINAL?                         
         BE    VA200                YES ALLOW IT ALL                            
*                                                                               
         BRAS  RE,CKTAB            GO SEE IF THIS UID IS LEGAL                  
         BNE   BADAGYER                                                         
*                                                                               
VA200    B     EXIT                                                             
         EJECT                                                                  
HDRTN    NTR1                                                                   
         B     EXIT                                                             
         SPACE 3                                                                
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
         B     EXIT                                                             
*                                                                               
CSCLC    CLC   8(0,R2),SPACES                                                   
CSOC     OC    8(0,R2),8(R2)                                                    
CSXC     XC    8(0,R2),8(R2)                                                    
         EJECT                                                                  
         GETEL R6,DATADISP,ELCODE                                               
         SPACE                                                                  
BDMEDERR XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'BDMEDMS),BDMEDMS                                       
         GOTO1 ERREX2                                                           
         SPACE                                                                  
BADAGYER XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'BADAGYMS),BADAGYMS                                     
         GOTO1 ERREX2                                                           
         SPACE                                                                  
SAMEIDER XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'SAMEIDMS),SAMEIDMS                                     
         GOTO1 ERREX2                                                           
         SPACE                                                                  
EQSTAERR XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'EQSTAMS),EQSTAMS                                       
         GOTO1 ERREX2                                                           
         SPACE                                                                  
TRAPERR  GOTO1 ERREX                                                            
         DC    H'0'                                                             
*                                                                               
         LTORG                                                                  
*                                                                               
*                                                                               
*                                                                               
BDMEDMS  DC    C'* ERROR * MEDIA MUST BE T, N, R, X, OR A *'                    
BADAGYMS DC    C'* ERROR * CAN''T MOVE TO DIFFERENT AGENCY *'                   
SAMEIDMS DC    C'* ERROR * SAME ID AS SIGNED ON *'                              
EQSTAMS  DC    C'* ERROR * SAME STATION AS PREVIOUS ENTRY *'                    
         SPACE                                                                  
HEADING  SSPEC H1,3,REQUESTOR                                                   
         SSPEC H1,49,C'ADVNAM ID RECORDS'                                       
         SSPEC H2,46,C'-----------------------'                                 
         SSPEC H1,93,AGYNAME                                                    
         SSPEC H2,93,AGYADD                                                     
         SSPEC H3,93,REPORT                                                     
         SSPEC H4,93,RUN                                                        
         SSPEC H5,103,PAGE                                                      
         SSPEC H8,3,C'NAME'                                                     
         SSPEC H9,3,C'----'                                                     
         SSPEC H8,30,C'ADVNAM STATION LIST'                                     
         SSPEC H9,30,C'-------------------'                                     
         DC    X'00'                                                            
*                                                                               
*                                                                               
*                                                                               
CKTAB    NTR1  BASE=*,LABEL=*                                                   
         LA    R1,PCTABLE                                                       
*                                                                               
CKTAB10  LA    R0,AGENCY                                                        
         BRAS  RE,CKINTAB                                                       
         BNE   CKTAB20                                                          
*                                                                               
         LA    R0,SVAGY                                                         
         BRAS  RE,CKINTAB                                                       
         JE    EQXIT                                                            
*                                                                               
* ADVANCE TO NEXT LINE (PAST X'FF')                                             
CKTAB20  LA    R1,2(R1)            SKIP AGENCY POWER CODE                       
         CLI   0(R1),X'FF'         END OF TABLE?                                
         BNE   CKTAB20             NO - SKIP NEXT POWER CODE                    
*                                                                               
         LA    R1,1(R1)            ADVANCE PAST X'FF' - END OF TABLE            
*                                                                               
         CLI   0(R1),X'FF'         END OF ALL TABLES?                           
         JE    NEQXIT                                                           
         B     CKTAB10                                                          
*                                                                               
         LTORG                                                                  
*                                                                               
*                                                                               
*                                                                               
* AGENCY CODE IN TABLE?                                                         
* R0 EXPECTED TO ADDRESS AGENCY POWER CODE                                      
* R1 EXPECTED TO ADDRESS PCTABLE                                                
*                                                                               
CKINTAB  NTR1  BASE=*,LABEL=*                                                   
         LR    R2,R0               AGY POWER CODE                               
*                                                                               
CKINT10  CLC   0(2,R2),0(R1)       AGENCY CODE MATCHES?                         
         JE    EQXIT               YES                                          
         LA    R1,2(R1)            ADVANCE TO NEXT CODE                         
         CLI   0(R1),X'FF'         EOT?                                         
         JE    NEQXIT              YES - AGY NOT IN THIS TABLE                  
         B     CKINT10                                                          
         LTORG                                                                  
*                                                                               
*                                                                               
*                                                                               
PCTABLE  DC    C'DFDTLFTH'         OK TO MOVE BETWEEN THESE POWER CODES         
         DC    X'FF'                                                            
*                                                                               
         DC    C'BSTH'             OK TO MOVE BETWEEN THESE POWER CODES         
         DC    X'FF'                                                            
*                                                                               
         DC    C'BSDF'             OK TO MOVE BETWEEN THESE POWER CODES         
         DC    X'FF'                                                            
*                                                                               
         DC    C'JWFRH7'           OK TO MOVE BETWEEN THESE POWER CODES         
         DC    X'FF'                                                            
*                                                                               
         DC    C'SJMC'             OK TO MOVE BETWEEN THESE POWER CODES         
         DC    X'FF'                                                            
*                                                                               
         DC    C'LMGZ'             OK TO MOVE BETWEEN THESE POWER CODES         
         DC    X'FF'                                                            
*                                                                               
         DC    C'WWYN'             OK TO MOVE BETWEEN THESE POWER CODES         
         DC    X'FF'                                                            
*                                                                               
         DC    C'THTB'             OK TO MOVE BETWEEN THESE POWER CODES         
         DC    X'FF'                                                            
*                                                                               
         DC    C'THDFPC'           OK TO MOVE BETWEEN THESE POWER CODES         
         DC    X'FF'                                                            
*                                                                               
         DC    C'BNOO'             OK TO MOVE BETWEEN THESE POWER CODES         
         DC    X'FF'                                                            
*                                                                               
         DC    C'YRHY'             OK TO MOVE BETWEEN THESE POWER CODES         
         DC    X'FF'                                                            
*                                                                               
         DC    C'O0TB'             OK TO MOVE BETWEEN THESE POWER CODES         
         DC    X'FF'                                                            
*                                                                               
         DC    C'OOOU'             OMDTOA/OMDCAN SPEC-39341                     
         DC    X'FF'                                                            
*                                                                               
         DC    X'FF'               END OF TABLES                                
*                                                                               
*                                                                               
*                                                                               
*                                                                               
*                                                                               
*                                                                               
*                                                                               
LISTD    DSECT                                                                  
LNAME    DS    CL25                                                             
         DS    CL2                                                              
LID      DS    CL8                                                              
         DS    CL1                                                              
LSTA     DS    CL7                                                              
*                                                                               
*                                                                               
*                                                                               
* DDCOMFACS                                                                     
* DDSPOOLD                                                                      
* DDSPLWORKD                                                                    
* SPEZFFFD                                                                      
         PRINT OFF                                                              
       ++INCLUDE DDCOMFACS                                                      
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE SPEZFFFD                                                       
         PRINT ON                                                               
         ORG   CONTAGH                                                          
* SPEZFFDD                                                                      
       ++INCLUDE SPEZFF7D                                                       
         ORG   CONTAGH                                                          
* SPEZFFDD                                                                      
       ++INCLUDE SPEZFEFD                                                       
         PRINT OFF                                                              
       ++INCLUDE DDGENTWA                                                       
         EJECT                                                                  
         PRINT ON                                                               
* SPGENSTA                                                                      
         PRINT OFF                                                              
       ++INCLUDE SPGENSTA                                                       
         EJECT                                                                  
         PRINT ON                                                               
* SPGENEZ                                                                       
         PRINT OFF                                                              
       ++INCLUDE SPGENEZ                                                        
         EJECT                                                                  
         PRINT ON                                                               
*                                                                               
       ++INCLUDE SPEZDSCTS                                                      
*                                                                               
* CTGENFILE                                                                     
         PRINT OFF                                                              
       ++INCLUDE CTGENFILE                                                      
         EJECT                                                                  
         PRINT ON                                                               
*SPEZFWORKD                                                                     
         PRINT OFF                                                              
       ++INCLUDE SPEZFWORKD                                                     
         PRINT ON                                                               
SYSD     DSECT                                                                  
         ORG   SYSSPARE                                                         
* WORK AREAS FOR THIAS PROG                                                     
         SPACE                                                                  
ERRFLD   DS    CL1                 ERROR FIELD                                  
OPADV    DS    CL25                                                             
OPID     DS    CL8                                                              
OPSTA    DS    CL5                                                              
SVORIG   DS    XL2                                                              
SVAGY    DS    XL2                                                              
SVSIGN   DS    CL8                                                              
WRKFEND  EQU   *                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'030SPEZF0A   09/26/19'                                      
         END                                                                    
