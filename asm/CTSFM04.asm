*          DATA SET CTSFM04    AT LEVEL 003 AS OF 05/01/02                      
*PHASE TA0A04A                                                                  
*                                                                               
***********************************************************************         
*                                                                     *         
*  TITLE:        CTSFM04 -- DRIVER USER RECORD MAINTENANCE/LIST       *         
*                                                                     *         
*  COMMENTS:     MAINTAINS DRIVER USER RECORDS ON CTFILE.             *         
*                                                                     *         
*  CALLED FROM:  SFM CONTROLLER (TA0A00), WHICH CALLS                 *         
*                DDGENCON (T00A30), WHICH CALLS THIS.                 *         
*                                                                     *         
*  CALLS TO:     DRIVAL (DRIVER VALIDATION ROUTINES)                  *         
*                DATAMGR                                              *         
*                                                                     *         
*  INPUTS:       SCREENS CTSFMF4 (MAINTENANCE)                        *         
*                        CTSFME4 (LIST)                               *         
*                                                                     *         
*  REGISTERS:    R0 -- WORK                                           *         
*                R1 -- WORK                                           *         
*                R2 -- SCREEN FIELD HEADER POINTER                    *         
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
         TITLE 'TA0A04 - DRIVER USER RECORD MAINTENANCE/LIST'                   
TA0A04   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**0A04**,R7,RR=R3                                              
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     R8,ASPOOLD          GENERAL PRINT AREAS                          
         USING SPOOLD,R8                                                        
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA      BASE SCREEN FOR SYSTEM + THIS PROG           
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         ST    R3,RELO             RELOCATION FACTOR                            
*                                                                               
         XC    DMCB,DMCB                                                        
         MVC   DMCB+4(4),=X'D9000A36'                                           
         GOTO1 CALLOV,DMCB         GET A(DRIVAL)                                
         CLI   DMCB+4,X'FF'                                                     
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVC   ADRIVAL,DMCB                                                     
*                                                                               
         CLI   MODE,VALKEY         VALIDATE RECORD KEY                          
         BE    VK                                                               
         CLI   MODE,VALREC         VALIDATE RECORD                              
         BE    VR                                                               
         CLI   MODE,DISPKEY        DISPLAY KEY                                  
         BE    DK                                                               
         CLI   MODE,DISPREC        DISPLAY RECORD                               
         BE    DR                                                               
         CLI   MODE,LISTRECS       LIST RECORDS                                 
         BE    LR                                                               
*                                                                               
         B     EXIT                                                             
         EJECT                                                                  
* VALIDATE KEY                                                                  
*                                                                               
VK       XC    CODE,CODE           CODE                                         
         LA    R2,SFMCODEH                                                      
         CLI   5(R2),0             TEST ANY DATA                                
         BNE   *+16                YES                                          
         CLI   ACTNUM,ACTLIST      TEST ACTION LIST                             
         BE    VK10                YES                                          
         B     MISSERR             NO - SO FIELD IS REQUIRED                    
*                                                                               
         GOTO1 ANY                                                              
         MVC   CODE,WORK           SAVE CODE                                    
*                                                                               
         LA    R4,KEY                                                           
         USING CT02KEY,R4                                                       
         XC    KEY,KEY                                                          
         MVI   CT02KTYP,X'02'      USER RECORD TYPE                             
         MVC   CT02KAGY,AGENCY     ALPHA AGENCY CODE                            
         MVC   CT02KCOD,CODE       CODE                                         
         DROP  R4                                                               
*                                                                               
VK10     XC    FILTER,FILTER       FILTER                                       
         CLI   ACTNUM,ACTLIST                                                   
         BNE   VKX                 ONLY MATTERS IN LIST MODE                    
*                                                                               
         LA    R2,SFLFILTH                                                      
         CLI   5(R2),0             TEST ANY DATA                                
         BE    VKX                                                              
         GOTO1 ANY                                                              
         MVC   FILTER,WORK         SAVE FILTER                                  
*                                                                               
VKX      B     EXIT                                                             
         EJECT                                                                  
* VALIDATE RECORD                                                               
*                                                                               
VR       XC    DRVLBLK,DRVLBLK     CLEAR THE DRIVAL PARAMETER BLOCK             
         MVC   DBCOMADR,ACOMFACS   A(COMFACS)                                   
         LA    R6,ELEM                                                          
         ST    R6,DBELOADR         ALWAYS PUT ELEMENT INTO ELEM                 
         LA    R1,L'ELEM(R6)       A(END OF ELEMENT BUFFER)                     
         ST    R1,DBENDADR                                                      
*                                                                               
         MVI   ELCODE,X'02'        DESCRIPTION ELEMENT                          
         GOTO1 REMELEM                                                          
         USING USNAMD,R6                                                        
         XC    ELEM,ELEM                                                        
         MVI   USNAMEL,X'02'                                                    
         MVI   USNAMLEN,22                                                      
         LA    R2,SFMNAMEH                                                      
         CLI   5(R2),0                                                          
         BE    MISSERR             THIS ELEMENT IS REQUIRED                     
         GOTO1 ANY                                                              
         MVC   USNAME,WORK                                                      
         GOTO1 ADDELEM                                                          
         DROP  R6                                                               
*                                                                               
         MVI   ELCODE,X'07'        FILTER ELEMENT                               
         GOTO1 REMELEM                                                          
         LA    R2,SFMFILTH                                                      
         CLI   5(R2),0                                                          
         BE    VR10                OPTIONAL                                     
         USING USFLTD,R6                                                        
         XC    ELEM,ELEM                                                        
         MVI   USFLTEL,X'07'                                                    
         MVI   USFLTLEN,10                                                      
         GOTO1 ANY                                                              
         MVC   USFLTFLT,WORK                                                    
         GOTO1 ADDELEM                                                          
         DROP  R6                                                               
*                                                                               
VR10     MVI   ELCODE,X'33'        OUTPUT LENGTH ELEMENT                        
         GOTO1 REMELEM                                                          
         LA    R2,SFMWIDH                                                       
         CLI   5(R2),0                                                          
         BE    VR20                                                             
         XC    ELEM,ELEM                                                        
         MVI   DBOPCODE,X'33'                                                   
         ST    R2,DBSRCADR                                                      
         GOTO1 ADRIVAL,DMCB,DRVLBLK                                             
         CLI   DBERRNUM,0                                                       
         BNE   ERR                                                              
         GOTO1 ADDELEM                                                          
*                                                                               
VR20     MVI   ELCODE,X'36'        OUTPUT OPTIONS ELEMENTS                      
         GOTO1 REMELEM                                                          
         XC    ELEM,ELEM                                                        
         LA    R2,SFMOPTSH                                                      
         CLI   5(R2),0                                                          
         BNE   VR30                                                             
         GOTO1 HELLO,DMCB,(C'D',=C'CTFILE'),(X'04',AIO),(1,=X'36')              
         B     VR50                                                             
VR30     MVI   DBOPCODE,X'36'                                                   
         LA    R1,SCANBLK                                                       
         LA    R0,8                                                             
         XC    0(74,R1),0(R1)                                                   
         LA    R1,74(R1)                                                        
         BCT   R0,*-10                                                          
         XC    SCANOFLO,SCANOFLO                                                
         GOTO1 SCANNER,DMCB,(52,(R2)),(9,SCANBLK),C',=,='                       
         CLI   4(R1),0                                                          
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLI   4(R1),8                                                          
         BH    MAX8OPT                                                          
         XC    SCANOFLO,SCANOFLO                                                
         MVC   DBSCANLN,4(R1)                                                   
         LA    R3,SCANBLK                                                       
VR40     ST    R3,DBSRCADR                                                      
         XC    ELEM,ELEM                                                        
         GOTO1 ADRIVAL,DMCB,DRVLBLK                                             
         CLI   DBERRNUM,0                                                       
         BNE   ERR                                                              
         GOTO1 ADDELEM                                                          
         LA    R3,74(R3)                                                        
         CLI   0(R3),0                                                          
         BNE   VR40                                                             
         MVI   ELTYPE,X'36'                                                     
         BAS   RE,INPUTEL                                                       
*                                                                               
VR50     MVI   ELCODE,X'87'        HEAD1 LITERAL                                
         GOTO1 REMELEM                                                          
         MVI   DBOPCODE,X'87'                                                   
         LA    R2,SFMHD1H                                                       
         CLI   5(R2),0                                                          
         BE    VR60                                                             
         XC    ELEM,ELEM                                                        
         ST    R2,DBSRCADR                                                      
         GOTO1 ADRIVAL,DMCB,DRVLBLK                                             
         CLI   DBERRNUM,0                                                       
         BNE   ERR                                                              
         MVI   ELEM+2,1                                                         
         GOTO1 ADDELEM                                                          
*                                                                               
VR60     LA    R2,SFMHD2H          HEAD2 LITERAL                                
         CLI   5(R2),0                                                          
         BE    VR70                                                             
         XC    ELEM,ELEM                                                        
         ST    R2,DBSRCADR                                                      
         GOTO1 ADRIVAL,DMCB,DRVLBLK                                             
         CLI   DBERRNUM,0                                                       
         BNE   ERR                                                              
         MVI   ELEM+2,2                                                         
         GOTO1 ADDELEM                                                          
*                                                                               
VR70     LA    R2,SFMHD3H          HEAD3 LITERAL                                
         CLI   5(R2),0                                                          
         BE    VR80                                                             
         XC    ELEM,ELEM                                                        
         ST    R2,DBSRCADR                                                      
         GOTO1 ADRIVAL,DMCB,DRVLBLK                                             
         CLI   DBERRNUM,0                                                       
         BNE   ERR                                                              
         MVI   ELEM+2,3                                                         
         GOTO1 ADDELEM                                                          
*                                                                               
VR80     LA    R2,SFMHD4H          HEAD4 LITERAL                                
         CLI   5(R2),0                                                          
         BE    DR                                                               
         XC    ELEM,ELEM                                                        
         ST    R2,DBSRCADR                                                      
         GOTO1 ADRIVAL,DMCB,DRVLBLK                                             
         CLI   DBERRNUM,0                                                       
         BNE   ERR                                                              
         MVI   ELEM+2,4                                                         
         GOTO1 ADDELEM                                                          
         EJECT                                                                  
* DISPLAY RECORD                                                                
*                                                                               
DR       BAS   RE,CLEAR            CLEAR ALL INPUT FIELDS                       
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,X'02'        DESCRIPTION                                  
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                THIS ELEMENT IS REQUIRED                     
         USING USNAMD,R6                                                        
         MVC   SFMNAME,USNAME                                                   
         OI    SFMNAMEH+6,X'80'                                                 
         DROP  R6                                                               
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,X'07'        FILTER                                       
         BAS   RE,GETEL                                                         
         BNE   DR10                                                             
         USING USFLTD,R6                                                        
         MVC   SFMFILT,USFLTFLT                                                 
         OI    SFMFILTH+6,X'80'                                                 
         DROP  R6                                                               
*                                                                               
DR10     L     R6,AIO                                                           
         MVI   ELCODE,X'33'        OUTPUT LENGTH                                
         BAS   RE,GETEL                                                         
         BNE   DR20                                                             
         USING USOLD,R6                                                         
         EDIT  (1,USOLEN),(2,SFMWID),ALIGN=LEFT                                 
         OI    SFMWIDH+6,X'80'                                                  
         DROP  R6                                                               
*                                                                               
DR20     L     R6,AIO                                                           
         MVI   ELCODE,X'04'        INPUT FIELD ELEMENT                          
         BAS   RE,GETEL                                                         
         BNE   DR30                                                             
         USING USINPD,R6                                                        
         CLI   USINPOP,X'36'       OUTPUT OPTIONS                               
         BE    *+16                                                             
         BAS   RE,NEXTEL                                                        
         BE    *-12                                                             
         B     DR30                                                             
         ZIC   R1,USINPLEN                                                      
         SH    R1,=H'4'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   SFMOPTS(0),USINPTXT                                              
         OI    SFMOPTSH+6,X'80'                                                 
         DROP  R6                                                               
*                                                                               
DR30     L     R6,AIO                                                           
         MVI   ELCODE,X'87'        HEAD LINES 1,2,3,4                           
         BAS   RE,GETEL                                                         
         BNE   DRX                                                              
         USING USLITD,R6                                                        
DR40     LA    R2,SFMHD1H                                                       
         CLI   USLITLIN,1                                                       
         BE    DR50                                                             
         LA    R2,SFMHD2H                                                       
         CLI   USLITLIN,2                                                       
         BE    DR50                                                             
         LA    R2,SFMHD3H                                                       
         CLI   USLITLIN,3                                                       
         BE    DR50                                                             
         LA    R2,SFMHD4H                                                       
DR50     ZIC   R1,USLITLEN                                                      
         SH    R1,=H'4'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   8(0,R2),USLITRAL                                                 
         OI    6(R2),X'80'                                                      
         BAS   RE,NEXTEL                                                        
         BE    DR40                                                             
         DROP  R6                                                               
*                                                                               
DRX      B     EXIT                                                             
         EJECT                                                                  
* ONLINE LIST                                                                   
*                                                                               
LR       OC    KEY,KEY             TEST FIRST TIME THROUGH                      
         BNZ   LR10                                                             
*                                                                               
         USING CT02KEY,R4                                                       
         LA    R4,KEY                                                           
         MVI   CT02KTYP,X'02'      USER RECORD TYPE                             
         MVC   CT02KAGY,AGENCY     ALPHA AGENCY CODE                            
         MVC   CT02KCOD,CODE       CODE                                         
*                                                                               
LR10     GOTO1 HIGH                FIRST RECORD                                 
         B     LR30                                                             
*                                                                               
LR20     GOTO1 SEQ                 NEXT RECORD                                  
*                                                                               
LR30     L     R4,AIO                                                           
         CLI   CT02KTYP,X'02'      TEST USER RECORD                             
         BNE   LRX                 NO - LEAVE                                   
         CLC   CT02KAGY,AGENCY     TEST SAME AGENCY                             
         BNE   LRX                 NO - LEAVE                                   
*                                                                               
         MVC   LISTAR,SPACES       CLEAR LIST LINE                              
         L     R6,AIO                                                           
         MVI   ELCODE,X'07'        FILTER ELEMENT                               
         BAS   RE,GETEL                                                         
         BE    *+18                FILTER IS THERE - CHECK IT OUT               
         OC    FILTER,FILTER       TEST FILTER GIVEN                            
         BZ    LR40                NO - LIST THE RECORD                         
         B     LR20                YES - NO MATCH ON FILTER                     
*                                                                               
         USING USFLTD,R6                                                        
         OC    FILTER,FILTER       TEST FILTER GIVEN                            
         BNZ   *+14                YES - TEST FOR MATCH                         
         MVC   LSTFILT,USFLTFLT    PUT FILTER IN LIST LINE                      
         B     LR40                                                             
*                                                                               
         CLC   FILTER,USFLTFLT     TEST MATCH ON FILTER                         
         BNE   LR20                NO                                           
         MVC   LSTFILT,USFLTFLT    PUT FILTER IN LIST LINE                      
         DROP  R6                                                               
*                                                                               
LR40     MVC   LSTCODE,CT02KCOD    PUT CODE IN LIST LINE                        
         DROP  R4                                                               
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,X'02'        DESCRIPTION ELEMENT                          
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                THIS ELEMENT IS REQUIRED                     
         USING USNAMD,R6                                                        
         MVC   LSTNAME,USNAME      PUT DESCRIPTION IN LIST LINE                 
         DROP  R6                                                               
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,X'33'        OUTPUT LENGTH                                
         BAS   RE,GETEL                                                         
         BNE   LR50                                                             
         USING USOLD,R6                                                         
         EDIT  (1,USOLEN),(2,LSTWIDTH)                                          
         DROP  R6                                                               
*                                                                               
LR50     GOTO1 LISTMON             SEND RECORD TO SCREEN                        
         B     LR20                NEXT RECORD                                  
*                                                                               
LRX      B     EXIT                                                             
         SPACE 5                                                                
* DISPLAY KEY                                                                   
*                                                                               
DK       L     R4,AIO              SELECTED RECORD                              
         USING CT02KEY,R4                                                       
*                                                                               
         MVC   SFMCODE,CT02KCOD    CODE                                         
         OI    SFMCODEH+6,X'80'    XMIT                                         
         DROP  R4                                                               
*                                                                               
         B     EXIT                                                             
         EJECT                                                                  
* HANDLE INPUT FIELD ELEMENTS                                                   
*                                                                               
INPUTEL  NTR1                                                                   
*                                                                               
         GOTO1 HELLO,DMCB,(C'D',=C'CTFILE'),(X'04',AIO),(1,ELTYPE)              
*                                                                               
         XC    ELEM,ELEM                                                        
         USING USINPD,R6                                                        
         MVI   USINPEL,X'04'                                                    
         ZIC   R1,5(R2)                                                         
         AH    R1,=H'3'                                                         
         STC   R1,USINPLEN                                                      
         MVC   USINPOP,ELTYPE                                                   
         SH    R1,=H'4'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   USINPTXT(0),8(R2)                                                
         GOTO1 ADDELEM                                                          
         DROP  R6                                                               
*                                                                               
         B     EXIT                                                             
         SPACE 5                                                                
* BLANK OUT ALL INPUT FIELDS                                                    
*                                                                               
CLEAR    NTR1                                                                   
*                                                                               
         LA    R5,SFMTAGH          LAST FIELD ON SCREEN                         
         LA    R2,SFMNAMEH         FIRST FIELD HEADER                           
*                                                                               
CLEAR10  ZIC   R3,0(R2)            LENGTH OF FIELD + HEADER                     
         SH    R3,=H'9'            MINUS HEADER LENGTH AND 1 FOR EX             
         TM    1(R2),X'02'         TEST EXTENDED HEADER                         
         BZ    *+8                                                              
         SH    R3,=H'8'            MINUS LENGTH OF EXTENSION                    
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   8(0,R2),SPACES      BLANK OUT FIELD                              
         OI    6(R2),X'80'         TRANSMIT                                     
*                                                                               
CLEAR20  ZIC   R3,0(R2)            RESTORE LENGTH                               
         LA    R2,0(R2,R3)         NEXT SCREEN FIELD                            
         CR    R2,R5               TEST END OF SCREEN                           
         BE    CLEARX                                                           
*                                                                               
         TM    1(R2),X'20'         TEST FIELD IS PROTECTED                      
         BZ    CLEAR10             IT IS NOT -- CLEAR IT                        
         B     CLEAR20             IT IS -- TRY NEXT FIELD                      
*                                                                               
CLEARX   B     EXIT                                                             
         EJECT                                                                  
ERR      XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(10),=C'* ERROR * '                                       
         MVC   CONHEAD+10(40),DBERRMSG                                          
         MVC   CONHEAD+55(2),=C' *'                                             
         GOTO1 SQUASHER,DMCB,CONHEAD,60                                         
         GOTO1 ERREX2                                                           
*                                                                               
MAX8OPT  XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'MAX8OPTM),MAX8OPTM                                     
         GOTO1 ERREX2                                                           
MAX8OPTM DC    C'* ERROR * MAXIMUM OF 8 OPTIONS *'                              
*                                                                               
MISSERR  MVI   ERROR,MISSING                                                    
         GOTO1 ERREX                                                            
         SPACE 3                                                                
         GETEL R6,DATADISP,ELCODE                                               
         SPACE 3                                                                
EXIT     XIT1                                                                   
         SPACE 3                                                                
         LTORG                                                                  
         EJECT                                                                  
       ++INCLUDE DRIVALBLKD                                                     
         EJECT                                                                  
       ++INCLUDE CTGENUSER                                                      
         PRINT OFF                                                              
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE CTSFMFFD                                                       
         PRINT ON                                                               
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE CTSFMF4D                                                       
         PRINT OFF                                                              
         ORG   CONTAGH                                                          
       ++INCLUDE CTSFME4D                                                       
       ++INCLUDE DDGENTWA                                                       
       ++INCLUDE CTSFMWORKD                                                     
         PRINT ON                                                               
         EJECT                                                                  
* START OF SAVED STORAGE (6144)                                                 
*                                                                               
         ORG   SYSSPARE                                                         
RELO     DS    F                   RELOCATION FACTOR                            
ADRIVAL  DS    A                   A(DRIVAL)                                    
CODE     DS    CL8                 CODE                                         
FILTER   DS    CL8                 FILTER                                       
ELTYPE   DS    XL1                                                              
SCANBLK  DS    8XL74               SCANNER BLOCK                                
SCANOFLO DS    XL74                SCANNER BLOCK OVERFLOW                       
         SPACE 5                                                                
* ONLINE LIST LINE                                                              
*                                                                               
GEND     DSECT                                                                  
         ORG   LISTAR                                                           
         DS    CL3                                                              
LSTCODE  DS    CL8                                                              
         DS    CL2                                                              
LSTNAME  DS    CL20                                                             
         DS    CL6                                                              
LSTFILT  DS    CL8                                                              
         DS    CL2                                                              
LSTWIDTH DS    CL2                                                              
         SPACE 5                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'003CTSFM04   05/01/02'                                      
         END                                                                    
