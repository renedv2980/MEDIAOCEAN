*          DATA SET NESFM29    AT LEVEL 114 AS OF 10/31/05                      
*PHASE T31C29A                                                                  
*                                                                               
***********************************************************************         
*                                                                     *         
*  TITLE        T31C29 - STATION/CLIENT GROUP ASSIGN MAINT            *         
*                                                                     *         
*  CALLED FROM  GENCON VIA T31C00 (NFM CONTROLLER)                    *         
*                                                                     *         
*  COMMENTS     SUPPORTS ADD, DISPLAY, DELETE, RESTORE, CHANGE, LIST  *         
*                                                                     *         
*  INPUTS       SCREEN T31CB5 (MAINTENANCE)                           *         
*               SCREEN T31CB6 (LIST)                                  *         
*                                                                     *         
*  OUTPUTS      UPDATED GROUP DEFINITION RECORDS                      *         
*                                                                     *         
*  REGISTERS    R0 -- WORK                                            *         
*               R1 -- WORK                                            *         
*               R2 -- WORK                                            *         
*               R3 -- WORK                                            *         
*               R4 -- WORK                                            *         
*               R5 -- WORK                                            *         
*               R6 -- WORK                                            *         
*               R7 -- WORK                                            *         
*               R8 -- FREE                                            *         
*               R9 -- SYSD                                            *         
*               RA -- TWA                                             *         
*               RB -- FIRST BASE                                      *         
*               RC -- GEND                                            *         
*               RD -- SYSTEM                                          *         
*               RE -- SYSTEM                                          *         
*               RF -- SYSTEM                                          *         
*                                                                     *         
*  I/O AREAS    IO1 - GROUP DEFINITION RECORD                         *         
*                                                                     *         
***********************************************************************         
         TITLE 'T31C29 - STATION/CLIENT GROUP DEFINITION RECORDS'               
T31C29   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T31C29                                                         
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA                                                   
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         LA    R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
*                                                                               
         BAS   RE,SETUP                                                         
         CLI   MODE,SETFILE        SET FILE NAME                                
         BE    SF                                                               
*                                                                               
         CLI   MODE,VALKEY         VALIDATE RECORD KEY                          
         BE    VK                                                               
         CLI   MODE,DISPREC        DISPLAY RECORD                               
         BE    DR                                                               
*        B     DL                                                               
         B     XIT                                                              
*                                                                               
DL       NTR1                                                                   
         MVI   ERROR,INVACT                                                     
         LA    R2,CONACTH                                                       
         B     TRAPERR                                                          
         EJECT                                                                  
*                                                                               
* SET FILE                                                                      
SF       DS    0H                                                               
         OI    GENSTAT4,NODELLST                                                
         BAS   RE,SAVEDEF                                                       
         B     XIT                                                              
         EJECT                                                                  
*                                                                               
XIT      XIT1                                                                   
         EJECT                                                                  
* VALIDATE KEY ROUTINE                                                          
*                                                                               
VK       BAS   RE,SAVEDEF                                                       
         MVC   QNET,SPACES                                                      
         MVC   QCLT,ZEROES                                                      
         LA    R3,SVKEY            CLEAR KEY                                    
         USING GRPPKEY,R3                                                       
         XC    SVKEY,SVKEY                                                      
*                                                                               
         MVI   GRPKTYP,GRPKTYPQ    RECORD TYPE                                  
         MVI   GRPKSTYP,GRPKSTYQ                                                
*                                                                               
VK10     LA    R2,SFSMEDH          MEDIA                                        
         GOTO1 VALIMED                                                          
         MVC   GRPPAGMD,BAGYMD                                                  
*                                                                               
         LA    R2,SFSSTNH                                                       
         CLI   5(R2),0                                                          
         BNE   *+12                                                             
         MVI   ERROR,MISSING                                                    
         B     TRAPERR                                                          
*                                                                               
         MVC   QNET,8(R2)                                                       
         OC    QNET,SPACES                                                      
*                                                                               
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING STAREC,R4                                                        
         MVC   STAKEY(STAKEYLN),ZEROES                                          
         MVI   STAKTYPE,C'S'       RECORD TYPE                                  
         MVC   STAKMED,QMED        MEDIA                                        
         OC    STAKCALL(4),=XL4'40404040'                                       
         MVC   STAKCALL+4(1),QMED                                               
         MVC   STAKCALL(4),QNET                                                 
         MVC   STAKAGY,AGENCY      AGENCY                                       
         MVC   STAKCLT,QCLT        CLIENT EXCEPTION                             
*                                                                               
         BAS   RE,SETDEF                                                        
         GOTO1 HIGH                                                             
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLC   KEY(L'STAKEY),KEYSAVE                                            
         BE    *+12                                                             
         MVI   ERROR,INVALID                                                    
         B     TRAPERR                                                          
         DROP  R4                                                               
*                                                                               
*                                                                               
         LA    R2,SFSSIDH          GROUP ID                                     
         CLI   5(R2),0                                                          
         BNE   VK20                                                             
         B     VKX                                                              
*                                                                               
VK20     TM    4(R2),X'04'         ALPHABETIC CHARACTER?                        
         BO    *+12                                                             
         MVI   ERROR,NOTALPHA                                                   
         B     TRAPERR                                                          
         MVC   GRPPID,8(R2)        PUT ID IN KEY                                
*                                                                               
VKX      XC    KEY,KEY                                                          
         MVC   KEY,SVKEY                                                        
         BAS   RE,RSTRDEF                                                       
*  JUST DO READ AND READ ANYTHING, I DONT CARE :)                               
         GOTO1 HIGH                                                             
         B     XIT                                                              
         DROP  R3                                                               
         EJECT                                                                  
***************************************************************                 
* DISPLAY RECORD                                                                
*                                                                               
DR       GOTO1  CLRS                                                            
         XC     KEY,KEY                                                         
         LA     R4,KEY                                                          
         USING  GRPPKEY,R4                                                      
*                                                                               
         MVI   GRPPTYP,GRPPTYPQ                                                 
         MVI   GRPPSTYP,GRPPSTYQ                                                
         MVC   GRPPAGMD,BAGYMD                                                  
         MVC   GRPPVAL,SFSSTN                                                   
         OC    GRPPVAL,SPACES                                                   
         MVC   GRPPID,SFSSID                                                    
         LA     R2,SFSLINH                                                      
*                                                                               
         GOTO1  HIGH                                                            
         CLI    DMCB+8,0                                                        
         BE     *+6                                                             
         DC     H'0'                                                            
         B      DR15                                                            
*                                                                               
DR10     GOTO1  SEQ                                                             
         CLI    DMCB+8,0                                                        
         BE     *+6                                                             
         DC     H'0'                                                            
*                                                                               
DR15     CLC    GRPPVAL,KEYSAVE+3                                               
         BNE    DRX                                                             
*                                                                               
         CLC    KEY(9),KEYSAVE                                                  
         BNE    DRX                                                             
         DROP   R4                                                              
         XC     SVKEY,SVKEY                                                     
         MVC    SVKEY,KEY                                                       
*                                                                               
         GOTO1  GETREC                                                          
         CLI    DMCB+8,0                                                        
         BE     *+6                                                             
         DC     H'0'                                                            
*                                                                               
         L      R6,AIO                                                          
         USING  GRPRECD,R6                                                      
*                                                                               
*        LA     R2,SFSLINH                                                      
         XC     P,P                                                             
         MVC    SVID,GRPKID                                                     
*                                                                               
         GOTO1  HEXOUT,DMCB,GRPKCODE,FULL,L'GRPKCODE,=C'TOG',0                  
*                                                                               
         MVI    ELCODE,GRPGRPCQ                                                 
         BAS    RE,GETEL                                                        
         BE     *+6                                                             
         DC     H'0'                                                            
*                                                                               
         USING  GRPGRPD,R6                                                      
         MVC    SVNM1,GRPGNAM1                                                  
         MVC    SVNM2,GRPGNAM2                                                  
         DROP   R6                                                              
*        B      DR30                                                            
*  LETS READ SGROUP DEF RECORDS TO GET OTHER INFO                               
*                                                                               
         XC     KEY,KEY                                                         
         LA     R4,KEY                                                          
         USING  GRPRECD,R4                                                      
         MVI    GRPKTYP,GRPKTYPQ                                                
         MVI    GRPKSTYP,GRPKSTYQ                                               
         MVC    GRPKAGMD,BAGYMD                                                 
         MVC    GRPKID,SVID                                                     
         XC     GRPKCODE,GRPKCODE                                               
         GOTO1  HIGH                                                            
         CLI    DMCB+8,0                                                        
         BE     *+6                                                             
         DC     H'0'                                                            
         CLC    KEY(L'GRPKEY),KEYSAVE                                           
         BE     *+6                                                             
         DC     H'0'                                                            
*                                                                               
         GOTO1  GETREC                                                          
         CLI    DMCB+8,0                                                        
         BE     *+6                                                             
         DC     H'0'                                                            
*                                                                               
         L      R6,AIO                                                          
         MVI    ELCODE,GRPBRKCQ                                                 
         BAS    RE,GETEL                                                        
         BE     *+6                                                             
         DC     H'0'                                                            
*                                                                               
         USING  GRPBRKD,R6                                                      
         XC     P,P                                                             
         MVC    PBK1,GRPBK1                                                     
         MVC    PBK2,GRPBK2                                                     
         ZIC    R1,GRPBK1LN                                                     
         ZIC    R0,GRPBK2LN                                                     
         AR     R1,R0                                                           
         BCTR   R1,0                                                            
         DROP   R6                                                              
*                                                                               
         EX     R1,*+8                                                          
         B      *+10                                                            
         MVC    PCODE(0),FULL                                                   
         MVC    PID,SVID                                                        
         MVC    PNM1,SVNM1                                                      
         MVC    PNM2,SVNM2                                                      
*                                                                               
DR30     MVC    8(L'SFSLIN,R2),P                                                
         OI     6(R2),X'80'                                                     
*                                                                               
         ZIC    RE,0(R2)                                                        
         AR     R2,RE                                                           
         XC     KEY,KEY                                                         
         MVC    KEY(L'SVKEY),SVKEY                                              
         GOTO1  HIGH                                                            
         LA     R4,KEY                                                          
         USING  GRPPKEY,R4                                                      
         B      DR10                                                            
*                                                                               
DRX      B      XIT                                                             
         DROP   R4                                                              
         EJECT                                                                  
******************************************************                          
CLRS     NTR1                                                                   
         LA    R2,SFSLINH                                                       
         LA    R3,SFSFILLH                                                      
CLR10    CR    R2,R3                                                            
         BH    CLRX                                                             
         XC    8(L'SFSLIN,R2),8(R2)                                             
         OI    6(R2),X'80'                                                      
         ZIC   RE,0(R2)                                                         
         AR    R2,RE                                                            
         B     CLR10                                                            
CLRX     XIT1                                                                   
******************************************************                          
SETUP    NTR1                                                                   
         OI    GENSTAT1,USKYMRG+NOSETEFH                                        
         OI    CONSERVH+1,X'01'    MODIFY SERVICE REQUEST                       
         OI    CONSERVH+6,X'80'    TRANSMIT TO GET CONTROL                      
         OI    GENSTAT4,NODELLST   NO DEL ALLOWED                               
         OI    GENSTAT2,DISTHSPG                                                
SETUPX   B     XIT                                                              
*                                                                               
         EJECT                                                                  
*******************************************************************             
SAVEDEF  DS    0H                  SAVE DEFINITION BEFORE SETDEF                
         MVC   MYSYSDIR,SYSDIR                                                  
         MVC   MYSYSFIL,SYSFIL                                                  
         MVC   MYUSEIO,USEIO                                                    
         MVC   MYACELOP,ACTELOPT                                                
         MVC   MYLKEY,LKEY                                                      
         BR    RE                                                               
*                                                                               
SETDEF   MVC   SYSDIR,=C'STATION '      SET TO READ STATION FILE                
         MVC   SYSFIL,=C'STATION '                                              
         MVI   USEIO,C'Y'                                                       
         MVI   ACTELOPT,C'N'            NO ACTIVITY ELEMENTS                    
         MVC   LKEY,=H'15'              SET LENGTH OF STATION KEY               
         BR    RE                                                               
*                                                                               
RSTRDEF  DS    0H                  RESTORE DEFINITION AFTER SETDEF              
         MVC   SYSDIR,MYSYSDIR                                                  
         MVC   SYSFIL,MYSYSFIL                                                  
         MVC   USEIO,MYUSEIO                                                    
         MVC   ACTELOPT,MYACELOP                                                
         MVC   LKEY,MYLKEY                                                      
         BR    RE                                                               
*                                                                               
ZEROES   DC    60C'0'                                                           
*                                                                               
         EJECT                                                                  
         GETEL R6,DATADISP,ELCODE                                               
         SPACE 2                                                                
TRAPERR  GOTO1 ERREX                                                            
         SPACE 2                                                                
BADLENG  XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'BADLENGM),BADLENGM                                     
         GOTO1 ERREX2                                                           
BADLENGM DC    C'* ERROR * LENGTH MUST BE WITHIN RANGE 1 TO 4'                  
         SPACE 2                                                                
NOLENG   XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'NOLENGM),NOLENGM                                       
         GOTO1 ERREX2                                                           
NOLENGM  DC    C'* ERROR * LENGTH INVALID WITHOUT BREAK NAME'                   
         SPACE 2                                                                
BADSUML  XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'BADSUMLM),BADSUMLM                                     
         GOTO1 ERREX2                                                           
BADSUMLM DC    C'* ERROR * SUM OF BREAK LENGTHS MAY NOT EXCEED 4'               
         SPACE 2                                                                
NOCHANGE XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'NOCHANGM),NOCHANGM                                     
         GOTO1 ERREX2                                                           
NOCHANGM DC    C'* ERROR * BREAK LENGTHS CANNOT BE CHANGED'                     
         SPACE 2                                                                
         EJECT                                                                  
         LTORG                                                                  
         PRINT OFF                                                              
       ++INCLUDE DDBIGBOX                                                       
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE NESFMFFD                                                       
         PRINT ON                                                               
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE NESFMBBD                                                       
         PRINT OFF                                                              
       ++INCLUDE DDGENTWA                                                       
       ++INCLUDE NESFMWORKD                                                     
         PRINT ON                                                               
         SPACE 5                                                                
* WORK AREA                                                                     
*                                                                               
SYSD     DSECT                                                                  
         ORG   SYSSPARE                                                         
MYSYSDIR DS    CL(L'SYSDIR)        SAVED SYSDIR VALUE BEFORE SETDEF RTN         
MYSYSFIL DS    CL(L'SYSFIL)          "   SYSFIL   "     "      "     "          
MYUSEIO  DS    CL(L'USEIO)           "   USEIO    "     "      "     "          
MYACELOP DS    CL(L'ACTELOPT)        "   ACTELOPT "     "      "     "          
MYLKEY   DS    CL(L'LKEY)            "   LKEY     "     "      "     "          
*                                                                               
SAVEKEY  DS    XL48                                                             
SVID     DS    CL1                                                              
SVNM1    DS    CL24                                                             
SVNM2    DS    CL24                                                             
         EJECT                                                                  
       ++INCLUDE SPGENGRP                                                       
       ++INCLUDE SPGENSTA                                                       
         EJECT                                                                  
*                                                                               
SPOOLD   DSECT                                                                  
         ORG  P                                                                 
PID      DS   CL1                                                               
         DS   CL2                                                               
PCODE    DS   CL4                                                               
         DS   CL1                                                               
PBK1     DS   CL12                                                              
         DS   CL2                                                               
PNM1     DS   CL20                                                              
         DS   CL2                                                               
PBK2     DS   CL12                                                              
         DS   CL2                                                               
PNM2     DS   CL20                                                              
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'114NESFM29   10/31/05'                                      
         END                                                                    
