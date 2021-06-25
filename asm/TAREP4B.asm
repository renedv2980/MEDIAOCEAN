*          DATA SET TAREP4B    AT LEVEL 048 AS OF 05/01/02                      
*PHASE T7034BA,*                                                                
         TITLE 'T7034B - LASER LABEL PRINTING'                                  
T7034B   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 MYEND-MYSTART,T7034B,R7                                          
         LR    RA,RC                                                            
         USING MYD,RA                                                           
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         L     R9,ASUBSYSD                                                      
         USING SUBSYSD,R9          SYSTEM SPECIFIC WORK                         
         L     R6,ATWA                                                          
         USING CONHEADH-64,R6                                                   
         GOTO1 INITIAL,DMCB,0                                                   
         SPACE 1                                                                
         CLI   MODE,PRINTREP                                                    
         BNE   XIT                                                              
         BAS   RE,VREC                                                          
         BAS   RE,PREP                                                          
         B     XIT                                                              
         EJECT                                                                  
*              VALIDATE RECORD                                                  
         SPACE 3                                                                
VREC     NTR1                                                                   
         MVC   CODELINE,SPACES                                                  
         SPACE 1                                                                
VAL2     MVC   LABELS,=H'1'        DEFAULT LABELS TO 1                          
         LA    R2,SPLLABH                                                       
         CLI   5(R2),0                                                          
         BE    VAL4                                                             
         TM    4(R2),X'08'         VALID NUMBERIC                               
         BNO   INVERR                                                           
         ZIC   R1,5(R2)                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         PACK  DUB,8(0,R2)                                                      
         CVB   R1,DUB                                                           
         STH   R1,LABELS                                                        
         SPACE 1                                                                
VAL4     XC    THISAGY,THISAGY     AGENCY                                       
         XC    THISATT,THISATT                                                  
         XC    THISAGG,THISAGG                                                  
         XC    THISOFF,THISOFF                                                  
         XC    THISFILT,THISFILT                                                
         XC    THISAGNT,THISAGNT                                                
         XC    THISUN,THISUN                                                    
         XC    THISLOCL,THISLOCL                                                
         MVI   ANYREQ,C'N'                                                      
         LA    R2,SPLAGYH                                                       
         CLI   5(R2),0                                                          
         BE    VAL6                                                             
         GOTO1 ANY                                                              
         MVI   ANYREQ,C'Y'                                                      
         MVC   THISAGY,WORK                                                     
         MVC   CODELINE+30(6),WORK                                              
         GOTO1 RECVAL,DMCB,TLAYCDQ,(R2),0                                       
         SPACE 1                                                                
         LA    R2,SPLATTH          OPTIONALLY, ATTENTION AS WELL                
         CLI   5(R2),0                                                          
         BE    XIT                                                              
         GOTO1 ANY                                                              
         MVC   THISATT,WORK                                                     
         MVC   CODELINE+37(2),WORK                                              
         GOTO1 RECVAL,DMCB,TLATCDQ,(R2),0                                       
         B     XIT                                                              
         SPACE 1                                                                
VAL6     DS    0H                  AGENCY GROUP                                 
         LA    R2,SPLAGGH                                                       
         CLI   5(R2),0                                                          
         BE    VAL8                                                             
         GOTO1 ANY                                                              
         MVI   ANYREQ,C'Y'                                                      
         MVC   THISAGG,WORK                                                     
         MVC   CODELINE+30(5),=C'GROUP'                                         
         MVC   CODELINE+36(6),WORK                                              
         GOTO1 RECVAL,DMCB,TLAGCDQ,(R2),0                                       
         SPACE 1                                                                
VAL8     DS    0H                  TP OFFICE                                    
         LA    R2,SPLOFFH                                                       
         CLI   5(R2),0                                                          
         BE    VAL10                                                            
         MVI   ANYREQ,C'Y'                                                      
         MVC   THISOFF,SPLOFF                                                   
         MVC   CODELINE+30(3),=C'TPO'                                           
         MVC   CODELINE+34(1),WORK                                              
         GOTO1 RECVAL,DMCB,TLOFCDQ,(R2),0                                       
         SPACE 1                                                                
VAL10    DS    0H                  FILTERS                                      
         LA    R2,SPLFILTH                                                      
         CLI   5(R2),0                                                          
         BE    VAL12                                                            
         MVC   THISFILT,SPLFILT                                                 
         SPACE 1                                                                
VAL12    LA    R2,SPLAGNH          AGENT                                        
         CLI   5(R2),0                                                          
         BE    VAL14                                                            
         GOTO1 ANY                                                              
         MVI   ANYREQ,C'Y'                                                      
         MVC   THISAGNT,WORK                                                    
         MVC   CODELINE+30(5),=C'AGENT'                                         
         MVC   CODELINE+36(4),WORK                                              
         CLC   =C'ALL',THISAGNT                                                 
         BE    VAL14                                                            
         GOTO1 RECVAL,DMCB,TLANCDQ,(R2),0                                       
         SPACE 1                                                                
VAL14    LA    R2,SPLUNH           UNION/LOCAL                                  
         CLI   5(R2),0                                                          
         BE    VAL16                                                            
         GOTO1 ANY                                                              
         MVC   THISUN,WORK                                                      
         MVC   TIFUN,WORK                                                       
         MVC   CODELINE+30(3),WORK                                              
         GOTO1 UNIVAL,DMCB,THISUN                                               
         LA    R2,SPLLOCH          LOCAL                                        
         GOTO1 ANY                 (REQIRED FIELD)                              
         MVI   ANYREQ,C'Y'                                                      
         MVC   THISLOCL,WORK                                                    
         MVC   CODELINE+34(3),WORK                                              
         CLC   =C'ALL',THISLOCL                                                 
         BE    VAL16                                                            
         GOTO1 RECVAL,DMCB,TLLOCDQ,(R2),0                                       
         LA    R2,SPLUNH           UNION REUQT                                  
         SPACE 1                                                                
VAL16    B     XIT                                                              
         SPACE 1                                                                
INVERR   MVI   ERROR,INVALID                                                    
         GOTO1 ERRXIT                                                           
         DROP  R6                                                               
         EJECT                                                                  
*              PRINT REPORT                                                     
         SPACE 3                                                                
PREP     NTR1                                                                   
****     L     R1,DRONE            ON THE VERY FIRST TIME                       
****     CLI   0(R1),X'90'                                                      
****     BNE   PREP1                                                            
****     MVI   0(R1),0                                                          
****     MVI   FORCEHED,C'Y'       ENSURE A SKIP TO CHANNEL 1                   
****     GOTO1 SPOOL,DMCB,(R8)                                                  
         SPACE 1                                                                
PREP1    MVI   LABCOUNT,0                                                       
         MVI   LABCNT2,4                                                        
         OC    THISAGNT,THISAGNT   SINGLE AGENT REQUESTED                       
         BZ    PREP2                                                            
         CLC   =C'ALL',THISAGNT                                                 
         BE    PREPALL2                                                         
         XC    KEY,KEY             READ AGENT                                   
         LA    R4,KEY                                                           
         USING TLAND,R4                                                         
         MVI   TLANCD,TLANCDQ                                                   
         MVC   TLANAGT,THISAGNT                                                 
         GOTO1 READ                                                             
         GOTO1 GETREC                                                           
         L     R6,AIO                                                           
         B     IOHOOK2                                                          
         SPACE 1                                                                
PREP2    OC    THISLOCL,THISLOCL   SINGLE LOCAL REQUESTED                       
         BZ    PREP4                                                            
         CLC   =C'ALL',THISLOCL                                                 
         BE    PREPALL3                                                         
         XC    KEY,KEY             READ LOCAL                                   
         LA    R4,KEY                                                           
         USING TLLOD,R4                                                         
         MVI   TLLOCD,TLLOCDQ                                                   
         MVC   TLLOUN,THISUN                                                    
         MVC   TLLOLCL,THISLOCL                                                 
         GOTO1 READ                                                             
         GOTO1 GETREC                                                           
         L     R6,AIO                                                           
         B     IOHOOK2                                                          
         SPACE 1                                                                
PREP4    OC    THISAGY,THISAGY     SINGLE AGENCY REQUESTED                      
         BZ    PREPALL                                                          
         OC    THISATT,THISATT                                                  
         BNZ   PREP6                                                            
         XC    KEY,KEY             READ AGENCY                                  
         LA    R4,KEY                                                           
         USING TLAYD,R4                                                         
         MVI   TLAYCD,TLAYCDQ                                                   
         MVC   TLAYAGY,THISAGY                                                  
         GOTO1 READ                                                             
         GOTO1 GETREC                                                           
         L     R6,AIO                                                           
         B     IOHOOK2                                                          
         SPACE 1                                                                
PREP6    XC    KEY,KEY             READ ATTENTION RECORD                        
         LA    R4,KEY                                                           
         USING TLATD,R4                                                         
         MVI   TLATCD,TLATCDQ                                                   
         MVC   TLATAGY,THISAGY                                                  
         MVC   TLATATT,THISATT                                                  
         GOTO1 READ                                                             
         GOTO1 GETREC                                                           
         L     R6,AIO                                                           
         B     IOHOOK2                                                          
         SPACE 1                                                                
PREPALL  DS    0H                  MULTIPLE AGENCIES                            
         MVI   TIREAD,TLAYCDQ      SET TO READ AGENCIES                         
         MVC   TIFAGG,THISAGG                                                   
         MVC   TIFOFF,THISOFF                                                   
         B     *+8                                                              
PREPALL2 MVI   TIREAD,TLANNCDQ     MULTIPLE AGENTS - SET TO READ AGENTS         
         B     *+8                                                              
PREPALL3 MVI   TIREAD,TLLOCDQ      MULTIPLE LOCALS - SET TO READ LOCALS         
         SPACE 1                                                                
         MVC   TIACOMFC,ACOMFACS   SET UP FOR TASYSIO                           
         LA    R1,IOHOOK                                                        
         ST    R1,TIHOOK                                                        
         MVC   TIFFILT1(4),THISFILT                                             
         GOTO1 TASYSIO,DMCB,TASYSIOD                                            
         BAS   RE,SPLAT                                                         
         B     XIT                                                              
         SPACE 1                                                                
IOHOOK   NTR1                                                                   
         CLI   TIMODE,PROCREC      IF THERE IS SOMETHING INTERESTING            
         BNE   XIT                                                              
         CLC   TIAGY,=C'999999'    NO ADJUSTMENTS                               
         BE    XIT                                                              
         L     R6,TIAREC                                                        
         SPACE 1                                                                
IOHOOK2  GOTO1 GETNAME,DMCB,AGYNAME     FOR NAME                                
         GOTO1 GETADD,DMCB,AGYADD       AND ADDRESS                             
         MVI   FREECODE,TAFNTATT                                                
         GOTO1 GETFREE,DMCB,ATTNAME     AND ATTENTION                           
         LH    R0,LABELS                                                        
         SPACE 1                                                                
IOHOOK4  CLI   0(R6),TLANCDQ       IF PROCESSING AGENTS                         
         BNE   IOHOOK4D                                                         
         CLI   LABCOUNT,1          DEPENDING ON WHICH SIDE                      
         BE    IOHOOK4B                                                         
         MVC   CODELINE+40(40),SPACES                                           
         MVC   CODELINE+36(4),TLANAGT-TLAND(R6)  DISPLAY CURRENT CODE           
         B     *+16                                                             
IOHOOK4B MVC   CODELINE+40(40),CODELINE                                         
         MVC   CODELINE+76(4),TLANAGT-TLAND(R6)                                 
         B     IOHOOK5                                                          
         SPACE 1                                                                
IOHOOK4D CLI   0(R6),TLLOCDQ       IF PROCESSING LOCALS                         
         BNE   IOHOOK5                                                          
         CLI   LABCOUNT,1          DEPENDING ON WHICH SIDE                      
         BE    IOHOOK4E                                                         
         MVC   CODELINE+37(43),SPACES                                           
         MVC   CODELINE+34(3),TLLOLCL-TLLOD(R6)  DISPLAY CURRENT CODE           
         B     *+16                                                             
IOHOOK4E MVC   CODELINE+40(40),CODELINE                                         
         MVC   CODELINE+74(3),TLLOLCL-TLLOD(R6)                                 
         SPACE 1                                                                
IOHOOK5  BAS   RE,DOLABEL          PRINT N LABELS                               
         BCT   R0,IOHOOK4                                                       
         SPACE 1                                                                
         OC    THISAGY,THISAGY     IF 1 AGENCY REQUESTED,                       
         BNZ   IOHOOK6                                                          
         OC    THISAGNT,THISAGNT   OR 1 AGENT                                   
         BZ    *+14                                                             
         CLC   =C'ALL',THISAGNT                                                 
         BNE   IOHOOK6                                                          
         OC    THISLOCL,THISLOCL   OR 1 LOCAL                                   
         BZ    *+14                                                             
         CLC   =C'ALL',THISLOCL                                                 
         BNE   IOHOOK6                                                          
         B     XIT                                                              
         SPACE 1                                                                
IOHOOK6  CLI   LABCOUNT,0          END OF REQUEST - PRINT LURKER                
         BE    XIT                                                              
         BAS   RE,SPLAT                                                         
         B     XIT                                                              
         EJECT                                                                  
*              PRINT A LABEL FOR CURRENT AGENCY                                 
         SPACE 3                                                                
DOLABEL  NTR1                                                                   
         LA    R2,LABPOOL+5        ADDRESS FIRST                                
         CLI   LABCOUNT,1                                                       
         BNE   *+8                                                              
         LA    R2,40(R2)           OR SECOND LABEL AREA                         
*                                  IF AROUND, ATTENTION COMES FIRST             
         CLI   ATTNAME,C'A'                                                     
         BL    DOLAB2                                                           
         MVC   0(5,R2),=C'ATTN:'                                                
         MVC   6(30,R2),ATTNAME                                                 
         LA    R2,132(R2)                                                       
         SPACE 1                                                                
*                                  MOVE IN NAME AND ADDRESS                     
DOLAB2   MVC   0(36,R2),AGYNAME                                                 
         LA    R2,132(R2)                                                       
         MVC   0(30,R2),AGYADD                                                  
         LA    R2,132(R2)                                                       
         MVC   0(30,R2),AGYADD+30                                               
         LA    R2,132(R2)                                                       
         MVC   0(30,R2),AGYADD+60                                               
         LA    R2,132(R2)                                                       
         MVC   0(30,R2),AGYADD+90                                               
         SPACE 1                                                                
DOLAB4   AI    LABCOUNT,1                                                       
         AI    LABCNT2,1                                                        
*                                                                               
         CLI   LABCOUNT,1                                                       
         BE    XIT                                                              
         BAS   RE,SPLAT            IF WE HAVE FINISHED BOTH, PRINT              
         MVI   LABCOUNT,0                                                       
         B     XIT                                                              
         SPACE 1                                                                
LABCOUNT DC    X'00'                                                            
LABCNT2  DC    X'00'                                                            
NOPRYET  DC    C'N'                                                             
         EJECT                                                                  
*              ODD ROUTINES                                                     
         SPACE 3                                                                
GETW4    NTR1                                                                   
         L     R2,0(R1)            GET W4 NAME                                  
*                                  (R6=A(RECORD))                               
         MVI   ELCODE,TAW4ELQ                                                   
         BAS   RE,GETEL                                                         
         MVC   0(36,R2),SPACES                                                  
         BNE   XIT                                                              
         USING TAW4D,R6                                                         
         MVC   0(32,R2),TAW4CRPN                                                
         CLI   TAW4TYPE,TAW4TYCO                                                
         BE    XIT                                                              
         CLI   TAW4TYPE,TAW4TYTR                                                
         BE    XIT                                                              
         MVC   0(32,R2),SPACES                                                  
         MVC   0(16,R2),TAW4NAM1                                                
         MVC   17(16,R2),TAW4NAM2                                               
         GOTO1 SQUASHER,DMCB,(R2),33                                            
         B     XIT                                                              
         SPACE 1                                                                
GETNAME  NTR1                                                                   
         L     R2,0(R1)            GET NAME OUT                                 
*                                  (R6=A(RECORD))                               
         MVI   ELCODE,TANAELQ                                                   
         BAS   RE,GETEL                                                         
         MVC   0(36,R2),SPACES                                                  
         BNE   XIT                                                              
         USING TANAD,R6                                                         
         ZIC   R1,TANALEN                                                       
         SH    R1,=H'3'                                                         
         EX    R1,*+8                                                           
         B     XIT                                                              
         MVC   0(0,R2),TANANAME                                                 
         SPACE 1                                                                
GETADD   NTR1                                                                   
         L     R2,0(R1)            GET ADDRESS                                  
*                                  (R6=A(RECORD))                               
         MVI   ELCODE,TAADELQ                                                   
         BAS   RE,GETEL                                                         
         MVC   0(120,R2),SPACES                                                 
         BNE   XIT                                                              
         USING TAADD,R6                                                         
         ZIC   R1,TAADLEN                                                       
         SH    R1,=H'4'                                                         
         EX    R1,*+8                                                           
         B     XIT                                                              
         MVC   0(0,R2),TAADADD                                                  
         SPACE 1                                                                
GETFREE  NTR1                                                                   
         L     R2,0(R1)            GET NAME OUT                                 
*                                  (R6=A(RECORD))                               
         MVI   ELCODE,TAFNELQ                                                   
         BAS   RE,GETEL                                                         
         MVC   0(36,R2),SPACES                                                  
         BNE   XIT                                                              
         SPACE 1                                                                
         USING TAFND,R6                                                         
GETFREE2 CLC   FREECODE,TAFNTYPE                                                
         BE    GETFREE4                                                         
         BAS   RE,NEXTEL                                                        
         BNE   XIT                                                              
         B     GETFREE2                                                         
         SPACE 1                                                                
GETFREE4 ZIC   R1,TAFNLEN                                                       
         SH    R1,=H'4'                                                         
         EX    R1,*+8                                                           
         B     XIT                                                              
         MVC   0(0,R2),TAFNNAME                                                 
         SPACE 1                                                                
YES      SR    R1,R1                                                            
         B     *+8                                                              
         SPACE 1                                                                
NO       LA    R1,1                                                             
         LTR   R1,R1                                                            
         B     XIT                                                              
         SPACE 1                                                                
SPLAT    NTR1                                                                   
         MVI   FORCEHED,C'N'                                                    
         MVI   LINE,0                                                           
         CLI   NOPRYET,C'N'        IF YET TO PRINT OUT A LINE                   
         BNE   SPLAT1A                                                          
         MVI   NOPRYET,0           PRINT OUT 23 BLANK LINES                     
         LA    R0,23                                                            
SPLAT1   GOTO1 SPOOL,DMCB,(R8)                                                  
         BCT   R0,SPLAT1                                                        
         SPACE 1                                                                
SPLAT1A  LA    R0,7                                                             
         MVC   P,CODELINE                                                       
         OC    THISAGNT,THISAGNT                                                
         BNZ   SPLAT2                                                           
         CLC   THISLOCL,=C'ALL'                                                 
         BE    SPLAT2                                                           
         CLC   LABPOOL+40(40),SPACES                                            
         BE    *+10                                                             
         MVC   P+40(40),CODELINE                                                
SPLAT2   GOTO1 SPOOL,DMCB,(R8)     SKIP 7 LINES AFTER CODELINES                 
         BCT   R0,SPLAT2                                                        
         SPACE 1                                                                
         LA    R2,LABPOOL                                                       
         LA    R0,6                                                             
SPLAT4   MVC   P,0(R2)                                                          
         MVC   0(132,R2),SPACES                                                 
         GOTO1 SPOOL,DMCB,(R8)                                                  
         LA    R2,132(R2)                                                       
         BCT   R0,SPLAT4                                                        
         SPACE 1                                                                
         LA    R0,10               SKIP 10 LINES AFTER LAB POOL                 
         CLI   LABCNT2,8                                                        
         BNE   SPLAT6              OR 16 LINES IF END OF PAGE                   
         LA    R0,16                                                            
         MVI   LABCNT2,0                                                        
SPLAT6   GOTO1 SPOOL,DMCB,(R8)                                                  
         BCT   R0,SPLAT6                                                        
         B     XIT                                                              
         SPACE 1                                                                
         GETEL R6,DATADISP,ELCODE                                               
         SPACE 1                                                                
XIT      XIT1                                                                   
         EJECT                                                                  
*              LTORG ETC                                                        
         SPACE 3                                                                
         LTORG                                                                  
         SPACE 1                                                                
CODELINE DC    CL132' '                                                         
         SPACE 1                                                                
LABPOOL  DS    0D                                                               
         DC    CL132' '                                                         
         DC    CL132' '                                                         
         DC    CL132' '                                                         
         DC    CL132' '                                                         
         DC    CL132' '                                                         
         DC    CL132' '                                                         
         EJECT                                                                  
*              MY WORKING STORAGE                                               
         SPACE 3                                                                
MYD      DSECT                                                                  
MYSTART  DS    0D                                                               
LABELS   DS    H                                                                
THISSSN  DS    CL9                                                              
THISAGNT DS    CL4                                                              
THISUN   DS    CL3                                                              
THISLOCL DS    CL3                                                              
THISAGY  DS    CL6                                                              
THISATT  DS    CL2                                                              
THISFILT DS    CL4                                                              
THISAGG  DS    CL6                                                              
THISOFF  DS    CL1                                                              
ANYREQ   DS    CL1                                                              
AGYNAME  DS    CL36                                                             
AGYADD   DS    CL120                                                            
W4NAME   DS    CL36                                                             
CORPNAME DS    CL36                                                             
CORPSSN  DS    CL9                                                              
AGNTNAME DS    CL36                                                             
W4ADD    DS    CL120                                                            
ATTNAME  DS    CL36                                                             
FREECODE DS    CL1                                                              
         SPACE 1                                                                
MYEND    DS    0D                                                               
         SPACE 3                                                                
*                                  PRINT LINE DSECT FOR SUMMARY                 
         EJECT                                                                  
*              OTHER DSECTS ARE HIDDEN IN HERE                                  
         SPACE 3                                                                
*TAREPWORKD                                                                     
*DDSPOOLD                                                                       
*DDSPLWORKD                                                                     
*DDBIGBOX                                                                       
*TAGENFILE                                                                      
*FAFACTS                                                                        
*FATIOB                                                                         
*TAREPFFD                                                                       
         PRINT OFF                                                              
       ++INCLUDE TAREPWORKD                                                     
       ++INCLUDE DDSPOOLD                                                       
         PRINT OFF                                                              
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE DDBIGBOX                                                       
       ++INCLUDE TAGENFILE                                                      
       ++INCLUDE FAFACTS                                                        
       ++INCLUDE FATIOB                                                         
       ++INCLUDE TAREPFFD                                                       
         PRINT ON                                                               
         ORG   CONTAGH                                                          
         EJECT                                                                  
       ++INCLUDE TAREPE7D                                                       
       ++INCLUDE DDGENTWA                                                       
         SPACE 1                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'048TAREP4B   05/01/02'                                      
         END                                                                    
