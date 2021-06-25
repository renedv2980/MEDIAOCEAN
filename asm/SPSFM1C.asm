*          DATA SET SPSFM1C    AT LEVEL 005 AS OF 05/01/02                      
*PHASE T2171CA                                                                  
*                                                                               
***********************************************************************         
*                                                                     *         
*  TITLE        T2171C - PAYMENT SPLIT RECORD MAINTENANCE/LIST        *         
*                                                                     *         
*  CALLED FROM  GENCON VIA T21700 (SFM CONTROLLER)                    *         
*                                                                     *         
*  COMMENTS     SUPPORTS ADD, DISPLAY, DELETE, RESTORE, CHANGE, LIST  *         
*                                                                     *         
*  INPUTS       SCREEN T217DC (MAINTENANCE)                           *         
*               SCREEN T217DD (LIST)                                  *         
*                                                                     *         
*  OUTPUTS      UPDATED PSR RECORDS                                   *         
*                                                                     *         
*  REGISTERS    R0 -- WORK                                            *         
*               R1 -- WORK                                            *         
*               R2 -- WORK                                            *         
*               R3 -- WORK                                            *         
*               R4 -- WORK                                            *         
*               R5 -- WORK                                            *         
*               R6 -- WORK                                            *         
*               R7 -- WORK                                            *         
*               R8 -- WORK                                            *         
*               R9 -- SYSD                                            *         
*               RA -- TWA                                             *         
*               RB -- FIRST BASE                                      *         
*               RC -- GEND                                            *         
*               RD -- SYSTEM                                          *         
*               RE -- SYSTEM                                          *         
*               RF -- SYSTEM                                          *         
*                                                                     *         
*  I/O AREAS    IO1 - PSR RECORD                                      *         
*               IO2 - MISC.                                           *         
*                                                                     *         
***********************************************************************         
         TITLE 'T2171C - PAYMENT SPLIT RECORDS (PSR)'                           
T2171C   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T2171C                                                         
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA                                                   
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
*                                                                               
         CLI   MODE,VALKEY         VALIDATE RECORD KEY                          
         BE    VK                                                               
         CLI   MODE,VALREC         VALIDATE RECORD                              
         BE    VR                                                               
         CLI   MODE,DISPKEY        DISPLAY KEY                                  
         BE    DK                                                               
         CLI   MODE,LISTRECS       LIST RECORDS                                 
         BE    LR                                                               
         CLI   MODE,DISPREC        DISPLAY RECORD                               
         BE    DR                                                               
*                                                                               
XIT      XIT1                                                                   
         EJECT                                                                  
* VALIDATE KEY ROUTINE                                                          
*                                                                               
VK       LA    R6,SVKEY                                                         
         USING PSRKEY,R6                                                        
         XC    SVKEY,SVKEY                                                      
         MVC   PSRKTYPE,=X'0D48'   PSR RECORD TYPE                              
*                                                                               
         LA    R2,PSRMEDH          MEDIA                                        
         GOTO1 VALIMED                                                          
         MVC   PSRKAM,BAGYMD                                                    
*                                                                               
         XC    BCLT,BCLT                                                        
         LA    R2,PSRCLTH          CLIENT FIELD                                 
         CLI   5(R2),0             TEST CLIENT GIVEN                            
         BNE   VK10                YES                                          
*                                                                               
         CLI   ACTNUM,ACTLIST      TEST ACTION LIST                             
         BE    VK20                YES - OPTIONAL                               
         MVI   ERROR,MISSING       REQUIRED FIELD                               
         B     TRAPERR                                                          
*                                                                               
VK10     GOTO1 VALICLT                                                          
         MVC   PSRKCLT,BCLT                                                     
*                                                                               
VK20     XC    BREP,BREP           REP CODE FIELD                               
         LA    R2,PSRREPH                                                       
         CLI   5(R2),0             TEST REP GIVEN                               
         BNE   VK30                                                             
*                                                                               
         CLI   ACTNUM,ACTLIST      TEST ACTION LIST                             
         BE    VKX                 YES - OPTIONAL                               
         MVI   ERROR,MISSING       REQUIRED FIELD                               
         B     TRAPERR                                                          
*                                                                               
VK30     TM    PSRREPH+4,X'08'     TEST VALID NUMERIC                           
         BZ    INVREPK             NO                                           
         ZIC   R1,PSRREPH+5                                                     
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         PACK  DUB,PSRREP(0)                                                    
         CVB   R1,DUB                                                           
         STH   R1,BREP             BINARY REP CODE                              
*                                                                               
         EDIT  BREP,(3,REPCODE),FILL=0                                          
         BAS   RE,VREP             VALIDATE REP CODE                            
         BNE   INVREPK             INVALID                                      
         MVC   PSRKREP,BREP                                                     
*                                                                               
VKX      MVC   KEY,SVKEY           GENCON NEEDS KEY                             
         B     XIT                                                              
         DROP  R6                                                               
         EJECT                                                                  
* VALIDATE RECORD                                                               
*                                                                               
VR       MVI   ELCODE,PSREL5Q      GET RID OF PAYMENT ELEMENTS                  
         GOTO1 REMELEM                                                          
         LA    R2,PSRDATEH         A(FIRST DATE FIELD)                          
         CLI   5(R2),0             TEST ANY DATA                                
         BNE   VR20                YES                                          
         MVI   ERROR,MISSING                                                    
         B     TRAPERR                                                          
*                                                                               
VR10     CLI   5(R2),0             TEST ANY MORE DATA                           
         BE    DR                  NO                                           
*                                                                               
VR20     XC    ELEM,ELEM                                                        
         USING PSREL5,R6                                                        
         LA    R6,ELEM                                                          
         MVI   PSREL5,PSREL5Q      ELEMENT CODE                                 
         MVI   PSREL5LN,PSREL5LQ   ELEMENT LENGTH                               
*                                                                               
         XC    WORK,WORK                                                        
         GOTO1 DATVAL,DMCB,(0,8(R2)),WORK                                       
         OC    0(4,R1),0(R1)       TEST VALID M/D/Y                             
         BZ    *+12                                                             
         MVI   ERROR,INVDATE                                                    
         B     TRAPERR                                                          
*                                                                               
         GOTO1 DATVAL,DMCB,(2,8(R2)),WORK                                       
         OC    0(4,R1),0(R1)       TEST MONTH/YEAR VALID                        
         BNZ   *+12                YES                                          
         MVI   ERROR,INVDATE                                                    
         B     TRAPERR                                                          
*                                                                               
         GOTO1 DATCON,DMCB,(0,WORK),(3,WORK+6)                                  
         MVC   PSR5DATE,WORK+6     PUT EFFECTIVE DATE IN ELEMENT                
*                                                                               
         ZIC   R0,0(R2)            BUMP TO REPS FIELD                           
         AR    R2,R0                                                            
         CLI   5(R2),0             TEST ANY DATA                                
         BNE   *+12                YES                                          
         MVI   ERROR,MISSING                                                    
         B     TRAPERR                                                          
*                                                                               
         XC    BLOCK(256),BLOCK    CLEAR SCANNER BLOCK                          
         GOTO1 SCANNER,DMCB,(R2),(6,BLOCK)                                      
         CLI   DMCB+4,0            TEST VALID SCANNER DATA                      
         BNE   *+12                YES                                          
         MVI   ERROR,INVALID                                                    
         B     TRAPERR                                                          
*                                                                               
         CLI   DMCB+4,2            TEST NOT ENOUGH REPS                         
         BL    REPTOTAL            YES                                          
         CLI   DMCB+4,5            TEST TOO MANY REPS                           
         BH    REPTOTAL            YES                                          
*                                                                               
         LA    R6,PSR5DATA                                                      
         MVI   OPTNUM,1            FOR ERRORS                                   
         LA    R3,BLOCK            A(SCANNER BLOCK)                             
         XC    PERCENT,PERCENT     CLEAR PERCENTAGE TALLY                       
*                                                                               
VR30     CLI   0(R3),0             TEST END OF BLOCK                            
         BE    VR40                YES                                          
         CLI   0(R3),3             MAX DIGITS IN REP CODE                       
         BH    INVREPC             NO                                           
         TM    2(R3),X'80'         TEST REP CODE NUMERIC                        
         BZ    INVREPC             NO                                           
         CLI   1(R3),0             TEST ANY PERCENTAGE                          
         BE    INVPCT              NO                                           
*                                                                               
         EDIT  (B2,6(R3)),(3,REPCODE),FILL=0                                    
         BAS   RE,VREP             VALIDATE REP CODE                            
         BNE   INVREPC             INVALID                                      
*                                                                               
         CLI   OPTNUM,1            TEST FIRST REP                               
         BNE   *+14                NO                                           
         CLC   BREP,6(R3)          TEST MATCH ON KEY REP                        
         BNE   REPMATCH            NO                                           
         MVC   0(2,R6),6(R3)       PUT REP CODE IN ELEMENT                      
*                                                                               
         ZIC   R5,1(R3)            L'PCT FIELD                                  
         GOTO1 CASHVAL,DMCB,(3,22(R3)),(R5)                                     
         CLI   DMCB,0              TEST VALID PERCENTAGE                        
         BNE   INVPCT                                                           
         MVC   2(4,R6),DMCB+4      PUT PERCENTAGE IN ELEMENT                    
         L     R1,PERCENT          INCREMENT PERCENTAGE TOTAL                   
         A     R1,DMCB+4                                                        
         ST    R1,PERCENT                                                       
*                                                                               
         LA    R6,6(R6)            BUMP TO NEXT ELEMENT FIELD                   
         LA    R3,32(R3)           BUMP TO NEXT SCANNER ENTRY                   
         ZIC   R1,OPTNUM                                                        
         LA    R1,1(R1)                                                         
         STC   R1,OPTNUM                                                        
         B     VR30                                                             
*                                                                               
VR40     L     R1,PERCENT                                                       
         C     R1,=F'100000'       TEST PERCENTAGES TOTAL 100                   
         BNE   PCTTOTAL            NO                                           
*                                                                               
         GOTO1 ADDELEM                                                          
         ZIC   R0,0(R2)            BUMP TO NEXT DATE FIELD                      
         AR    R2,R0                                                            
         B     VR10                                                             
         DROP  R6                                                               
         EJECT                                                                  
* DISPLAY RECORD                                                                
*                                                                               
DR       LA    R2,PSRDATEH         A(FIRST DATE FIELD)                          
*                                                                               
DR10     ZIC   RF,0(R2)            RF HAS LENGTH OF FIELD                       
         SH    RF,=H'17'           SUBTRACT EXTENDED HEADER LENGTH +1           
         EX    RF,*+8              PAD WITH BLANKS                              
         B     *+10                                                             
         OC    8(0,R2),=80X'40'                                                 
         EX    RF,*+8              TEST FIELD EMPTY                             
         B     *+10                                                             
         CLC   8(0,R2),=80X'40'                                                 
         BE    DR20                YES                                          
         EX    RF,*+8              NO - CLEAR FIELD                             
         B     *+10                                                             
         XC    8(0,R2),8(R2)                                                    
         OI    6(R2),X'80'         XMIT                                         
*                                                                               
DR20     ZIC   R0,0(R2)            BUMP                                         
         AR    R2,R0                                                            
         CLI   0(R2),0             TEST END OF SCREEN                           
         BNE   DR10                NO                                           
*                                                                               
         LA    R2,PSRDATEH         A(FIRST DATE FIELD)                          
         L     R6,AIO                                                           
         MVI   ELCODE,PSREL5Q      PAYMENT ELEMENT                              
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         USING PSREL5,R6                                                        
DR30     GOTO1 DATCON,DMCB,(3,PSR5DATE),(6,8(R2))                               
         OI    6(R2),X'80'         XMIT DATE                                    
*                                                                               
         ZIC   R0,0(R2)                                                         
         AR    R2,R0               BUMP TO REPS FIELD                           
         OI    6(R2),X'80'         XMIT REPS                                    
         LA    R3,PSR5DATA                                                      
         LA    R4,8(R2)            R4 = DISPLAY POSITION                        
         LA    R5,5                MAX NUMBER OF REP CODES                      
*                                                                               
DR40     EDIT  (B2,0(R3)),(3,(R4)),ALIGN=LEFT                                   
         AR    R4,R0               BUMP PAST EDITED NUMBER                      
         MVI   0(R4),C'='                                                       
         LA    R4,1(R4)                                                         
         EDIT  (B4,2(R3)),(6,(R4)),3,ALIGN=LEFT                                 
         AR    R4,R0               BUMP PAST EDITED PERCENTAGE                  
         MVI   0(R4),C','                                                       
         LA    R4,1(R4)                                                         
*                                                                               
         LA    R3,6(R3)            BUMP TO NEXT REP CODE                        
         OC    0(2,R3),0(R3)       TEST ANY MORE REP CODES                      
         BZ    *+8                 NO                                           
         BCT   R5,DR40             MAX OF 5 REP CODES                           
*                                                                               
         BCTR  R4,0                BACK UP ONE SPACE                            
         MVI   0(R4),C' '                                                       
         BAS   RE,NEXTEL           LOOK FOR MORE DATES                          
         BNE   DRX                                                              
*                                                                               
         ZIC   R0,0(R2)                                                         
         AR    R2,R0               BUMP TO NEXT DATE FIELD                      
         B     DR30                                                             
*                                                                               
DRX      B     XIT                                                              
         EJECT                                                                  
* DISPLAY KEY                                                                   
*                                                                               
DK       L     R6,AIO              RECORD SELECTED                              
         USING PSRRECD,R6                                                       
*                                                                               
         MVC   PSRMED,QMED                                                      
         OI    PSRMEDH+6,X'80'     MEDIA                                        
         GOTO1 CLUNPK,DMCB,PSRKCLT,PSRCLT                                       
         OI    PSRCLTH+6,X'80'     CLIENT                                       
         EDIT  PSRKREP,(3,PSRREP),ALIGN=LEFT                                    
         OI    PSRREPH+6,X'80'     REP                                          
*                                                                               
         B     XIT                                                              
         DROP  R6                                                               
         EJECT                                                                  
* LIST RECORDS                                                                  
*                                                                               
LR       LA    R4,KEY                                                           
         USING PSRKEY,R4                                                        
*                                                                               
         OC    KEY(13),KEY         TEST FIRST TIME THROUGH                      
         BNZ   LR10                                                             
         MVC   PSRKTYPE,=X'0D48'   RECORD TYPE                                  
         MVC   PSRKAM,BAGYMD       AGY/MED                                      
         MVC   PSRKCLT,BCLT        CLIENT CODE                                  
         MVC   PSRKREP,BREP        REP CODE                                     
         MVC   SAVEKEY,KEY                                                      
*                                                                               
LR10     MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                FIRST RECORD                                 
         B     LR30                                                             
*                                                                               
LR20     LA    R4,KEY              NEXT RECORD                                  
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 SEQ                                                              
*                                                                               
LR30     CLC   KEY(3),SAVEKEY      TEST SAME TYPE/AGY/MED                       
         BNE   LRX                                                              
*                                                                               
         OC    BREP,BREP           TEST REP FILTER GIVEN                        
         BZ    *+14                NO                                           
         CLC   PSRKREP,BREP        TEST MATCH ON REP                            
         BNE   LR20                NO                                           
*                                                                               
         XC    LISTAR,LISTAR       FILL IN LIST LINE                            
         GOTO1 CLUNPK,DMCB,PSRKCLT,LSTCLT  CLIENT                               
         EDIT  PSRKREP,(3,LSTREP)  REP                                          
*                                                                               
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
         L     R6,AIO                                                           
         MVI   ELCODE,PSREL5Q      PAYMENT ELEMENT                              
         BAS   RE,GETEL                                                         
         BE    *+6                 MUST BE AT LEAST ONE                         
         DC    H'0'                                                             
*                                                                               
         USING PSREL5,R6                                                        
         MVC   HALF,PSR5DATE       SAVE THE LAST DATE FOUND                     
         BAS   RE,NEXTEL           FIND THE MOST RECENT ONE                     
         BE    *-10                                                             
*                                                                               
         GOTO1 DATCON,DMCB,(3,HALF),(6,LSTDATE)                                 
         DROP  R6                                                               
*                                                                               
         GOTO1 LISTMON             DISPLAY LINE                                 
         B     LR20                                                             
*                                                                               
LRX      B     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
* VALIDATE REP CODE                                                             
*                                                                               
VREP     NTR1                                                                   
*                                                                               
         XC    SAVEKEY,SAVEKEY     BUILD REP KEY                                
         USING REPRECD,R4                                                       
         LA    R4,SAVEKEY                                                       
*                                                                               
         MVI   REPKTYPE,C'R'       REP RECORD                                   
         MVC   REPKMED,QMED        MEDIA                                        
         MVC   REPKREP,REPCODE     REP                                          
         MVC   REPKAGY,AGENCY      AGENCY CODE                                  
         MVC   REPKFILL,=C'0000000000'                                          
         DROP  R4                                                               
*                                                                               
         GOTO1 DATAMGR,DMCB,(0,=C'DMREAD'),=C'STATION',SAVEKEY,AIO2             
         CLI   8(R1),0                                                          
         BE    *+10                REP RECORD WAS FOUND                         
*                                                                               
         CR    R1,RE               SET CC NOT EQUAL                             
         B     VREPX                                                            
*                                                                               
         CR    R1,R1               SET CC EQUAL                                 
*                                                                               
VREPX    B     XIT                                                              
         EJECT                                                                  
         GETEL R6,DATADISP,ELCODE                                               
*                                                                               
TRAPERR  GOTO1 ERREX                                                            
*                                                                               
INVREPC  OI    6(R2),X'40'         SET CURSOR                                   
         LA    R2,CONHEADH                                                      
         XC    CONHEAD,CONHEAD                                                  
         MVC   8(39,R2),=C'* ERROR * INVALID REP CODE IN FIELD   *'             
         EDIT  OPTNUM,(1,44(R2))                                                
         GOTO1 ERREX2                                                           
*                                                                               
INVPCT   OI    6(R2),X'40'         SET CURSOR                                   
         LA    R2,CONHEADH                                                      
         XC    CONHEAD,CONHEAD                                                  
         MVC   8(41,R2),=C'* ERROR * INVALID PERCENTAGE IN FIELD   *'           
         EDIT  OPTNUM,(1,46(R2))                                                
         GOTO1 ERREX2                                                           
*                                                                               
PCTTOTAL XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'PCTTOTM),PCTTOTM                                       
         GOTO1 ERREX2                                                           
PCTTOTM  DC    C'* ERROR * PERCENTAGES MUST TOTAL TO 100 *'                     
*                                                                               
REPTOTAL XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'REPTOTM),REPTOTM                                       
         GOTO1 ERREX2                                                           
REPTOTM  DC    C'* ERROR * MUST SPECIFY BETWEEN 2 AND 5 REPS *'                 
*                                                                               
REPMATCH XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'REPMATM),REPMATM                                       
         GOTO1 ERREX2                                                           
REPMATM  DC    C'* ERROR * FIRST REP MUST MATCH REP IN KEY *'                   
*                                                                               
INVREPK  XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'INVREPKM),INVREPKM                                     
         GOTO1 ERREX2                                                           
INVREPKM DC    C'* ERROR * INVALID REP CODE *'                                  
         SPACE 3                                                                
         LTORG                                                                  
         PRINT OFF                                                              
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE SPSFMFFD                                                       
         PRINT ON                                                               
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE SPSFMDCD                                                       
         PRINT OFF                                                              
       ++INCLUDE SPSFMWORKD                                                     
         PRINT ON                                                               
         SPACE 5                                                                
* WORK AREA                                                                     
*                                                                               
         ORG   SYSSPARE                                                         
PERCENT  DS    F                   SUM OF PERCENTAGES                           
OPTNUM   DS    X                   SCANNER ENTRY NUMBER                         
REPCODE  DS    CL3                 EBCDIC REP CODE                              
BREP     DS    H                   BINARY REP CODE                              
SAVEKEY  DS    XL48                                                             
         EJECT                                                                  
REPRECD  DSECT                                                                  
       ++INCLUDE SPGENREP                                                       
         EJECT                                                                  
PSRRECD  DSECT                                                                  
       ++INCLUDE SPGENPSR                                                       
         SPACE 5                                                                
GEND     DSECT                                                                  
         ORG   LISTAR                                                           
         DS    CL3                                                              
LSTCLT   DS    CL3                                                              
         DS    CL2                                                              
LSTREP   DS    CL3                                                              
         DS    CL2                                                              
LSTDATE  DS    CL6                                                              
         SPACE 3                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'005SPSFM1C   05/01/02'                                      
         END                                                                    
