*          DATA SET SPSFM1D    AT LEVEL 006 AS OF 05/01/02                      
*PHASE T2171DA                                                                  
*                                                                               
***********************************************************************         
*                                                                     *         
*  TITLE        T2171D - ESTIMATE SPLIT RECORD MAINTENANCE/LIST       *         
*                                                                     *         
*  CALLED FROM  GENCON VIA T21700 (SFM CONTROLLER)                    *         
*                                                                     *         
*  COMMENTS     SUPPORTS ADD, DISPLAY, DELETE, RESTORE, CHANGE, LIST  *         
*                                                                     *         
*  INPUTS       SCREEN T217DE (MAINTENANCE)                           *         
*               SCREEN T217EE (LIST)                                  *         
*                                                                     *         
*  OUTPUTS      UPDATED ESR RECORDS                                   *         
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
*  I/O AREAS    IO1 - ESR RECORD                                      *         
*               IO2 - MISC.                                           *         
*                                                                     *         
***********************************************************************         
         TITLE 'T2171D - ESTIMATE SPLIT RECORDS (ESR)'                          
T2171D   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T2171D                                                         
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
         USING ESRKEY,R6                                                        
         XC    SVKEY,SVKEY                                                      
         MVC   ESRKTYPE,=X'0D53'   ESR RECORD TYPE                              
*                                                                               
         LA    R2,ESRMEDH          MEDIA                                        
         GOTO1 VALIMED                                                          
         MVC   ESRKAM,BAGYMD                                                    
*                                                                               
         XC    BCLT,BCLT                                                        
         LA    R2,ESRCLTH          CLIENT FIELD                                 
         CLI   5(R2),0             TEST CLIENT GIVEN                            
         BNE   VK10                YES                                          
*                                                                               
         CLI   ACTNUM,ACTLIST      TEST ACTION LIST                             
         BE    VKX                 YES - OPTIONAL                               
         MVI   ERROR,MISSING       REQUIRED FIELD                               
         B     TRAPERR                                                          
*                                                                               
VK10     GOTO1 VALICLT                                                          
         MVC   ESRKCLT,BCLT                                                     
*                                                                               
         CLI   ACTNUM,ACTLIST      TEST ACTION LIST                             
         BE    VKX                 YES - NOTHING ELSE TO DO                     
*                                                                               
         XC    QPRD,QPRD           PRODUCT CODE FIELD                           
         LA    R2,ESRPRDH                                                       
         CLI   5(R2),0             TEST PRODUCT GIVEN                           
         BNE   *+12                                                             
         MVI   ERROR,MISSING       REQUIRED FIELD                               
         B     TRAPERR                                                          
*                                                                               
         GOTO1 VALIPRD                                                          
         MVC   QPRD,WORK           SAVE PRODUCT CODE                            
         MVC   ESRKPRD,QPRD                                                     
*                                                                               
         MVI   BEST,0              ESTIMATE FIELD                               
         LA    R2,ESRESTH                                                       
         CLI   5(R2),0             TEST EST GIVEN                               
         BNE   *+12                YES                                          
         MVI   ERROR,MISSING       REQUIRED FIELD                               
         B     TRAPERR                                                          
*                                                                               
         TM    ESRESTH+4,X'08'     TEST VALID NUMERIC                           
         BO    *+12                YES                                          
         MVI   ERROR,INVEST                                                     
         B     TRAPERR                                                          
*                                                                               
         ZIC   R1,ESRESTH+5                                                     
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         PACK  DUB,ESREST(0)                                                    
         CVB   R1,DUB                                                           
*                                                                               
         CH    R1,=H'1'            EST MUST BE BETWEEN 1 AND 255                
         BNL   *+12                                                             
         MVI   ERROR,INVEST                                                     
         B     TRAPERR                                                          
         CH    R1,=H'255'                                                       
         BNH   *+12                                                             
         MVI   ERROR,INVEST                                                     
         B     TRAPERR                                                          
*                                                                               
         STC   R1,BEST             BINARY EST CODE                              
         XC    KEY,KEY             BUILD ESTIMATE HEADER KEY                    
         MVC   KEY+1(1),BAGYMD                                                  
         MVC   KEY+2(2),BCLT                                                    
         MVC   KEY+4(3),QPRD                                                    
         MVC   KEY+7(1),BEST                                                    
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 READ                                                             
         MVC   ESRKEST,BEST        IF WE COME BACK, WE'VE FOUND IT              
*                                                                               
         MVC   KEY,SVKEY           GENCON NEEDS KEY                             
*                                                                               
VKX      B     XIT                                                              
         DROP  R6                                                               
         EJECT                                                                  
* VALIDATE RECORD                                                               
*                                                                               
VR       MVI   ELCODE,ESREL5Q      GET RID OF SPLIT ELEMENTS                    
         GOTO1 REMELEM                                                          
         LA    R2,ESRDATEH         A(FIRST DATE FIELD)                          
         CLI   5(R2),0             TEST ANY DATA                                
         BNE   VR20                YES                                          
         MVI   ERROR,MISSING                                                    
         B     TRAPERR                                                          
*                                                                               
VR10     CLI   5(R2),0             TEST ANY MORE DATA                           
         BE    DR                  NO                                           
*                                                                               
VR20     XC    ELEM,ELEM                                                        
         USING ESREL5,R6                                                        
         LA    R6,ELEM                                                          
         MVI   ESREL5,ESREL5Q      ELEMENT CODE                                 
         MVI   ESREL5LN,ESREL5LQ   ELEMENT LENGTH                               
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
         MVC   ESR5DATE,WORK+6     PUT EFFECTIVE DATE IN ELEMENT                
*                                                                               
         ZIC   R0,0(R2)            BUMP TO PRODUCTS FIELD                       
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
         CLI   DMCB+4,2            TEST NOT ENOUGH PRODUCTS                     
         BL    PRDTOTAL            YES                                          
         CLI   DMCB+4,5            TEST TOO MANY PRODUCTS                       
         BH    PRDTOTAL            YES                                          
*                                                                               
         LA    R6,ESR5DATA                                                      
         MVI   OPTNUM,1            FOR ERRORS                                   
         LA    R3,BLOCK            A(SCANNER BLOCK)                             
         XC    PERCENT,PERCENT     CLEAR PERCENTAGE TALLY                       
*                                                                               
VR30     CLI   0(R3),0             TEST END OF BLOCK                            
         BE    VR40                YES                                          
         CLI   0(R3),2             PRODUCT CODE IS 2 OR 3 CHARS                 
         BL    INVPRDC                                                          
         CLI   0(R3),3                                                          
         BH    INVPRDC                                                          
         CLI   1(R3),0             TEST ANY PERCENTAGE                          
         BE    INVPCT              NO                                           
*                                                                               
         XC    KEY,KEY             BUILD PRODUCT HEADER KEY                     
         MVC   KEY+1(1),BAGYMD                                                  
         MVC   KEY+2(2),BCLT                                                    
         MVC   KEY+4(3),12(R3)                                                  
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE     TEST PRODUCT FOUND                           
         BNE   INVPRDC             NO                                           
         MVC   0(3,R6),12(R3)      PUT PRODUCT CODE IN ELEMENT                  
*                                                                               
         ZIC   R5,1(R3)            L'PCT FIELD                                  
         GOTO1 CASHVAL,DMCB,(3,22(R3)),(R5)                                     
         CLI   DMCB,0              TEST VALID PERCENTAGE                        
         BNE   INVPCT                                                           
         MVC   3(4,R6),DMCB+4      PUT PERCENTAGE IN ELEMENT                    
         L     R1,PERCENT          INCREMENT PERCENTAGE TOTAL                   
         A     R1,DMCB+4                                                        
         ST    R1,PERCENT                                                       
*                                                                               
         LA    R6,7(R6)            BUMP TO NEXT ELEMENT FIELD                   
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
DR       LA    R2,ESRDATEH         A(FIRST DATE FIELD)                          
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
         LA    R2,ESRDATEH         A(FIRST DATE FIELD)                          
         L     R6,AIO                                                           
         MVI   ELCODE,ESREL5Q      SPLIT ELEMENT                                
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         USING ESREL5,R6                                                        
DR30     GOTO1 DATCON,DMCB,(3,ESR5DATE),(6,8(R2))                               
         OI    6(R2),X'80'         XMIT DATE                                    
*                                                                               
         ZIC   R0,0(R2)                                                         
         AR    R2,R0               BUMP TO PRODUCTS FIELD                       
         OI    6(R2),X'80'         XMIT PRODUCTS                                
         LA    R3,ESR5DATA                                                      
         LA    R4,8(R2)            R4 = DISPLAY POSITION                        
         LA    R5,5                MAX NUMBER OF PRODUCT CODES                  
*                                                                               
DR40     MVC   0(3,R4),0(R3)       PRODUCT CODE                                 
         LA    R4,2(R4)            AT LEAST 2 CHARS USED                        
         CLI   0(R4),C' '          TEST 2 OR 3 CHARS USED                       
         BNH   *+8                 ONLY 2                                       
         LA    R4,1(R4)                                                         
*                                                                               
         MVI   0(R4),C'='                                                       
         LA    R4,1(R4)                                                         
         EDIT  (B4,3(R3)),(6,(R4)),3,ALIGN=LEFT                                 
         AR    R4,R0               BUMP PAST EDITED PERCENTAGE                  
         MVI   0(R4),C','                                                       
         LA    R4,1(R4)                                                         
*                                                                               
         LA    R3,7(R3)            BUMP TO NEXT PRODUCT CODE                    
         OC    0(3,R3),0(R3)       TEST ANY MORE PRODUCT CODES                  
         BZ    *+8                 NO                                           
         BCT   R5,DR40             MAX OF 5 PRODUCT CODES                       
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
         USING ESRRECD,R6                                                       
*                                                                               
         MVC   ESRMED,QMED                                                      
         OI    ESRMEDH+6,X'80'     MEDIA                                        
         GOTO1 CLUNPK,DMCB,ESRKCLT,ESRCLT                                       
         OI    ESRCLTH+6,X'80'     CLIENT                                       
         MVC   ESRPRD,ESRKPRD                                                   
         OI    ESRPRDH+6,X'80'     PRODUCT                                      
         EDIT  ESRKEST,(3,ESREST),ALIGN=LEFT                                    
         OI    ESRESTH+6,X'80'     ESTIMATE                                     
*                                                                               
         B     XIT                                                              
         DROP  R6                                                               
         EJECT                                                                  
* LIST RECORDS                                                                  
*                                                                               
LR       LA    R4,KEY                                                           
         USING ESRKEY,R4                                                        
*                                                                               
         OC    KEY(13),KEY         TEST FIRST TIME THROUGH                      
         BNZ   LR10                                                             
         MVC   ESRKTYPE,=X'0D53'   RECORD TYPE                                  
         MVC   ESRKAM,BAGYMD       AGY/MED                                      
         MVC   ESRKCLT,BCLT        CLIENT CODE                                  
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
         XC    LISTAR,LISTAR       FILL IN LIST LINE                            
         GOTO1 CLUNPK,DMCB,ESRKCLT,LSTCLT  CLIENT                               
         MVC   LSTPRD,ESRKPRD      PRODUCT                                      
         EDIT  ESRKEST,(3,LSTEST)  ESTIMATE                                     
*                                                                               
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
         L     R6,AIO                                                           
         MVI   ELCODE,ESREL5Q      ESTIMATE ELEMENT                             
         BAS   RE,GETEL                                                         
         BE    *+6                 MUST BE AT LEAST ONE                         
         DC    H'0'                                                             
*                                                                               
         USING ESREL5,R6                                                        
         MVC   HALF,ESR5DATE       SAVE THE LAST DATE FOUND                     
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
         GETEL R6,DATADISP,ELCODE                                               
*                                                                               
TRAPERR  GOTO1 ERREX                                                            
*                                                                               
INVPRDC  OI    6(R2),X'40'         SET CURSOR                                   
         LA    R2,CONHEADH                                                      
         XC    CONHEAD,CONHEAD                                                  
         MVC   8(43,R2),=C'* ERROR * INVALID PRODUCT CODE IN FIELD   *'         
         EDIT  OPTNUM,(1,48(R2))                                                
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
PRDTOTAL XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'PRDTOTM),PRDTOTM                                       
         GOTO1 ERREX2                                                           
PRDTOTM  DC    C'* ERROR * MUST SPECIFY BETWEEN 2 AND 5 PRODUCTS *'             
         SPACE 3                                                                
         LTORG                                                                  
         PRINT OFF                                                              
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE SPSFMFFD                                                       
         PRINT ON                                                               
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE SPSFMDED                                                       
         PRINT OFF                                                              
       ++INCLUDE SPSFMWORKD                                                     
         PRINT ON                                                               
         SPACE 5                                                                
* WORK AREA                                                                     
*                                                                               
         ORG   SYSSPARE                                                         
PERCENT  DS    F                   SUM OF PERCENTAGES                           
OPTNUM   DS    X                   SCANNER ENTRY NUMBER                         
SAVEKEY  DS    XL48                                                             
         EJECT                                                                  
ESRRECD  DSECT                                                                  
       ++INCLUDE SPGENESR                                                       
         SPACE 5                                                                
GEND     DSECT                                                                  
         ORG   LISTAR                                                           
         DS    CL3                                                              
LSTCLT   DS    CL3                                                              
         DS    CL2                                                              
LSTPRD   DS    CL3                                                              
         DS    CL2                                                              
LSTEST   DS    CL3                                                              
         DS    CL6                                                              
LSTDATE  DS    CL6                                                              
         SPACE 3                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'006SPSFM1D   05/01/02'                                      
         END                                                                    
