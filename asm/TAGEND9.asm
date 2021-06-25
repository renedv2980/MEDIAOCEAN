*          DATA SET TAGEND9    AT LEVEL 009 AS OF 11/06/03                      
*PHASE T702D9A,*                                                                
*INCLUDE TAINTER                                                                
         TITLE 'T702D9 - PRODUCTION INTERFACE TEST'                             
T702D9   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T702D9,RR=R3                                                   
         L     RC,0(R1)            RC=GENCON STORAGE AREA                       
         USING GEND,RC                                                          
         L     RA,ATWA             RA=A(TWA)                                    
         USING T702FFD,RA                                                       
         L     R9,ASYSD            R9=ROOT STORAGE AREA                         
         USING SYSD,R9                                                          
         L     R8,ASPOOLD          R8=SPOOL DSECT                               
         USING SPOOLD,R8                                                        
         ST    R3,RELO                                                          
         EJECT                                                                  
*              MODE CONTROLLED ROUTINES                                         
                                                                                
         GOTO1 INITIAL,DMCB,PFTAB                                               
                                                                                
         CLI   MODE,VALKEY         VALIDATE KEY                                 
         BNE   *+12                                                             
         BAS   RE,VKEY                                                          
         B     XIT                                                              
                                                                                
         CLI   MODE,VALREC         ACTION MODE                                  
         BNE   XIT                                                              
         BAS   RE,VREC                                                          
         B     XIT                                                              
         EJECT                                                                  
*              VALIDATE THE KEY                                                 
                                                                                
VKEY     NTR1                                                                   
         LA    R2,SIFAGYH                                                       
         GOTO1 RECVAL,DMCB,TLAYCDQ,(X'08',(R2)),SIFAGYNH                        
                                                                                
         XC    TGPCLI,TGPCLI       CLEAR GLOBAL PROD CLIENT                     
         CLI   SIFCLIH+5,0         IF ANY INPUT - USE IT                        
         BE    *+16                                                             
         MVC   TGPCLI(L'SIFCLI),SIFCLI   SET GLOBAL PRODUCTION CLIENT           
         OC    TGPCLI,SPACES                                                    
                                                                                
         GOTO1 RECVAL,DMCB,TLIFCDQ,(X'20',0)  READ INTERFACE RECORD             
         BNE   ERRXIT                                                           
         L     R4,AIO                                                           
         MVI   ELCODE,TAIFELQ      GET A(INTERFACE DETAILS ELEMENT)             
         BAS   RE,GETEL                                                         
         BNE   ERRINV                                                           
         ST    R4,AIFEL            SAVE FOR TAINTER                             
         B     XIT                                                              
         EJECT                                                                  
*              VALIDATE INPUT                                                   
                                                                                
VREC     NTR1                                                                   
         MVI   TGMEEQU,0           PRE-CLEAR GLOBALS                            
         MVI   TGUSEQU,0                                                        
         MVI   TGCAEQU,0                                                        
         MVI   TGONOF,0                                                         
                                                                                
         LA    R3,SIFFRSTH         SET TO LOOP THROUGH SCREEN LINES             
         USING LINED,R3                                                         
         LA    R0,(SIFPFKH-SIFFRSTH)/LINLNQ                                     
                                                                                
VR10     XC    LINWCS,LINWCS       CLEAR WORKCODES                              
         OI    LINWCSH+6,X'80'                                                  
         GOTO1 FLDVAL,DMCB,(X'80',LINMEDH),LINCAMH  TEST FOR INPUT              
         BE    VR100                                                            
                                                                                
         LA    R2,LINMEDH          VALIDATE MEDIA                               
         CLI   5(R2),0                                                          
         BNE   VR20                                                             
         CLI   TGMEEQU,0                                                        
         BE    ERRMISS                                                          
         MVC   8(L'LINMED,R2),TGMENAME                                          
         OI    6(R2),X'80'                                                      
         B     VR30                                                             
VR20     GOTO1 MEDVAL,DMCB,8(R2)                                                
         BNE   ERRINV                                                           
                                                                                
VR30     LA    R2,LINUSEH          VALIDATE USE                                 
         CLI   5(R2),0                                                          
         BNE   VR40                                                             
         CLI   TGUSEQU,0                                                        
         BE    ERRMISS                                                          
         MVC   8(L'LINUSE,R2),TGUSCDE                                           
         OI    6(R2),X'80'                                                      
         B     VR50                                                             
VR40     GOTO1 USEVAL,DMCB,(X'40',8(R2))                                        
         BNE   ERRINV                                                           
         MVC   BYTE,TGUSMEDS       INSURE VALID FOR MEDIA                       
         NC    BYTE,TGMEEQU                                                     
         BZ    ERRUSEM                                                          
                                                                                
VR50     LA    R2,LINCATH          VALIDATE CATEGORY                            
         CLI   5(R2),0                                                          
         BNE   VR60                                                             
         CLI   TGCAEQU,0                                                        
         BE    ERRMISS                                                          
         MVC   8(L'LINCAT,R2),TGCACDE                                           
         OI    6(R2),X'80'                                                      
         B     VR70                                                             
VR60     OC    8(L'LINCAT,R2),SPACES                                            
         GOTO1 CATVAL,DMCB,8(R2)                                                
         BNE   ERRINV                                                           
                                                                                
         MVI   BYTE,AFT            DEFAULT TO UNION AFT                         
         GOTO1 UNITEST,DMCB,TGCAUNIS,AFM,0,0,0                                  
         BZ    *+8                                                              
         MVI   BYTE,AFM            UNLESS AFM IS ALLOWABLE UNION                
         GOTO1 UNIVAL,DMCB,(X'80',BYTE)                                         
                                                                                
VR70     LA    R2,LINCAMH          VALIDATE ON/OFF CAMERA                       
         TM    TGMEEQU,RADIO                                                    
         BZ    *+14                                                             
         MVC   TGONOF,=C'OFF'      RADIO IS ALWAYS OFF CAMERA                   
         B     VR75                                                             
         CLI   5(R2),0                                                          
         BNE   VR80                                                             
         CLI   TGONOF,0                                                         
         BE    ERRMISS                                                          
VR75     MVC   8(L'LINCAM,R2),TGONOF                                            
         OI    6(R2),X'80'                                                      
         B     VR90                                                             
VR80     OC    8(L'LINCAM,R2),SPACES                                            
         CLC   =C'ON ',8(R2)                                                    
         BE    *+14                                                             
         CLC   =C'OFF',8(R2)                                                    
         BNE   ERRINV                                                           
         MVC   TGONOF,8(R2)                                                     
                                                                                
VR90     LA    R2,LINCLBH          VALIDATE CELEBRITY                           
         CLI   5(R2),0             IF NO INPUT                                  
         BNE   VR95                                                             
         MVI   8(R2),C'N'          ASSUME NOT CELEBRITY                         
         OI    6(R2),X'80'                                                      
VR95     CLI   8(R2),C'Y'          ELSE, VALIDATE YES OR NO                     
         BE    *+12                                                             
         CLI   8(R2),C'N'                                                       
         BNE   ERRINV                                                           
         MVC   CELEB,8(R2)         SET CELEBRITY FLAG                           
*                                                                               
         BAS   RE,GETWCS           RESOLVE / DISPLAY WORKCODES                  
                                                                                
VR100    LA    R3,LINNEXT          BUMP TO NEXT LINE                            
         BCT   R0,VR10                                                          
         B     XIT                                                              
         EJECT                                                                  
*              PASS CONTROL TO TAINTER TO RESOLVE WORKCODES                     
                                                                                
         USING LINED,R3            R3 = A(SCREEN LINE)                          
GETWCS   NTR1                                                                   
         LA    R4,BLOCK            R4=A(TAINTER PARAMETER BLOCK)                
         USING TND,R4                                                           
         XC    0(TNLNQ,R4),0(R4)                                                
         ST    RC,TNRC             A(GEND)                                      
         LA    R1,SYSCOMM          A(SYSTEM COMMON ROUTINES)                    
         ST    R1,TNASYCOM                                                      
         LA    R1,WORK             A(WORK-CODE TABLE)                           
         XC    WORK,WORK                                                        
         ST    R1,TNAWCTAB                                                      
         MVC   TNAIFEL,AIFEL       A(INTERFACE ELEMENT)                         
         CLI   CELEB,C'Y'          IF CELEBRITY                                 
         BNE   *+8                                                              
         OI    TNCASTA2,TACASCLB   TURN ON INDICATOR                            
                                                                                
         GOTO1 =V(TAINTER),DMCB,TND,RR=RELO  RESOLVE WORKCODES                  
                                                                                
         MVC   LINFEES,TNFEEWC     DISPLAY WORKCODES                            
         MVC   LINPNH,TNPNHWC                                                   
         MVC   LINHNW,TNHNWWC                                                   
         TM    TNSTAT,TNHCOMB      IF TAX AND HANDLING WAS COMBINED             
         BZ    GETWCS5                                                          
         MVC   LINTAX,TNTNHWC      SHOW THE WORK CODE IN TAX                    
         MVC   LINHND,TNTNHWC      AND IN HANDLING                              
         B     GETWCS8                                                          
*                                                                               
GETWCS5  MVC   LINTAX,TNTAXWC      ELSE SHOW TAX IN TAX COLUMN                  
         MVC   LINHND,TNHANDWC     AND HANDLING IN HANDLING COLUMN              
*                                                                               
GETWCS8  MVC   LINCOMM,TNCOMMWC                                                 
         B     XIT                                                              
         EJECT                                                                  
*              ERRORS, EXITS                                                    
                                                                                
ERRMISS  MVI   ERROR,MISSING                                                    
         B     ERRXIT                                                           
                                                                                
ERRINV   MVI   ERROR,INVALID                                                    
         B     ERRXIT                                                           
                                                                                
ERRUSEM  MVI   ERROR,ERUSEMED      INVALID USE FOR MEDIA                        
         B     ERRXIT                                                           
                                                                                
ERRXIT   GOTO1 EXIT,DMCB,0                                                      
XIT      XIT1                                                                   
                                                                                
         GETEL R4,DATADISP,ELCODE                                               
         EJECT                                                                  
*              CONSTANTS, ETC.                                                  
         SPACE 2                                                                
PFTAB    DS    0H                                                               
         DC    AL1(PF13X-*,13,0,0,0)                                            
         DC    CL3'   ',CL8'        ',CL8'DISPLAY'                              
PF13X    EQU   *                                                                
         DC    X'FF'                                                            
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
*              DSECT TO COVER SCREEN LINE                                       
                                                                                
LINED    DSECT                                                                  
LINMEDH  DS    CL8                                                              
LINMED   DS    CL1                 MEDIA                                        
LINUSEH  DS    CL8                                                              
LINUSE   DS    CL3                 USE CODE                                     
LINCATH  DS    CL8                                                              
LINCAT   DS    CL3                 CATEGORY                                     
LINCAMH  DS    CL8                                                              
LINCAM   DS    CL3                 ON/OFF CAMERA                                
LINCLBH  DS    CL8                                                              
LINCLB   DS    CL1                 CELEBRITY                                    
LINWCSH  DS    CL8                                                              
LINWCS   DS    CL23                WORKCODES                                    
         ORG   LINWCS                                                           
LINFEES  DS    CL2                 FEES                                         
         DS    CL3                                                              
LINPNH   DS    CL2                 P&H                                          
         DS    CL3                                                              
LINHNW   DS    CL2                 H&W                                          
         DS    CL3                                                              
LINTAX   DS    CL2                 TAX                                          
         DS    CL3                                                              
LINHND   DS    CL2                 HANDLING                                     
         DS    CL4                                                              
LINCOMM  DS    CL2                 COMMISSION                                   
LINLNQ   EQU   *-LINED                                                          
LINNEXT  EQU   *                                                                
         EJECT                                                                  
       ++INCLUDE TAINTERD                                                       
         EJECT                                                                  
       ++INCLUDE TAGENFFD                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE TASCRD9D                                                       
                                                                                
RELO     DS    F                   TEMP STORAGE                                 
AIFEL    DS    A                   A(INTERFACE DETAILS ELEMENT)                 
CELEB    DS    CL1                 CELEBRITY FLAG                               
         EJECT                                                                  
* TAGENWORKD                                                                    
* TASYSDSECT                                                                    
* TASYSEQUS                                                                     
* TAGENFILE                                                                     
* DDSPOOLD                                                                      
* DDSPLWORKD                                                                    
         PRINT OFF                                                              
       ++INCLUDE TAGENWORKD                                                     
       ++INCLUDE TASYSDSECT                                                     
       ++INCLUDE TASYSEQUS                                                      
       ++INCLUDE TAGENFILE                                                      
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'009TAGEND9   11/06/03'                                      
         END                                                                    
