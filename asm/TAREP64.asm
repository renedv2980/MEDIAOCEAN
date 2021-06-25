*          DATA SET TAREP64    AT LEVEL 001 AS OF 04/03/15                      
*PHASE T70364B,*                                                                
*INCLUDE TAPOST                                                                 
*INCLUDE DLFLD                                                                  
         TITLE 'T70364 - POSTINGS REPORT'                                       
T70364   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T70364                                                         
         L     RC,0(R1)            RC=A(CONTROLLER W/S)                         
         USING GEND,RC                                                          
         L     RA,ATWA             RA=A(SCREEN)                                 
         USING T703FFD,RA                                                       
         L     R9,ASUBSYSD         R9=A(SYSTEM W/S)                             
         USING SUBSYSD,R9                                                       
         L     R8,ASPOOLD          R8=A(SPOOL DSECT)                            
         USING SPOOLD,R8                                                        
         LA    R7,BUFF                                                          
         LA    R7,8(R7)                                                         
         USING MYD,R7              R7=A(LOCAL W/S)                              
         EJECT                                                                  
***********************************************************************         
*        MODE CONTROLLED ROUTINES                                     *         
***********************************************************************         
                                                                                
         GOTO1 INITIAL,DMCB,0      INITIALIZE                                   
                                                                                
         CLI   MODE,VALKEY                                                      
         JNE   *+8                                                              
         BRAS  RE,VK               PRINT REPORT                                 
                                                                                
         CLI   MODE,PRINTREP                                                    
         JNE   *+8                                                              
         BRAS  RE,PREP             PRINT REPORT                                 
                                                                                
YES      XR    RC,RC                                                            
NO       LTR   RC,RC                                                            
XIT      XIT1                                                                   
                                                                                
         GETEL R4,DATADISP,ELCODE                                               
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        VALIDATE KEY                                                 *         
***********************************************************************         
                                                                                
VK       NTR1  BASE=*,LABEL=*                                                   
         USING TPD,R3                                                           
         LA    R3,TPBLOCK                                                       
         XC    TPFLTS(TPFLTLNQ),TPFLTS                                          
                                                                                
         LA    R2,POSTYPEH         TYPE                                         
         GOTO1 ANY                                                              
         CLI   WORK,TPTYPEB          BILLING                                    
         JE    VK10                                                             
         CLI   WORK,TPTYPEC          CHECKS                                     
         JNE   FLDINV                                                           
VK10     MVC   TPTYPE,WORK                                                      
                                                                                
         LA    R2,POSPERH          PERIOD                                       
         GOTO1 ANY                                                              
         GOTO1 VALPERD                                                          
         MVC   TPSDATE,TIQPSTR                                                  
         MVC   TPEDATE,TIQPEND                                                  
                                                                                
         BAS   RE,VALOFF           OFFICE                                       
                                                                                
         LA    R2,POSCURRH         CURRENCY REQUIRED                            
         CLI   5(R2),0             FOR TYPE CHECKS                              
         JNE   VK20                                                             
         CLI   TPTYPE,TPTYPEC                                                   
         JE    FLDMIS                                                           
         J     VK40                                                             
                                                                                
VK20     LA    R2,POSCURRH         CURRENCY                                     
         CLI   8(R2),C'U'            US$                                        
         JE    VK30                                                             
         CLI   8(R2),C'C'            CAN$                                       
         JE    VK30                                                             
         CLI   8(R2),C'E'            EURO                                       
         JNE   FLDINV                                                           
VK30     MVC   TPCURR,8(R2)                                                     
                                                                                
VK40     MVI   TPOPTS1,TPODOWN+TPOKEEP                                          
         CLI   POSOPTH+5,0         DEFAULT TO DOWNLOAD FORMAT                   
         JE    XIT                 AND PUT WORKER FILE ON KEEP                  
                                                                                
         LA    R2,POSOPTH                                                       
         GOTO1 SCANNER,DMCB,(R2),(10,BLOCK),0                                   
         ZIC   R0,4(R1)                                                         
         LA    R4,BLOCK                                                         
         LTR   R0,R0                                                            
         JZ    FLDINV                                                           
                                                                                
VK50     CLC   =C'URG',12(R4)      URGENT CHECK RUN                             
         JNE   VK60                                                             
         CLI   TPTYPE,TPTYPEC                                                   
         JNE   FLDINV                                                           
         OI    TPOPTS1,TPOURG                                                   
         J     VK900                                                            
                                                                                
VK60     CLC   =C'WRKR',12(R4)     GENERATE WORKER FILE                         
         JNE   VK70                                                             
         NI    TPOPTS1,X'FF'-TPODOWN                                            
         J     VK900                                                            
                                                                                
VK70     CLC   =C'PRINT',12(R4)    PRINT                                        
         JNE   VK75                                                             
         OI    TPOPTS1,TPOPRNT                                                  
         J     VK900                                                            
                                                                                
VK75     CLC   =C'PLUS',12(R4)     PAYROLL PLUS                                 
         JNE   VK80                                                             
         OI    TPOPTS1,TPOPLUS                                                  
         J     VK900                                                            
                                                                                
VK80     CLC   =C'NOKEEP',12(R4)   DO NOT PUT WORKER FILE ON KEEP               
         JNE   VK90                                                             
         NI    TPOPTS1,X'FF'-TPOKEEP                                            
         J     VK900                                                            
                                                                                
VK90     CLC   =C'EMAIL',12(R4)    EMAIL WHEN OUT OF BALANCE                    
         JNE   VK100                                                            
         OI    TPOPTS1,TPOEMAIL                                                 
         J     VK900                                                            
                                                                                
VK100    CLC   =C'AGY',12(R4)      FILTER AGENCY                                
         JNE   VK110                                                            
         MVC   TPFAGY,22(R4)                                                    
         J     VK900                                                            
                                                                                
VK110    CLC   =C'INV',12(R4)      FILTER INVOICE                               
         JNE   FLDINV                                                           
         GOTO1 TINVCON,DMCB,22(R4),TGINV,DATCON  CVT TO INTERNAL FORMAT         
         CLI   0(R1),X'FF'                                                      
         BE    FLDINV                                                           
         MVC   TPFINV,TGINV                                                     
                                                                                
VK900    LA    R4,32(R4)           BUMP TO THE NEXT OPTION                      
         BCT   R0,VK50                                                          
         J     XIT                                                              
         DROP  R3                                                               
                                                                                
FLDINV   MVI   ERROR,INVALID                                                    
         GOTO1 ERREX                                                            
                                                                                
FLDMIS   MVI   ERROR,MISSING                                                    
         GOTO1 ERREX                                                            
                                                                                
***********************************************************************         
*        VALIDATE OFFICE NUMBERS                                      *         
***********************************************************************         
                                                                                
         USING TPD,R3                                                           
VALOFF   NTR1                                                                   
         CLI   POSOFFH+5,0                                                      
         JE    XIT                                                              
                                                                                
         XC    HALF,HALF           CLEAR FIELD DISP. COUNTER                    
                                                                                
         LA    R2,POSOFFH          OFFICE LIST                                  
         GOTO1 SCANNER,DMCB,(R2),(X'08',BLOCK)                                  
         CLI   4(R1),0                                                          
         JE    FLDINV                                                           
                                                                                
         USING SCAND,R4                                                         
         LA    R4,BLOCK            R4=A(SCAN BLOCK)                             
         ZIC   R0,4(R1)            R0=N'SCAN BLOCK ENTRIES                      
                                                                                
         LA    R3,TPOFFLST         LIST OF OFFICES                              
         DROP  R3                                                               
                                                                                
VO10     CLI   SCLEN1,L'TGOFF      CHECK INPUT IS CORRECT LENGTH                
         JNE   FLDINV                                                           
         GOTO1 RECVAL,DMCB,TLOFCDQ,(X'80',SCDATA1)  VALIDATE OFFICE             
         JNE   FLDINV                                                           
         MVC   0(L'TGOFF,R3),TGOFF                                              
         LA    R3,L'TGOFF(R3)      BUMP TO NEXT POSITION                        
                                                                                
         ZIC   RF,SCLEN1           L'LHS                                        
         ZIC   RE,SCLEN2           + L'RHS                                      
         LTR   RE,RE                                                            
         JZ    *+8                                                              
         AHI   RE,1                + '=' SIGN IF THERE IS A RIGHT HALF          
         LA    RF,1(RF,RE)         + DELIMITER                                  
         AH    RF,HALF             + L'SO FAR                                   
         STH   RF,HALF             = CURRENT DISPLACEMENT INTO FIELD            
                                                                                
         LA    R4,SCANNEXT         BUMP TO IT                                   
         BCT   R0,VO10             AND CONTINUE                                 
         J     XIT                                                              
         DROP  R4                                                               
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        PRINT REPORT                                                 *         
***********************************************************************         
                                                                                
PREP     NTR1  BASE=*,LABEL=*                                                   
         XC    WORK(8),WORK                                                     
         MVC   WORK+8(2),TWAORIG                                                
         GOTO1 USERVAL,DMCB,(X'80',WORK)                                        
                                                                                
         USING TPD,R2                                                           
         LA    R2,TPBLOCK                                                       
         ST    RC,TPRC                                                          
         MVC   TPINVCON,TINVCON                                                 
         MVC   TPRCVAL,RECVAL                                                   
         MVC   TPDLFLD,=V(DLFLD)                                                
         MVC   TPHEX,TGACCHX                                                    
         MVC   TPUSER,TGUSERID                                                  
         GOTO1 =V(TAPOST),DMCB,(R2)                                             
         J     XIT                                                              
         DROP  R2                                                               
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        WORKING STORAGE                                                        
***********************************************************************         
                                                                                
MYD      DSECT                                                                  
TPBLOCK  DS    XL(TPINVLNQ)                                                     
MYDLNQ   EQU   *-MYD                                                            
         EJECT                                                                  
       ++INCLUDE TAREPFFD                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE TAREPF6D                                                       
         EJECT                                                                  
*DDGENTWA  (MUST FOLLOW LAST SCREEN)                                            
*DDSPOOLD                                                                       
*DDSPLWORKD                                                                     
*DDBIGBOX                                                                       
*TAGENFILE                                                                      
*TAPOSTD                                                                        
*DDPERVALD                                                                      
*TASYSDSECT                                                                     
*TASYSEQUS                                                                      
*DDDLCB                                                                         
*DDTWADCONS                                                                     
*TAREPWORKD                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDGENTWA                                                       
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE DDBIGBOX                                                       
       ++INCLUDE TAGENFILE                                                      
       ++INCLUDE TAPOSTD                                                        
       ++INCLUDE DDPERVALD                                                      
       ++INCLUDE TASYSDSECT                                                     
       ++INCLUDE TASYSEQUS                                                      
       ++INCLUDE DDDLCB                                                         
TWADCOND DSECT                                                                  
       ++INCLUDE DDTWADCONS                                                     
       ++INCLUDE TAREPWORKD                                                     
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'001TAREP64   04/03/15'                                      
         END                                                                    
