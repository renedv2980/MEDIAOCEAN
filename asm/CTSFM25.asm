*          DATA SET CTSFM25    AT LEVEL 157 AS OF 11/12/18                      
*PHASE TA0A25A                                                                  
*INCLUDE DLFLD                                                                  
***********************************************************************         
*                                                                     *         
*  TITLE: TA0A25 - DARE CATEGORY RECORDS MAINT/LIST                   *         
*                                                                     *         
*  COMMENTS: MAINTAINS DARE STATION RECORDS                           *         
*                                                                     *         
*  CALLED FROM: SFM CONTROLLER (TA0A00), WHICH CALLS                  *         
*               DDGENCON (T00A30) WHICH CALLS THIS.                   *         
*                                                                     *         
*  CALLS TO:    DATAMGR                                               *         
*                                                                     *         
*  INPUTS: SCREENS CTSFM8E (TA0A8E) -- MAINTENANCE                    *         
*                  CTSFM8F (TA0A8F) -- LIST                           *         
*                                                                     *         
*  OUTPUTS: UPDATED DARE STATION RECORDS                              *         
*                                                                     *         
*  LOCALS: REGISTER USAGE                                             *         
*          R0 - WORK                                                  *         
*          R1 - WORK                                                  *         
*          R2 - POINTER TO SCREEN FLDH, ERREX WILL POSITION CURSOR    *         
*          R3 - GETEL REGISTER                                        *         
*          R4 - WORK                                                  *         
*          R5 - WORK                                                  *         
*          R6 - WORK                                                  *         
*          R7 - 2ND BASE                                              *         
*          R8 - SPOOLD                                                *         
*          R9 - SYSD                                                  *         
*          RA - TWA                                                   *         
*          RB - FIRST BASE                                            *         
*          RC - GEND                                                  *         
*          RD - SYSTEM                                                *         
*          RE - SYSTEM                                                *         
*          RF - SYSTEM                                                *         
*                                                                     *         
***********************************************************************         
         EJECT                                                                  
         TITLE 'TA0A25 DARE CATEGORY RECORDS'                                   
TA0A25   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,*TA0A25*,R7,RR=R3                                              
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA                                                   
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         ST    R3,RELO                                                          
*                                                                               
         CLI   MODE,VALKEY         VALIDATE KEY                                 
         BE    VK                                                               
         CLI   MODE,VALREC         VALIDATE RECORD                              
         BE    VR                                                               
         CLI   MODE,DISPREC        DISPLAY RECORD                               
         BE    DR                                                               
         CLI   MODE,DISPKEY        DISPLAY KEY                                  
         BE    DK                                                               
         CLI   MODE,LISTRECS       ONLINE LIST RECORDS                          
         BE    LR                                                               
*                                                                               
YES      SR    RC,RC                                                            
NO       LTR   RC,RC                                                            
EXIT     XIT1                                                                   
*                                                                               
ERINVF   MVC   GERROR,=AL2(INVALID)                                             
         B     NO                                                               
ERMISS   MVC   GERROR,=AL2(MISSING)                                             
         B     NO                                                               
                                                                                
***********************************************************************         
* VALIDATE KEY                                                                  
***********************************************************************         
VK       XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
*                                                                               
         CLI   ACTNUM,ACTLIST      ACTION LIST?                                 
         BNE   VK000                                                            
         XC    CURLIC,CURLIC       LICENSE CODE                                 
         XC    CURSEQ,CURSEQ       LICENSE SEQUENCE CODE                        
         XC    CUROXC,CUROXC       OX DEMO CODE                                 
         B     VK030                                                            
*                                                                               
         USING CSLRECD,R4                                                       
VK000    MVI   CSLKMIN,LDCKMIQ     MINOR SYSTEM (DEMO C'D')                     
         MVI   CSLKREC,CSLKREQ     RECORD TYPE  (LICENSE C'L')                  
*                                                                               
         CLI   SFMLICCH+5,0                                                     
         BE    ERMISS                                                           
         MVC   CSLKLIC,SFMLICC     LICENSE NUMBER                               
*                                                                               
         GOTO1 HIGH                READ FOR LICENSE                             
         CLC   CSLKEY,KEYSAVE                                                   
         BNE   ERINVF                                                           
*                                                                               
         GOTO1 GETREC                                                           
         L     R4,AIO                                                           
         MVI   ELCODE,CSFLELQ      X'0F' GET FULL LICENSE ELEMENT               
         BAS   RE,GETEL                                                         
         BNE   ERINVF                                                           
*                                                                               
         USING CSFLD,R3                                                         
VK010    CLC   SFMLICC,CSFLIC      IS THIS THE ONE                              
         BE    VK020                                                            
*                                                                               
         BAS   RE,NEXTEL           ANY MORE FULL LICENSE ELEMENTS?              
         BNE   ERINVF              NO                                           
         B     VK010               YES: SEE IF IT'S THE ONE WE WANT             
*                                                                               
VK020    MVC   CURLIC,CSFLIC       LICENSE CODE                                 
         MVC   CURSEQ,CSFLSEQ      SEQUENCE NUMBER                              
*                                                                               
         CLI   SFMOXDCH+5,0                                                     
         BE    ERMISS                                                           
         MVC   CUROXC,SFMOXDC      OX DEMO CODE                                 
*                                                                               
VK030    XC    KEY,KEY                                                          
         USING LDCRECD,R4                                                       
         MVI   LDCKMIN,LDCKMIQ     MINOR SYSTEM (DEMO C'D')                     
         MVI   LDCKREC,LDCKREQ     RECORD TYPE  (LICENSE C'L')                  
         MVC   LDCKLIC,CURLIC      LICENSE CODE                                 
         MVC   LDCKSEQ,CURSEQ      LICENSE SEQUENCE CODE                        
         MVC   LDCKOXC,CUROXC      OX DEMO CODE                                 
*                                                                               
         B     EXIT                                                             
         DROP  R3,R4                                                            
                                                                                
***********************************************************************         
* VALIDATE RECORD                                                               
***********************************************************************         
VR       B     EXIT                                                             
                                                                                
***********************************************************************         
* DISPLAY KEY                                                                   
***********************************************************************         
DK       L     R4,AIO                                                           
         USING LDCRECD,R4                                                       
*                                                                               
         LR    R3,R4                                                            
         USING CSFLD,R3                                                         
         XC    ELEM,ELEM                                                        
         MVI   ELEM,CSFLELQ                                                     
         MVC   SFMLICC,=CL32'MISSING FULL LICENSE ELEMENT'                      
         BAS   RE,GETEL                                                         
         BNE   *+10                                                             
         MVC   SFMLICC,CSFLIC                                                   
         OI    SFMLICCH+6,X'80'                                                 
         DROP  R3                                                               
*                                                                               
         MVC   SFMOXDC,LDCKOXC     OX DEMO CODE                                 
         OI    SFMOXDCH+6,X'80'                                                 
*                                                                               
         B     EXIT                                                             
         DROP  R4                                                               
                                                                                
***********************************************************************         
* DISPLAY RECORD                                                                
***********************************************************************         
         USING LDCRECD,R4                                                       
DR       LA    R4,KEY                                                           
         MVC   CUROXC,LDCKOXC      OX DEMO CODE                                 
*                                                                               
         MVC   SFMKEYL,LDCKLIC                                                  
         OI    SFMKEYLH+6,X'80'                                                 
         EDIT  LDCKSEQ,SFMKEYS,ZERO=NOBLANK,FILL=0                              
         OI    SFMKEYSH+6,X'80'                                                 
*                                                                               
         GOTO1 HEXOUT,DMCB,LDCDA,SFMDSK,L'LDCDA                                 
         OI    SFMDSKH+6,X'80'                                                  
*                                                                               
         L     R4,AIO                                                           
*                                                                               
         LR    R3,R4                                                            
         XC    ELEM,ELEM                                                        
         MVI   ELEM,GACTELQ                                                     
         BAS   RE,GETEL                                                         
         BNE   DRX                                                              
         USING GACTELD,R3                                                       
*        GOTO1 DATCON,DMCB,(3,GACTCDT),(21,SFMDATE)                             
*        OI    SFMDATEH+6,X'80'                                                 
*                                                                               
         MVC   SFMCNA1,=CL40'MISSING OX DEMO CATEGORY RECORD'                   
         OI    SFMCNA1H+6,X'80'                                                 
         MVC   SFMCNA2,SPACES                                                   
         OI    SFMCNA2H+6,X'80'                                                 
*                                                                               
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING CDCRECD,R4                                                       
         MVI   CDCKMIN,CDCKMIQ                                                  
         MVI   CDCKREC,CDCKREQ                                                  
         MVC   CDCKOXC,CUROXC                                                   
         GOTO1 READ                                                             
         BNE   DRX                                                              
                                                                                
         GOTO1 HEXOUT,DMCB,CDCDA,SFMDSK2,L'CDCDA                                
         OI    SFMDSK2H+6,X'80'                                                 
*                                                                               
         GOTO1 GETREC                                                           
         BNE   DRX                                                              
         L     R4,AIO                                                           
*                                                                               
         LR    R3,R4                                                            
         USING CDCELD,R3                                                        
         XC    ELEM,ELEM                                                        
         MVI   ELEM,CDCELQ                                                      
         MVC   SFMCNA1,=CL40'MISSING OX DEMO CATEGORY ELEMENT'                  
         BAS   RE,GETEL                                                         
         BNE   DRX                                                              
*                                                                               
         MVC   SFMLOC(3),=C'NO '                                                
         OI    SFMLOCH+6,X'80'                                                  
         TM    CDCFLAG,CDCFLOC                                                  
         BZ    *+10                                                             
         MVC   SFMLOC(3),=C'YES'                                                
*                                                                               
         MVC   SFMNAT(3),=C'NO '                                                
         OI    SFMNATH+6,X'80'                                                  
         TM    CDCFLAG,CDCFNAT                                                  
         BZ    *+10                                                             
         MVC   SFMNAT(3),=C'YES'                                                
*                                                                               
         EDIT  CDCNUM,SFMCNUM,ALIGN=LEFT,ZERO=NOBLANK                           
         OI    SFMCNUMH+6,X'80'                                                 
*                                                                               
         MVC   SFMCNA1,CDCNAME                                                  
         MVC   SFMCNA2,CDCNAME+L'SFMCNA1                                        
         DROP  R3                                                               
*                                                                               
DRX      B     EXIT                                                             
         DROP  R4                                                               
                                                                                
***********************************************************************         
* LIST RECORDS                                                                  
***********************************************************************         
         USING LDCRECD,R4                                                       
LR       LA    R4,KEY                                                           
         OC    KEY(LDCKEYL),KEY    FIRST TIME THROUGH?                          
         BNZ   LR010               NO                                           
         MVI   LDCKMIN,LDCKMIQ     MINOR SYSTEM (DEMO C'D')                     
         MVI   LDCKREC,LDCKREQ     RECORD TYPE  (LICENSE C'L')                  
*                                                                               
         LLC   R1,LILLICH+5        LICENSE FILTER                               
         AHI   R1,-1                                                            
         BM    LR010                                                            
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   LDCKLIC(0),LILLIC                                                
*                                                                               
LR010    GOTO1 HIGH                                                             
         B     LR030                                                            
LR020    LA    R4,KEY                                                           
         GOTO1 SEQ                                                              
LR030    CLI   LDCKMIN,LDCKMIQ     MINOR SYSTEM (DEMO C'D')                     
         BNE   LRX                                                              
         CLI   LDCKREC,LDCKREQ     RECORD TYPE  (LICENSE C'L')                  
         BNE   LRX                                                              
*                                                                               
         GOTO1 GETREC                                                           
         L     R4,AIO                                                           
         LR    R3,R4                                                            
         USING CSFLD,R3                                                         
         XC    ELEM,ELEM                                                        
         MVI   ELEM,CSFLELQ                                                     
         MVC   LSTLIC,=CL32'MISSING FULL LICENSE ELEMENT'                       
         BAS   RE,GETEL                                                         
         BNE   *+10                                                             
         MVC   LSTLIC,CSFLIC                                                    
         MVC   LSTOXC,LDCKOXC                                                   
         DROP  R3                                                               
*                                                                               
         MVI   LSTOP,C'('                                                       
         MVC   LSTLICS,LDCKLIC                                                  
         MVI   LSTSL,C'/'                                                       
         EDIT  LDCKSEQ,LSTSNUM,ZERO=NOBLANK,FILL=0                              
         MVI   LSTCP,C')'                                                       
*                                                                               
         GOTO1 LISTMON             SEND LINE TO SCREEN                          
         B     LR020                                                            
*                                                                               
LRX      B     EXIT                                                             
         DROP  R4                                                               
                                                                                
***********************************************************************         
* GETEL AND LITERALS                                                            
***********************************************************************         
         GETEL  R3,DATADISP,ELCODE                                              
*                                                                               
         LTORG                                                                  
                                                                                
***********************************************************************         
* INCLUDES                                                                      
***********************************************************************         
       ++INCLUDE FAGETTXTD                                                      
       ++INCLUDE CTGENFILE                                                      
       ++INCLUDE GEGENFILE                                                      
       ++INCLUDE GEGENCDC                                                       
       ++INCLUDE DDCOMFACS                                                      
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE DDSPOOLD                                                       
*                                                                               
       ++INCLUDE CTSFMFFD                                                       
         ORG   CONTAGH                                                          
       ++INCLUDE CTSFM8ED                                                       
         ORG   CONTAGH                                                          
       ++INCLUDE CTSFM8FD                                                       
*                                                                               
       ++INCLUDE CTSFMWORKD                                                     
         PRINT ON                                                               
         ORG   SYSSPARE                                                         
*                                                                               
RELO     DS    A                                                                
CURLIC   DS    CL32                LICENSE CODE                                 
CURSEQ   DS    XL2                 SEQUENCE NUMBER                              
CUROXC   DS    CL8                 OX DEMO CODE                                 
*                                                                               
SPOOLD   DSECT                                                                  
         ORG   P                                                                
*                                                                               
GEND     DSECT                                                                  
         ORG   LISTAR                                                           
LSTLIC   DS    CL32                LICENSE                                      
         DS    CL2                                                              
LSTOXC   DS    CL8                 OXCODE                                       
         DS    CL6                                                              
LSTOP    DS    CL1                                                              
LSTLICS  DS    CL10                NUMBER                                       
LSTSL    DS    CL1                                                              
LSTSNUM  DS    CL6                 NUMBER                                       
LSTCP    DS    CL1                                                              
         DS    CL1                                                              
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'157CTSFM25   11/12/18'                                      
         END                                                                    
