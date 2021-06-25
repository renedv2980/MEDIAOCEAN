*          DATA SET SPSFM5D    AT LEVEL 003 AS OF 03/13/02                      
*PHASE T2175DA                                                                  
*                                                                               
***********************************************************************         
*                                                                     *         
*  TITLE:        T2175D  -- REASON CODE RECS MAINTENANCE & LIST       *         
*                      AND PURPOSE CODE RECS MAINTENANCE & LIST       *         
*                                                                     *         
*  CALLED FROM:  SFM CONTROLLER (T21700), WHICH CALLS                 *         
*                GEGENCON (T00A30), WHICH CALLS THIS.                 *         
*                                                                     *         
*  INPUTS:       SCREEN SPSFM6D (MAINT) & SPSFM6C (LIST)              *         
*                                                                     *         
*  REGISTERS:    R0 -- WORK                                           *         
*                R1 -- WORK                                           *         
*                R2 -- SCREEN FIELD HEADER                            *         
*                R3 -- WORK                                           *         
*                R4 -- WORK                                           *         
*                R5 -- WORK                                           *         
*                R6 -- GETEL REGISTER                                 *         
*                R7 -- SECOND BASE                                    *         
*                R8 -- SPOOL                                          *         
*                R9 -- SYSD                                           *         
*                RA -- TWA                                            *         
*                RB -- FIRST BASE                                     *         
*                RC -- GEND                                           *         
*                RD -- SYSTEM                                         *         
*                RE -- SYSTEM                                         *         
*                RF -- SYSTEM                                         *         
*                                                                     *         
***********************************************************************         
         TITLE 'T2175D - RESON CODE RECORD MAINTENANCE AND LIST'                
T2175D   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**175D**,R7,RR=R3                                              
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     R8,ASPOOLD          GENERAL PRINT AREAS                          
         USING SPOOLD,R8                                                        
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA      BASE SCREEN FOR SYSTEM + THIS PROG           
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         ST    R3,RELO                                                          
*                                                                               
         BAS   RE,SETUP                                                         
*                                                                               
         CLI   MODE,VALKEY         VALIDATE RECORD KEY                          
         BE    VK                                                               
         CLI   MODE,VALREC         VALIDATE RECORD                              
         BE    VR                                                               
         CLI   MODE,DISPKEY        DISPLAY KEY (FOR LIST)                       
         BE    DK                                                               
         CLI   MODE,DISPREC        DISPLAY RECORD                               
         BE    DR                                                               
         CLI   MODE,LISTRECS                                                    
         BE    LR                  LIST RECORDS                                 
         CLI   MODE,PRINTREP                                                    
         BNE   XIT                                                              
         LA    R5,RHDSPECS                                                      
         ST    R5,SPECS                                                         
         CLI   CONREC,C'P'         PURPOSE CODES?                               
         BNE   LR                                                               
         LA    R5,PHDSPECS                                                      
         ST    R5,SPECS                                                         
         LA    R5,PHDHOOK                                                       
         ST    R5,HEADHOOK                                                      
         B     LR                  PRINT REPORT BASE ON LISTED RECORDS          
*                                                                               
XIT      XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
*                       VALIDATE KEY                                  *         
***********************************************************************         
VK       DS    0X                                                               
         CLI   CONREC,C'P'         PURPOSE CODES?                               
         BNE   VK10                                                             
         LA    R2,PMNMEDH          VALIDATE MEDIA                               
         CLI   5(R2),0                                                          
         BE    ERRMIS                                                           
         GOTO1 VALIMED                                                          
*                                                                               
VK10     LA    R2,RMNRSNH          REASON OR PURPOSE CODE                       
         CLI   CONREC,C'P'         PURPOSE CODES?                               
         BNE   *+8                                                              
         LA    R2,PMNPURPH                                                      
         CLI   5(R2),0                                                          
         BNE   *+12                                                             
         CLI   ACTNUM,ACTLIST      NOT REQ'D FOR LIST                           
         BNE   ERRMIS                                                           
         OC    8(6,R2),SPACES                                                   
*                                                                               
         CLI   CONREC,C'P'         PURPOSE CODES?                               
         BNE   VK20                                                             
         CLC   PMNPURP(4),=C'LIST'   CANNOT START PURPOSE CODE W/LIST           
         BE    ERRINV                                                           
*                                                                               
VK20     XC    KEY,KEY             PREPARE KEY                                  
         LA    R1,KEY                                                           
         CLI   CONREC,C'P'         PURPOSE CODES?                               
         BE    VK30                                                             
*                                                                               
         USING RSNKEY,R1           REASON KEY                                   
         MVI   RSNKTYP,RSNKTYPQ    X'0D'                                        
         MVI   RSNKSUB,RSNKSUBQ    X'18'                                        
         MVC   RSNKAGY,AGENCY                                                   
         MVC   RSNCODE,RMNRSN                                                   
         B     VK50                                                             
         DROP  R1                                                               
*                                                                               
*                                                                               
         USING PRPKEY,R1           PURPOSE KEY                                  
VK30     MVI   PRPKTYP,PRPKTYPQ    X'0D'                                        
         MVI   PRPKSUB,PRPKSUBQ    X'19'                                        
         MVC   PRPKAGY,AGENCY                                                   
         MVC   PRPKMED,PMNMED                                                   
         MVC   PRPCODE,PMNPURP                                                  
         DROP  R1                                                               
*                                                                               
VK50     MVC   SAVEKEY,KEY                                                      
         MVC   AIO,AIO1                                                         
VKX      B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
*                       VALIDATE RECORD                               *         
***********************************************************************         
VR       DS    0X                                                               
         MVI   ELCODE,RSNELQ       X'01'                                        
         GOTO1 REMELEM                                                          
*                                                                               
         LA    R4,ELEM                                                          
         USING RSNEL,R4                                                         
         XC    ELEM,ELEM                                                        
         MVI   RSNEL,RSNELQ        X'01' ELEM                                   
         MVI   RSNELLEN,RSNELLNQ   ELEMENT LENGTH                               
*                                                                               
*  VALIDATE USER INPUT REQ'D FOR RCODE ONLY                                     
*                                                                               
         MVI   DESCREQD,C'Y'       DESC REQ'D FOR PURPOSE                       
         MVI   RSNELINP,C'N'       DEFAULT TO N                                 
*                                                                               
         CLI   CONREC,C'R'                                                      
         BNE   VR15                NO USER INPUT FOR PURPOSE CODES              
         LA    R2,RMNUSRH          USER INPUT REQ'D                             
         CLI   RMNUSRH+5,0                                                      
         BE    ERRMIS                                                           
         CLI   RMNUSR,C'N'                                                      
         BE    VR10                                                             
*                                                                               
         CLI   RMNUSR,C'Y'                                                      
         BNE   ERRINV                                                           
         MVI   DESCREQD,C'N'       NO DESC                                      
VR10     MVC   RSNELINP,RMNUSR     Y/N                                          
*                                                                               
* VALIDATE COMMENT                                                              
*                                                                               
VR15     LA    R2,RMNDESCH         REASON DESCRIPTION                           
         CLI   CONREC,C'R'                                                      
         BE    *+8                                                              
         LA    R2,PMNDESCH         PURPOSE DESCRIPTION                          
*                                                                               
         CLI   5(R2),0                                                          
         BNE   VR20                                                             
         CLI   DESCREQD,C'Y'       DESCRIPTION REQ'D                            
         BE    ERRMIS              COMMENT REQ'D IF USER=N                      
         XC    RSNELTXT,RSNELTXT   USER=Y = OK NO COMMENT                       
         B     VR100                                                            
*                                                                               
VR20     CLI   DESCREQD,C'N'       USER WILL ENTER DESCRIPTION                  
         BE    ERRINV              THEN COMMENTS HERE INVALID                   
         MVC   RSNELTXT,8(R2)                                                   
         OC    RSNELTXT,SPACES                                                  
*                                                                               
VR100    GOTO1 ADDELEM                                                          
         DROP  R4                                                               
*                                                                               
VRX      B     DR                  REDISPLAY RECORD                             
         EJECT                                                                  
***********************************************************************         
*                       DISPLAY RECORD                                *         
***********************************************************************         
DR       DS    0X                                                               
         L     R6,AIO                                                           
         USING RSNEL,R6                                                         
         MVI   ELCODE,RSNELQ                                                    
         BAS   RE,GETEL                                                         
         BNE   DRX                 ELEMENT ALWAYS PRESENT                       
*                                                                               
         CLI   CONREC,C'R'         REASON CODES?                                
         BNE   DR10                ELSE SKIP FOR PURPOSE CODES                  
         MVC   RMNUSR,RSNELINP                                                  
         OI    RMNUSRH+6,X'80'                                                  
*                                                                               
DR10     LA    R2,RMNDESCH                                                      
         CLI   CONREC,C'R'         REASON CODES?                                
         BE    *+8                                                              
         LA    R2,PMNDESCH                                                      
         MVC   8(L'RSNELTXT,R2),RSNELTXT                                        
         OI    6(R2),X'80'                                                      
         DROP  R6                                                               
*                                                                               
DRX      B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
*                       DISPLAY KEY                                   *         
***********************************************************************         
DK       DS    0X                                                               
         L     R6,AIO                                                           
*                                                                               
         CLI   CONREC,C'R'         REASON CODES?                                
         BNE   DK10                                                             
         USING RSNRECD,R6                                                       
         MVC   RMNRSN,RSNCODE                                                   
         OI    RMNRSNH+6,X'80'                                                  
         B     DKX                                                              
*                                                                               
         USING PRPRECD,R6                                                       
DK10     MVC   PMNMED,PRPKMED                                                   
         OI    PMNMEDH+6,X'80'                                                  
         MVC   PMNPURP,PRPCODE                                                  
         OI    PMNPURPH+6,X'80'                                                 
*                                                                               
DKX      MVC   SAVEKEY,KEY                                                      
         B     XIT                                                              
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
*                       LIST RECORDS                                  *         
***********************************************************************         
LR       DS    0X                                                               
*                                                                               
         LA    R6,KEY                                                           
         OC    KEY,KEY             FIRST TIME THROUGHT?                         
         BNZ   LR10                                                             
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(L'SAVEKEY),SAVEKEY                                           
*                                                                               
LR10     GOTO1 HIGH                                                             
         CLI   DMCB+8,0                                                         
         BE    LR30                                                             
         DC    H'0'                                                             
*                                                                               
LR20     LA    R6,KEY                                                           
         GOTO1 SEQ                                                              
         CLI   DMCB+8,0                                                         
         BE    LR30                                                             
         DC    H'0'                                                             
*                                                                               
LR30     CLC   KEY(4),SAVEKEY      SAME REC TYPE/AGY                            
         BNE   LRX                                                              
         CLI   CONREC,C'P'         PURPOSE?                                     
         BNE   LR35                                                             
         CLC   KEY+4(1),PLSMED     SAME MEDIA?                                  
         BNE   LRX                 THEN DONE                                    
*                                                                               
LR35     MVC   LISTAR,SPACES                                                    
*                                                                               
         GOTO1 GETREC                                                           
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R6,AIO                                                           
         USING PRPRECD,R6                                                       
         CLI   CONREC,C'P'         PURPOSE?                                     
         BNE   LR40                                                             
         MVC   LSCODE,PRPCODE                                                   
         B     LR45                                                             
*                                                                               
         USING RSNRECD,R6                                                       
LR40     MVC   LSCODE,RSNCODE                                                   
*                                                                               
LR45     CLI   RSNELINP,C'Y'                                                    
         BNE   *+14                                                             
         MVC   LSCOM(23),=C'**USER INPUT REQUIRED**'                            
         B     LR50                                                             
         MVC   LSCOM,RSNELTXT                                                   
         B     LR50                                                             
*                                                                               
LR50     CLI   MODE,PRINTREP                                                    
         BNE   LR60                                                             
         MVC   P(78),LISTAR                                                     
         GOTO1 SPOOL,DMCB,SPOOLD                                                
         B     LR20                                                             
*                                                                               
LR60     GOTO1 LISTMON                                                          
         B     LR20                                                             
*                                                                               
LRX      B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
*                          ERROR MESSAGES                             *         
***********************************************************************         
*                                                                               
ERRINV   MVI   ERROR,INVALID                                                    
         B     VSFMERR                                                          
ERRMIS   MVI   ERROR,MISSING                                                    
         B     VSFMERR                                                          
*                                                                               
SPERREX  OI    GENSTAT2,USGETTXT                                                
         LA    RF,GETTXTCB                                                      
         USING GETTXTD,RF                                                       
         MVC   GTMSGNO,ERRNUM                                                   
         MVI   GTMTYP,GTMERR                                                    
         MVI   GTMSYS,2                                                         
VSFMERR  MVC   AIO,AIO1                                                         
         GOTO1 ERREX                                                            
         DROP  RF                                                               
         SPACE 2                                                                
*                                  SHORT DESP OF ERROR MSGS                     
BKLNINV  EQU   449                 BREAK LN MUST BE 1-4                         
*                                                                               
         SPACE 2                                                                
         GETEL R6,DATADISP,ELCODE                                               
*                                                                               
         EJECT                                                                  
***********************************************************************         
*        SETUP                                                        *         
***********************************************************************         
SETUP    NTR1                                                                   
         OI    GENSTAT1,USKYMRG+NOSETEFH                                        
         OI    CONSERVH+1,X'01'    MODIFY SERVICE REQUEST                       
         OI    CONSERVH+6,X'80'    TRANSMIT TO GET CONTROL                      
SETUPX   B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
*        HEAD LINE SPECS - PURPOSE CODES                              *         
***********************************************************************         
PHDSPECS DS    0H                                                               
         SSPEC H1,1,C'MEDIA'                                                    
         SSPEC H1,58,RUN                                                        
         SSPEC H2,58,PAGE                                                       
         SSPEC H1,25,C'PURPOSE CODES'                                           
         SSPEC H2,25,C'-------------'                                           
         SSPEC H4,1,C'CODE  '                                                   
         SSPEC H5,1,C'------'                                                   
         SSPEC H4,14,C'DESCRIPTION'                                             
         SSPEC H5,14,C'---------------------------------'                       
         DC    X'00'                                                            
***********************************************************************         
*        HEAD LINE SPECS - PURPOSE CODES                              *         
***********************************************************************         
PHDHOOK  NTR1                                                                   
         MVC   H1+6(1),QMED                                                     
         MVC   H1+8(10),MEDNM                                                   
         XIT1                                                                   
***********************************************************************         
*        HEAD LINE SPECS - REASON CODES                               *         
***********************************************************************         
RHDSPECS DS    0H                                                               
         SSPEC H1,58,RUN                                                        
         SSPEC H2,58,PAGE                                                       
         SSPEC H1,25,C'REASON CODES'                                            
         SSPEC H2,25,C'------------'                                            
         SSPEC H4,1,C'CODE  '                                                   
         SSPEC H5,1,C'------'                                                   
         SSPEC H4,14,C'DESCRIPTION'                                             
         SSPEC H5,14,C'---------------------------------'                       
         DC    X'00'                                                            
***********************************************************************         
*        LTORG                                                        *         
***********************************************************************         
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
         PRINT OFF                                                              
       ++INCLUDE DDSPOOLD                                                       
         EJECT                                                                  
       ++INCLUDE DDSPLWORKD                                                     
         EJECT                                                                  
         PRINT ON                                                               
         EJECT                                                                  
       ++INCLUDE SPSFMFFD                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE SCSFM6DD          REASON MAINT SCREEN                          
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE SCSFM6BD          PURPOSE MAINT SCREEN                         
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE SCSFM6CD          REASON LIST SCREEN                           
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE SCSFM6ED          PURPOSE LIST SCREEN                          
         EJECT                                                                  
       ++INCLUDE DDGENTWA                                                       
         EJECT                                                                  
       ++INCLUDE SPGENRSN          REASON CODE RECORD DSECT                     
         EJECT                                                                  
       ++INCLUDE SPGENPURP         PURPOSE CODE RECORD DSECT                    
         EJECT                                                                  
       ++INCLUDE SPSFMWORKD                                                     
         EJECT                                                                  
* START OF SAVED STORAGE (6144)                                                 
*                                                                               
         ORG   SYSSPARE                                                         
RELO     DS    A                   RELOCATION FACTOR                            
LISTSEL  DS    C                   FOR USE WITH LIST SELECTIONS                 
SAVEKEY  DS    CL13                                                             
ERRNUM   DS    XL2                                                              
DESCREQD DS    CL1                                                              
*                                                                               
         EJECT                                                                  
       ++INCLUDE FAGETTXTD         ERROR MSGS                                   
*                                                                               
         EJECT                                                                  
GEND     DSECT                                                                  
         ORG   LISTAR              LABELS FOR LISTMON                           
LSCODE   DS    CL6                                                              
         DS    CL7                                                              
LSCOM    DS    CL30                                                             
         DS    CL2                                                              
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'003SPSFM5D   03/13/02'                                      
         END                                                                    
