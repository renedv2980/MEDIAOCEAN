*          DATA SET SPSFM79    AT LEVEL 002 AS OF 05/07/18                      
*PHASE T21779A                                                                  
T21779   TITLE 'SPSFM79 - BILL ADDRESS PROGRAM'                                 
***********************************************************************         
*                                                                               
*  TITLE:       T21779  -- BILL ADDRESS MAINT/LIST                              
*               T29729  -- BILL ADDRESS MAINT SCREEN                            
*               T2972B  -- BILL ADDRESS LIST SCREEN                             
*                                                                               
*  COMMENTS:    MAINTAINS BILL ADDRESS RECORDS                                  
*                                                                               
*  CALLED FROM: SFM CONTROLLER (T21700), WHICH CALLS                            
*               GEGENCON (T00A30), WHICH CALLS THIS.                            
*                                                                               
*  INPUTS:      SCREEN SCSFM29 (MAINT) & SCSFM2B (LIST)                         
*                                                                               
*  OUTPUTS:                                                                     
*                                                                               
*  REGISTERS:    R0 -- WORK                                                     
*                R1 -- WORK                                                     
*                R2 -- SCREEN FIELD HEADER                                      
*                R3 -- WORK                                                     
*                R4 -- WORK                                                     
*                R5 -- WORK                                                     
*                R6 -- GETEL REGISTER                                           
*                R7 -- SECOND BASE                                              
*                R8 -- SPOOL                                                    
*                R9 -- SYSD                                                     
*                RA -- TWA                                                      
*                RB -- FIRST BASE                                               
*                RC -- GEND                                                     
*                RD -- SYSTEM                                                   
*                RE -- SYSTEM                                                   
*                RF -- SYSTEM                                                   
*                                                                               
***********************************************************************         
T21779   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T21779,R7,RR=R3                                                
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     R8,ASPOOLD          GENERAL PRINT AREAS                          
         USING SPOOLD,R8                                                        
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA      BASE SCREEN FOR SYSTEM + THIS PROG           
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
*                                                                               
         OI    CONSERVH+6,X'81'    SERVICE REQ FLD MODIFIED                     
*                                                                               
         CLI   MODE,VALKEY         VALIDATE RECORD KEY                          
         BE    VK                                                               
         CLI   MODE,VALREC         VALIDATE RECORD                              
         BE    VR                                                               
         CLI   MODE,DISPKEY        DISPLAY KEY (FOR LIST)                       
         BE    DK                                                               
         CLI   MODE,DISPREC        DISPLAY RECORD                               
         BE    DR                                                               
         CLI   MODE,LISTRECS       DISPLAY RECORD                               
         BE    LR                                                               
*                                                                               
XIT      XIT1                                                                   
*                                                                               
*                                                                               
***********************************************************************         
*                       VALIDATE KEY                                  *         
***********************************************************************         
*                                                                               
VK       LA    R2,ADRMEDH          MEDIA                                        
         CLI   5(R2),0             ANY INPUT?                                   
         BE    ERRMIS              NO, THIS IS REQUIRED                         
*                                                                               
         GOTO1 VALIMED                                                          
*                                                                               
         XC    BCLT,BCLT                                                        
         LA    R2,ADRCLTH          CLIENT                                       
         CLI   5(R2),0             ANY INPUT?                                   
         BNE   VK05                YES                                          
         CLI   ACTNUM,ACTLIST      NOT REQUIRED IF ACTION=LIST                  
         BE    VK10                                                             
         B     ERRMIS                                                           
*                                                                               
VK05     GOTO1 VALICLT                                                          
*                                                                               
VK10     MVI   BPRD,0                                                           
         LA    R2,ADRPRDH          VALIDATE PRODUCT                             
         CLI   5(R2),0             ANY INPUT?                                   
         BNE   VK15                YES                                          
         CLI   ACTNUM,ACTLIST      NOT REQUIRED IF ACTION=LIST                  
         BE    VK20                                                             
         B     ERRMIS                                                           
*                                                                               
VK15     MVI   AAAOK,C'Y'                                                       
         GOTO1 VALIPRD                                                          
         MVI   AAAOK,C'N'                                                       
         MVC   ADRPRD,QPRD                                                      
*                                                                               
* BUILD KEY                                                                     
VK20     XC    KEY,KEY                                                          
         LA    R6,KEY              REQUIRED                                     
         USING ADRKEY,R6                                                        
         MVC   ADRKID,=X'0D09'                                                  
         MVC   ADRKAM,BAGYMD                                                    
         MVC   ADRKCLT,BCLT                                                     
         CLI   BPRD,0                                                           
         BE    *+10                                                             
         MVC   ADRKPRD,ADRPRD                                                   
*                                                                               
         MVC   SVMYKEY,KEY         SAVE KEY                                     
         B     XIT                                                              
         DROP  R6                                                               
*                                                                               
*                                                                               
***********************************************************************         
*                       VALIDATE RECORD                               *         
***********************************************************************         
*                                                                               
VR       MVI   ELCODE,X'10'        NAME ELEM                                    
         GOTO1 REMELEM                                                          
*                                                                               
         LA    R2,ADRNM1H                                                       
         CLI   5(R2),0             ANY NAME 1?                                  
         BE    ERRMIS              NO, MISSING                                  
*                                                                               
         XC    ELEM,ELEM                                                        
         LA    R6,ELEM                                                          
         USING ADRNAMEL,R6                                                      
         MVI   ADRNAMEL,X'10'      ELEM CODE                                    
         MVI   ADRNAMLN,ADRNAMEX-ADRNAMEL  ELEM LEN                             
         MVC   ADRNAM1,ADRNM1                                                   
         OC    ADRNAM1,SPACES                                                   
         MVC   ADRNAM2,ADRNM2                                                   
         OC    ADRNAM2,SPACES                                                   
         GOTO1 ADDELEM                                                          
         DROP  R6                                                               
*                                                                               
         MVI   ELCODE,X'20'        ADDRESS ELEM                                 
         GOTO1 REMELEM                                                          
*                                                                               
         LA    R2,ADRAL1H                                                       
         CLI   5(R2),0             ANY ADDRESS 1?                               
         BNE   VR05                                                             
*                                                                               
         LA    R2,ADRAL2H                                                       
         CLI   5(R2),0             ANY ADDRESS 2?                               
         BE    VR10                                                             
*                                                                               
VR05     XC    ELEM,ELEM                                                        
         LA    R6,ELEM                                                          
         USING ADRADDEL,R6                                                      
         MVI   ADRADDEL,X'20'      ELEM CODE                                    
         MVI   ADRADDLN,ADRADDRX-ADRADDEL  ELEM LEN                             
         MVC   ADRADDR1,ADRAL1                                                  
         OC    ADRADDR1,SPACES                                                  
         MVC   ADRADDR2,ADRAL2                                                  
         OC    ADRADDR2,SPACES                                                  
         GOTO1 ADDELEM                                                          
         DROP  R6                                                               
*                                                                               
VR10     MVI   ELCODE,X'30'        ATTENTION ELEM                               
         GOTO1 REMELEM                                                          
*                                                                               
         CLI   ADRATTH+5,0         ANY ATTENTION OF?                            
         BE    VRX                 NO, OPTIONAL                                 
*                                                                               
         XC    ELEM,ELEM                                                        
         LA    R6,ELEM                                                          
         USING ADRATTEL,R6                                                      
         MVI   ADRATTEL,X'30'      ELEM CODE                                    
         MVI   ADRATTLN,ADRATTX-ADRATTEL  ELEM LEN                              
         MVC   ADRATTN,ADRATT                                                   
         OC    ADRATTN,SPACES                                                   
         GOTO1 ADDELEM                                                          
         DROP  R6                                                               
*                                                                               
VRX      B     XIT                                                              
*                                                                               
*                                                                               
***********************************************************************         
*                       DISPLAY KEY                                   *         
***********************************************************************         
*                                                                               
DK       L     R4,AIO                                                           
         USING ADRRECD,R4                                                       
*                                                                               
         XC    ADRCLT,ADRCLT                                                    
         GOTO1 CLUNPK,DMCB,ADRKCLT,ADRCLT CLIENT                                
         OI    ADRCLTH+6,X'80'                                                  
*                                                                               
         XC    ADRPRD,ADRPRD                                                    
         MVC   ADRPRD,ADRKPRD      PRODUCT                                      
         OI    ADRPRDH+6,X'80'                                                  
*                                                                               
         B     XIT                                                              
         DROP  R4                                                               
*                                                                               
*                                                                               
***********************************************************************         
*                       DISPLAY RECORD                                *         
***********************************************************************         
*                                                                               
DR       L     R6,AIO                                                           
         MVI   ELCODE,X'10'        NAME ELEM                                    
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         XC    ADRNM1,ADRNM1       INIT NAME LINE 1                             
         OI    ADRNM1H+6,X'80'     TRANSMIT                                     
         XC    ADRNM2,ADRNM2                                                    
         OI    ADRNM2H+6,X'80'     TRANSMIT                                     
*                                                                               
         USING ADRNAMEL,R6                                                      
         MVC   ADRNM1,ADRNAM1      NAME LINE 1                                  
         MVC   ADRNM2,ADRNAM2      NAME LINE 2                                  
*                                                                               
         XC    ADRAL1,ADRAL1       INIT ADDRESS LINE 1                          
         OI    ADRAL1H+6,X'80'     TRANSMIT                                     
         XC    ADRAL2,ADRAL2                                                    
         OI    ADRAL2H+6,X'80'                                                  
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,X'20'        ADDRESS ELEM                                 
         BAS   RE,GETEL                                                         
         BNE   DR10                                                             
*                                                                               
         USING ADRADDEL,R6                                                      
         MVC   ADRAL1,ADRADDR1     ADDRESS LINE 1                               
         MVC   ADRAL2,ADRADDR2     LINE 2                                       
*                                                                               
DR10     XC    ADRATT,ADRATT       INIT ATTENTION OF                            
         OI    ADRATTH+6,X'80'     TRANSMIT                                     
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,X'30'        ATTENTION ELEM                               
         BAS   RE,GETEL                                                         
         BNE   DRX                 OPTIONAL                                     
*                                                                               
         USING ADRATTEL,R6                                                      
         MVC   ADRATT,ADRATTN      ATTENTION OF                                 
*                                                                               
DRX      B     XIT                                                              
         DROP  R6                                                               
*                                                                               
*                                                                               
***********************************************************************         
* LIST DEAL RECORDS                                                             
***********************************************************************         
*                                                                               
LR       MVI   NLISTS,15                                                        
         LA    R4,KEY                                                           
         USING ADRKEY,R4                                                        
         OC    KEY(13),KEY         TEST 1ST TIME?                               
         BNZ   LR05                                                             
*                                                                               
         MVC   KEY,SVMYKEY         MOVE IN SAVED KEY                            
*                                                                               
LR05     GOTO1 HIGH                                                             
         CLC   SVMYKEY(2),KEY      ANY RECS?                                    
         BNE   XIT                                                              
         B     LR10                                                             
*                                                                               
LRSEQ    GOTO1 SEQ                                                              
*                                                                               
LR10     CLC   ADRKID,KEYSAVE      STILL BILL ADDRESS RECORD?                   
         BNE   LRX                                                              
*                                                                               
         CLC   ADRKAM,BAGYMD                                                    
         BNE   LRX                                                              
*                                                                               
         OC    BCLT,BCLT           WAS CLIENT ENTERED?                          
         BZ    *+14                                                             
         CLC   ADRKCLT,BCLT        MATCH ON CLIENT?                             
         BNE   LRSEQ                                                            
*                                                                               
         OC    BPRD,BPRD           WAS PRODUCT ENTERED?                         
         BZ    *+14                                                             
         CLC   ADRKPRD,QPRD        MATCH ON PRODUCT?                            
         BNE   LRSEQ                                                            
         DROP  R4                                                               
*                                                                               
         L     R6,AIO                                                           
         GOTO1 GETREC                                                           
         USING ADRRECD,R6                                                       
*                                                                               
         MVC   LISTAR,SPACES                                                    
         GOTO1 CLUNPK,DMCB,ADRKCLT,LSCLT                                        
         MVC   LSPRD,ADRKPRD                                                    
*                                                                               
         MVI   ELCODE,X'10'        NAME ELEMENT                                 
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         USING ADRNAMEL,R6                                                      
         MVC   LSNAME,ADRNAM1                                                   
*                                                                               
         GOTO1 LISTMON                                                          
*                                                                               
         LA    R6,KEY                                                           
         B     LRSEQ                                                            
*                                                                               
LRX      B     XIT                                                              
*                                                                               
*                                                                               
***********************************************************************         
*                          ERROR MESSAGES                             *         
***********************************************************************         
*                                                                               
ERRMIS   MVI   ERROR,MISSING                                                    
         MVC   AIO,AIO1                                                         
         GOTO1 ERREX                                                            
*                                                                               
         GETEL R6,DATADISP,ELCODE                                               
         EJECT                                                                  
         LTORG                                                        *         
         EJECT                                                                  
       ++INCLUDE SPGENADR                                                       
         EJECT                                                                  
       ++INCLUDE SPSFMWORKD                                                     
         EJECT                                                                  
***********************************************************************         
*        SAVED STORAGE DSECT                                          *         
***********************************************************************         
*                                                                               
         ORG   SYSSPARE                                                         
*                                                                               
SVMYKEY  DS    XL(L'KEY)                                                        
*                                                                               
         EJECT                                                                  
***********************************************************************         
         PRINT  ON                                                              
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE SPSFMFFD                                                       
         ORG   CONTAGH                                                          
       ++INCLUDE SCSFM29D          MAINTENACE SCREEN                            
       ++INCLUDE DDGENTWA                                                       
       ++INCLUDE FAGETTXTD         ERROR MSGS                                   
         EJECT                                                                  
*                                                                               
GEND     DSECT                                                                  
         ORG   LISTAR                                                           
LSCLT    DS    CL3                                                              
         DS    CL1                                                              
LSPRD    DS    CL3                                                              
         DS    CL1                                                              
LSNAME   DS    CL60                                                             
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'002SPSFM79   05/07/18'                                      
         END                                                                    
