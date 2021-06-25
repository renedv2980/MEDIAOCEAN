*          DATA SET SPSFM05    AT LEVEL 002 AS OF 03/14/02                      
*PHASE T21705A                                                                  
T21705   TITLE 'SPSFM05 - CONTRACTOR MAINTENANCE OVERLAY'                       
***********************************************************************         
*                                                                     *         
*  TITLE:        SPSFM05 -- CONTRACTOR MAINTENANCE OVERLAY            *         
*                                                                     *         
*  COMMENTS:                                                          *         
*                                                                     *         
*  CALLED FROM:  SFM CONTROLLER (T21700), WHICH CALLS                 *         
*                GEGENCON (T00A30), WHICH CALLS THIS.                 *         
*                                                                     *         
*  INPUTS:       SCREENS SCSFM81 (LIST)                               *         
*                        SCSFM85 (MAINT)                              *         
*                                                                     *         
*  OUTPUTS:      NONE                                                 *         
*                                                                     *         
*  REGISTERS:    R0 -- WORK                                           *         
*                R1 -- WORK                                           *         
*                R2 -- SCREEN FIELD HEADER                            *         
*                R3 -- WORK, GETEL2 REGISTER                          *         
*                R4 -- WORK                                           *         
*                R5 -- THIRD BASE                                               
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
***********************************************************************         
*============================= UPDATE LOG ============================*         
* DATE    LEV WHO  DESCRIPTION                                        *         
* ------- -- ----  ---------------------------------------------------*         
* 08NOV94 01 SPRI  ORIGINAL ENTRY                                     *         
*                                                                     *         
*                                                                     *         
*                                                                     *         
***********************************************************************         
         EJECT                                                                  
T21705   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,*T21705*,R7,RR=RE                                              
                                                                                
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA      BASE SCREEN FOR SYSTEM + THIS PROG           
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
                                                                                
*                                                                               
*        CLI   MODE,PROCPFK        PROCESS PFKEY                                
*        BE    PF                                                               
                                                                                
         CLI   MODE,VALKEY         VALIDATE KEY                                 
         BE    VK                                                               
                                                                                
         CLI   MODE,VALREC         VALIDATE RECORD                              
         BE    VR                                                               
                                                                                
         CLI   MODE,DISPKEY        DISPLAY KEY                                  
         BE    DK                                                               
                                                                                
         CLI   MODE,XRECADD        RECORD HAS JUST BEEN ADDED                   
         BE    DR                                                               
                                                                                
         CLI   MODE,XRECPUT        RECORD HAS JUST BEEN PUT                     
         BE    DR                                                               
                                                                                
         CLI   MODE,DISPREC        DISPLAY RECORD                               
         BE    DR                                                               
                                                                                
         CLI   MODE,LISTRECS       LIST RECORDS                                 
         BE    LR                                                               
                                                                                
*        CLI   MODE,REPORT         PRINT RECORDS                                
*        BE    PR                                                               
                                                                                
*        CLI   MODE,RECPUT         PUT RECORD                                   
*        BE    PR                                                               
                                                                                
*        CLI   MODE,XRECPUT        AFTER PUT RECORD                             
*        BE    XP                                                               
                                                                                
NEXTMODE DS    0H                                                               
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
*                         VALIDATE KEY ROUTINE                        *         
***********************************************************************         
VK       DS    0H                                                               
                                                                                
* VALIDATE MEDIA                                                                
         LA    R2,CTMMEDH          MEDIA                                        
         TM    4(R2),X'20'         VALIDATED PREVIOUSLY                         
         BO    VK10                                                             
         GOTO1 VALIMED                                                          
         OI    4(R2),X'20'                                                      
                                                                                
* BUILD KEY FOR GENCON                                                          
VK10     XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING CXRRECD,R4                                                       
         MVI   CXRKTYP,CXRKTYPQ    RECORD TYPE                                  
         MVI   CXRKSUB,CXRKSUBQ    SUB TYPE                                     
         MVC   CXRKAGMD,BAGYMD     BINARY AGENCY MEDIA                          
                                                                                
* VALIDATE CONTRACTOR                                                           
         CLI   ACTEQU,ACTLIST      LIST?                                        
         BE    VK20                                                             
         LA    R2,CTMCNTRH         CONTRACTOR                                   
         GOTO1 ANY                                                              
                                                                                
VK20     MVC   CXRKCTA,WORK        CONTRACTOR                                   
                                                                                
         CLI   ACTEQU,ACTDEL       DELETE?                                      
         BNE   VKX                                                              
         MVI   ERROR,NOTFOUND                                                   
         GOTO1 HIGH                                                             
         CLC   KEY(L'CXRKEY),KEYSAVE                                            
         BNE   MYERREX                                                          
         MVC   ERRNUM,=AL2(SE#CNTDL)                                            
         TM    CXRKCNTL,CXRKCCNT   CONTRACT RECORD EXISTS                       
         BO    SPERREX                                                          
                                                                                
         DROP  R4                                                               
                                                                                
VKX      B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
*                             DISPLAY KEY                             *         
***********************************************************************         
DK       DS    0H                                                               
         L     R4,AIO                                                           
         USING CXRRECD,R4                                                       
         MVC   CTMMED,QMED         MEDIA                                        
         OI    CTMMEDH+6,X'80'                                                  
         MVC   CTMCNTR,CXRKCTA     CONTRACTOR                                   
         OI    CTMCNTRH+6,X'80'                                                 
                                                                                
         CLI   THISLSEL,C'D'       SELECT FOR DELETE?                           
         BNE   DKX                                                              
         MVC   ERRNUM,=AL2(SE#CNTDL)                                            
         TM    CXRRCNTL,CXRRCCNT   CONTRACT RECORD EXISTS                       
         BO    SPERREX                                                          
DKX      B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
*                           VALIDATE RECORD                           *         
***********************************************************************         
VR       DS    0H                                                               
                                                                                
         L     R4,AIO                                                           
         USING CXRKEY,R4                                                        
         MVC   CXRRAGYA,TWAAGY     ALPHA AGY                                    
         DROP  R4                                                               
                                                                                
**************************** CONTRACTOR *******************************         
         MVI   ELCODE,CXRCTELQ     CONTRACTOR ELEMENT                           
         GOTO1 REMELEM                                                          
                                                                                
         XC    ELEM,ELEM                                                        
         LA    R3,ELEM                                                          
         USING CXRCTEL,R3          CONTRACTOR ELEMENT                           
         MVI   CXRCTEL,CXRCTELQ    ELEMENT CODE                                 
         MVI   CXRCTLEN,CXRCTLNQ   LENGTH                                       
                                                                                
         LA    R2,CTMCONTH         CONTRACTOR NAME                              
         GOTO1 ANY                                                              
         MVC   CXRCTCON,WORK                                                    
                                                                                
         LA    R2,CTMNAMEH         NAME ON CONTRACT                             
         MVC   CXRCTNAM,8(R2)      NAME ON CONTRACT                             
         OC    CXRCTNAM,SPACES                                                  
                                                                                
         GOTO1 ADDELEM             ADD CONTRACTOR ELEMENT                       
         CLI   DMCB+12,0                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
**************************** ADDRESS **********************************         
         MVI   ELCODE,CXADRELQ     ADDRESS ELEMENT                              
         GOTO1 REMELEM                                                          
                                                                                
         LA    R3,ELEM                                                          
         USING CXADRELD,R3         ADDRESS ELEMENT                              
         MVI   CXADRNUM,0          NUMBER OF LINES                              
                                                                                
         XC    ELEM,ELEM                                                        
         MVI   CXADREL,CXADRELQ    ELEMENT CODE                                 
                                                                                
         LA    R2,CTMADR1H         FIRST ADDRESS LINE                           
         CLI   5(R2),0                                                          
         BE    *+12                                                             
         MVI   CXADRNUM,1                                                       
         MVI   CXADRLEN,CXADRL1Q                                                
         MVC   CXADRAD1,8(R2)                                                   
         OC    CXADRAD1,SPACES                                                  
                                                                                
         LA    R2,CTMADR2H         SECOND ADDRESS LINE                          
         CLI   5(R2),0                                                          
         BE    *+12                                                             
         MVI   CXADRNUM,2                                                       
         MVI   CXADRLEN,CXADRL2Q                                                
         MVC   CXADRAD2,8(R2)                                                   
         OC    CXADRAD2,SPACES                                                  
                                                                                
         LA    R2,CTMADR3H         THIRD ADDRESS LINE                           
         CLI   5(R2),0                                                          
         BE    *+12                                                             
         MVI   CXADRNUM,3                                                       
         MVI   CXADRLEN,CXADRL3Q                                                
         MVC   CXADRAD3,8(R2)                                                   
         OC    CXADRAD3,SPACES                                                  
                                                                                
         CLI   CXADRNUM,0                                                       
         BE    VRX                                                              
         GOTO1 ADDELEM             ADD ADDRESS ELEMENT                          
         CLI   DMCB+12,0                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         OI    4(R2),X'20'         PREVIOUSLY VALIDATED                         
                                                                                
                                                                                
         DROP  R3                                                               
VRX      B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
*                            DISPLAY RECORD                           *         
***********************************************************************         
DR       DS    0H                                                               
                                                                                
* CLEAR ADDRESS LINES                                                           
         LA    R2,CTMADR1H         FIRST ADDRESS LINE                           
         LA    R5,CTMADR3H         LAST ADDRESS LINE                            
DR10     CR    R2,R5                                                            
         BH    DR15                                                             
                                                                                
         XC    8(L'CTMADR1,R2),8(R2)                                            
         OI    6(R2),X'80'                                                      
                                                                                
         ZIC   R1,0(R2)                                                         
         AR    R2,R1                                                            
         IC    R1,0(R2)                                                         
         AR    R2,R1                                                            
                                                                                
         B     DR10                                                             
                                                                                
DR15     L     R6,AIO              A(FIRST ELEMENT)                             
         USING CXRCTEL,R6                                                       
         MVI   ELCODE,CXRCTELQ     CONTRACTOR ELEMENT                           
         BAS   RE,GETEL            REQUIRED                                     
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         MVC   CTMCONT,CXRCTCON    CONTRACTOR NAME                              
         OI    CTMCONTH+6,X'80'                                                 
         MVC   CTMNAME,CXRCTNAM    CONTRACTOR NAME                              
         OI    CTMNAMEH+6,X'80'                                                 
                                                                                
         L     R6,AIO              A(FIRST ELEMENT)                             
         USING CXADREL,R6                                                       
         MVI   ELCODE,CXADRELQ     ADDRESS ELEMENT                              
         BAS   RE,GETEL                                                         
         BNE   DR20                                                             
                                                                                
         MVC   CTMADR1,CXADRAD1                                                 
         OI    CTMADR1H+6,X'80'                                                 
         CLI   CXADRNUM,2                                                       
         BL    DR20                                                             
         MVC   CTMADR2,CXADRAD2                                                 
         OI    CTMADR2H+6,X'80'                                                 
         CLI   CXADRNUM,3                                                       
         BL    DR20                                                             
         MVC   CTMADR3,CXADRAD3                                                 
         OI    CTMADR3H+6,X'80'                                                 
                                                                                
DR20     L     R6,AIO              A(FIRST ELEMENT)                             
         USING ACTVD,R6                                                         
         MVI   ELCODE,X'F1'        ACTIVITY ELEMENT                             
         BAS   RE,GETEL            REQUIRED                                     
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         GOTO1 DATCON,DMCB,(3,ACTVADDT),(5,CTMDADD)                             
         OI    CTMDADDH+6,X'80'    DATE RECORD ADDED                            
                                                                                
         GOTO1 DATCON,DMCB,(3,ACTVCHDT),(5,CTMDCHG)                             
         OI    CTMDCHGH+6,X'80'    DATE LAST CHANGED                            
                                                                                
DRX      B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
*                            LIST RECORD                              *         
***********************************************************************         
LR       DS    0H                                                               
                                                                                
         LA    R4,KEY                                                           
         USING CXRRECD,R4                                                       
                                                                                
         OC    KEY(CXRRLEN-CXRKEY),KEY   FIRST TIME THROUGH?                    
         BNZ   LR10                                                             
                                                                                
* BUILD KEY                                                                     
         XC    KEY,KEY                                                          
         MVI   CXRKTYP,CXRKTYPQ    RECORD TYPE                                  
         MVI   CXRKSUB,CXRKSUBQ    SUB TYPE                                     
         MVC   CXRKAGMD,BAGYMD     MEDIA                                        
         MVC   CXRKCTA,CTLCNTR     CONTRACTOR (CODE)                            
         MVC   SAVEKEY(CXRKCTA-CXRKEY),KEY     SAVE RECORD TYPE & MEDIA         
                                                                                
LR10     GOTO1 HIGH                                                             
                                                                                
LR20     CLC   KEY(CXRKCTA-CXRKEY),SAVEKEY   SAME RECORD TYPE & MEDIA?          
         BNE   LRX                                                              
                                                                                
         XC    LISTAR,LISTAR                                                    
         MVC   LSTCNTR,CXRKCTA     CONTRACTOR (CODE)                            
                                                                                
         GOTO1 GETREC                                                           
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         L     R6,AIO                                                           
         USING CXRCTEL,R6          CONTRACTOR ELEMENT                           
         MVI   ELCODE,CXRCTELQ     ELEMENT CODE                                 
                                                                                
         BAS   RE,GETEL            REQUIRED                                     
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         MVC   LSTNAME,CXRCTCON    CONTRACTOR NAME                              
         GOTO1 LISTMON             PRINT LINE                                   
                                                                                
         GOTO1 SEQ                 NEXT CONTRACTOR RECORD                       
         B     LR20                                                             
         DROP  R4,R6                                                            
                                                                                
LRX      B     XIT                                                              
         EJECT                                                                  
SPERREX  OI    GENSTAT2,USGETTXT                                                
         LA    RF,GETTXTCB                                                      
         USING GETTXTD,RF                                                       
         MVC   GTMSGNO,ERRNUM      ERROR MESSAGE NUMBER                         
         MVC   GTMTYP,GTMERR       ERROR TYPE                                   
         MVI   GTMSYS,2            SPOT SYSTEM                                  
         DROP  RF                                                               
MYERREX  GOTO1 ERREX                                                            
YES      SR    RC,RC                                                            
NO       LTR   RC,RC                                                            
XIT      XIT1                                                                   
         EJECT                                                                  
                                                                                
*---------------------------- STORAGE AREA ---------------------------*         
                                                                                
MYTWAL   EQU   *-CONHEADH                                                       
         SPACE 3                                                                
         LTORG                                                                  
         GETEL R6,DATADISP,ELCODE                                               
                                                                                
*---------------------------- OTHER DSECTS ---------------------------*         
                                                                                
         EJECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE SPSFMWORKD                                                     
       ++INCLUDE SPSFMFFD                                                       
         PRINT ON                                                               
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE SCSFM81D          LIST SCREEN                                  
         SPACE 2                                                                
         ORG   CONTAGH                                                          
       ++INCLUDE SCSFM85D          MAINTENANCE SCREEN                           
         EJECT                                                                  
                                                                                
* DDGENTWA                                                                      
* DDSPOOLD                                                                      
* DDSPLWORKD                                                                    
* FATIOB                                                                        
* FAGETTXTD                                                                     
* DDCOMFACS                                                                     
* DDGLOBEQUS                                                                    
* DDPERVALD                                                                     
* DDCOREQUS                                                                     
* SPDDEQUS                                                                      
* SPMSGEQUS                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDGENTWA                                                       
       ++INCLUDE DDSPOOLD          (GENERAL PRINT AREAS)                        
       ++INCLUDE DDSPLWORKD        (GENERAL CONTROLLER AREAS)                   
       ++INCLUDE FATIOB                                                         
       ++INCLUDE FAGETTXTD                                                      
       ++INCLUDE DDCOMFACS                                                      
       ++INCLUDE DDGLOBEQUS                                                     
       ++INCLUDE DDPERVALD                                                      
       ++INCLUDE DDCOREQUS                                                      
       ++INCLUDE SPDDEQUS                                                       
       ++INCLUDE SPMSGEQUS                                                      
         PRINT ON                                                               
       ++INCLUDE SPGENCTR          CONTRACTOR RECORD DSECT                      
       ++INCLUDE DDACTIVD          ACTIVITY ELEMENT DSECT                       
*---------------------------- SPGEN DSECTS ---------------------------*         
                                                                                
                                                                                
SYSD     DSECT                                                                  
         ORG   SYSSPARE                                                         
SAVEKEY  DS    XL13                                                             
MORE     DS    X                                                                
ERRNUM   DS    XL2                 ERROR MESSAGE NUMBER                         
MYSSPREL EQU   *-SYSSPARE                                                       
         DS    0CL(1024-MYSSPREL)  CHECK AGAINST AVAIL SYSSPARE AMT             
                                                                                
GEND     DSECT                                                                  
         ORG   LISTAR                                                           
LSTCNTR  DS    CL8                 CONTRACTOR (CODE)                            
         DS    C                                                                
LSTNAME  DS    CL40                CONTRACTOR NAME                              
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'002SPSFM05   03/14/02'                                      
         END                                                                    
