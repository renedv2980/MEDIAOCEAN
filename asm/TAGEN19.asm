*          DATA SET TAGEN19    AT LEVEL 015 AS OF 05/01/02                      
*PHASE T70219A                                                                  
         TITLE 'T70219 - HISTORY COMMENT ADD'                                   
T70219   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T70219                                                         
         L     RC,0(R1)            RC=GENCON STORAGE AREA                       
         USING GEND,RC                                                          
         L     RA,ATWA             RA=A(TWA)                                    
         USING T702FFD,RA                                                       
         L     R9,ASYSD            R9=ROOT STORAGE AREA                         
         USING SYSD,R9                                                          
         L     R8,ASPOOLD          R8=SPOOL DSECT                               
         USING SPOOLD,R8                                                        
         EJECT                                                                  
*              MODE CONTROLLED ROUTINES FOR HISTORY ADD                         
         GOTO1 INITIAL,DMCB,0      INITIALIZE                                   
         SPACE 1                                                                
         CLI   MODE,VALKEY                                                      
         BNE   HCOM10                                                           
         CLI   ACTNUM,ACTDEL       DON'T ALLOW DELETE                           
         BE    ACTERR                                                           
         GOTO1 RECVAL,DMCB,TLAYCDQ,(X'08',SHCAGYH),SHCAGYNH   AGENCY            
         GOTO1 RECVAL,DMCB,TLCOICDQ,(X'08',SHCCIDH),SHCCOMNH  CID               
         SPACE 1                                                                
         USING TLCOD,R3                                                         
         L     R3,AIO                                                           
         MVC   TGCOM,TLCOCOM                                                    
         SPACE 1                                                                
         LA    R2,SHCINVH                                                       
         CLI   5(R2),0             IF DON'T HAVE INVOICE INPUT                  
         BNE   HCOM2                                                            
         TM    SCRSTAT,SCRCHG      AND IF FIRST TIME IN                         
         BZ    HCOM4                                                            
         B     PLSENTER            GIVE USER A CHANCE TO INPUT                  
         SPACE                                                                  
HCOM2    GOTO1 TINVCON,DMCB,SHCINV,TGINV,DATCON CONVERT INVOICE FOR KEY         
         CLI   0(R1),X'FF'                                                      
         BE    FLDINV                                                           
         XC    TGINV,ALLFFS        COMPLEMENT INVOICE NUMBER                    
         SPACE                                                                  
         USING TLINPD,R3                                                        
         LA    R3,KEY                                                           
         XC    KEY,KEY                                                          
         MVI   TLINPCD,TLINHCDQ    LOOK UP PAYMENT HISTORY PASSIVE              
         MVC   TLINHCOM,TGCOM                                                   
         MVC   TLINHINV,TGINV                                                   
         GOTO1 HIGH                                                             
         CLC   KEY(TLINHSEQ-TLINPD),KEYSAVE  COMPARE UPTO INVOICE NUM           
         BNE   NTFOUND                                                          
         B     HCOM6                                                            
         SPACE                                                                  
HCOM4    LA    R3,KEY              NO INOVICE INPUT                             
         XC    KEY,KEY                                                          
         MVI   TLINPCD,TLINHCDQ    LOOK UP PAYMENT HISTORY PASSIVE              
         MVC   TLINHCOM,TGCOM                                                   
         GOTO1 HIGH                                                             
         CLC   KEY(TLINHINV-TLINPD),KEYSAVE  COMPARE UPTO INT COMM NUM          
         BE    HCOM6                                                            
         SPACE                                                                  
         USING TLHCD,R4            NO PAYMENTS OR HISTORY COMMENTS YET          
         XC    WORK,WORK           BUILD KEY OF RECORD TO ADD IN WORK           
         LA    R4,WORK                                                          
         MVI   TLHCCD,TLHCCDQ                                                   
         MVC   TLHCCOM,TGCOM                                                    
         MVC   TLHCINV,ALLFFS                                                   
         MVI   TLHCSEQ,X'80'       SET COMPLEMENTED SEQ TO ADD = X'80'          
         B     HCOM8                                                            
         SPACE                                                                  
         USING TLHCD,R4                                                         
HCOM6    XC    WORK,WORK           BUILD KEY OF RECORD TO ADD IN WORK           
         LA    R4,WORK                                                          
         MVI   TLHCCD,TLHCCDQ                                                   
         MVC   TLHCCOM,TLINHCOM                                                 
         MVC   TLHCINV,TLINHINV                                                 
         CLI   TLINHSEQ,X'80'      IF COMPL SEQ OF EXISTING REC > X'80'         
         BNH   *+12                                                             
         MVI   TLHCSEQ,X'80'       SET COMPLEMENTED SEQ TO ADD = X'80'          
         B     HCOM8                                                            
         SPACE                                                                  
         ZIC   R0,TLINHSEQ         ELSE COMPL SEQ OF EXISTING REC               
         BCTR  R0,0                - 1                                          
         STC   R0,TLHCSEQ          = COMPLEMENTED SEQ TO ADD                    
         SPACE                                                                  
HCOM8    MVC   KEY,WORK            SET KEY OF RECORD TO ADD                     
         B     XIT                                                              
         EJECT                                                                  
HCOM10   CLI   MODE,DISPREC                                                     
         BE    HCOM12                                                           
         CLI   MODE,XRECADD        IF MODE IS NEW RECORD ADDED                  
         BNE   HCOM20                                                           
         GOTO1 ADDPTRS,DMCB,PTRS   ADD PASSIVE PTRS                             
         SPACE                                                                  
HCOM12   BAS   RE,DISPLAY          (RE)DISPLAY RECORD                           
         B     XIT                                                              
         SPACE                                                                  
HCOM20   CLI   MODE,VALREC         TEST MODE IS VALIDATE RECORD                 
         BNE   XIT                                                              
         GOTO1 SAVPTRS,DMCB,PTRS   SAVE PASSIVE PTRS                            
         BAS   RE,BLDREC           BUILD RECORD                                 
         B     XIT                                                              
         EJECT                                                                  
*              DISPLAY THE RECORD                                               
         SPACE 1                                                                
DISPLAY  NTR1                                                                   
         GOTO1 CHAROUT,DMCB,TACMELQ,SHCHCOMH,TACMTYPH                           
         B     XIT                                                              
         EJECT                                                                  
*              BUILD THE RECORD                                                 
         SPACE 1                                                                
BLDREC   NTR1                                                                   
         GOTO1 NAMIN,DMCB,TACMELQ,SHCHCOMH,TACMTYPH  HIST COMMENT REQD          
         B     XIT                                                              
         EJECT                                                                  
*              ERROR/EXIT ROUTINES                                              
         SPACE 1                                                                
FLDINV   MVI   ERROR,INVALID       INVALID INPUT FIELD                          
         B     THEEND                                                           
         SPACE 1                                                                
FLDMISS  MVI   ERROR,MISSING       MISSING INPUT FIELD                          
         B     THEEND                                                           
         SPACE 1                                                                
NTFOUND  MVI   ERROR,NOTFOUND      RECORD NOT FOUND                             
         B     THEEND                                                           
         SPACE 1                                                                
PLSENTER MVI   MYMSGNO1,2          PLEASE ENTER FIELDS AS REQUIRED              
         MVI   MYMSYS,X'FF'                                                     
         OI    GENSTAT2,USGETTXT                                                
         B     THEEND                                                           
         SPACE 1                                                                
ACTERR   MVI   ERROR,INVRCACT      RECORD/ACTION COMBINATION INVALID            
         LA    R2,CONACTH                                                       
         B     THEEND                                                           
         SPACE 1                                                                
RECEND   LA    R2,CONRECH                                                       
INFEND   OI    GENSTAT2,USGETTXT                                                
THEEND   GOTO1 EXIT,DMCB,0                                                      
         SPACE 1                                                                
YES      XR    RC,RC                                                            
NO       LTR   RC,RC                                                            
XIT      XIT1                                                                   
         SPACE 2                                                                
         GETEL R4,DATADISP,ELCODE                                               
         EJECT                                                                  
*              CONSTANTS, ETC.                                                  
         SPACE 1                                                                
ALLFFS   DC    6X'FF'              SHOULD BE SAME LENGTH AS LONGEST             
*                                  FIELD BEING COMPLEMENTED                     
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
       ++INCLUDE TAGENFFD                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE TASCR19D                                                       
         SPACE 3                                                                
         ORG   SHCWORK                                                          
INV      DS    CL6                 INVOICE NUM WORK SPACE                       
PTRS     DS    CL(L'TLDRREC*2+1)   SAVED ACTIVE AND X'91' PASSIVE               
         EJECT                                                                  
* DDGENTWA                                                                      
* TASYSDSECT                                                                    
* TAGENWORKD                                                                    
* TAGENFILE                                                                     
* TAGENEQUS                                                                     
* TASYSEQUS                                                                     
* DDSPOOLD                                                                      
* DDSPLWORKD                                                                    
         PRINT OFF                                                              
       ++INCLUDE DDGENTWA                                                       
       ++INCLUDE TASYSDSECT                                                     
       ++INCLUDE TAGENWORKD                                                     
       ++INCLUDE TAGENFILE                                                      
       ++INCLUDE TAGENEQUS                                                      
       ++INCLUDE TASYSEQUS                                                      
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'015TAGEN19   05/01/02'                                      
         END                                                                    
