*          DATA SET RESFM02    AT LEVEL 017 AS OF 05/01/02                      
*PHASE T81802A,*                                                                
         TITLE 'T81802 - OVERNIGHT UPLOAD - USER ACTION'                        
**********************************************************************          
*                                                                    *          
*        RESFM02 (T81802) --- OVERNIGHT UPLOAD - USER ACTION         *          
*                                                                    *          
* ------------------------------------------------------------------ *          
* UPDATE HISTORY:                                                    *          
*                                                                    *          
* ???????  (???) --- HISTORY LOST                                    *          
*                                                                    *          
* 08OCT90  (EFJ) --- TOMBSTONE ADDED, PHASE CARD CHANGED TO 'A'      *          
*                                                                    *          
* 04JAN91  (EFJ) --- FIX LIST TO PROPERLY DEAL WITH DELETED RECS     *          
*                     CREATED BY OVAD                                *          
*                                                                    *          
**********************************************************************          
T81802   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**1802**                                                       
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     R8,ASPOOLD          GENERAL PRINT AREAS                          
         USING SPOOLD,R8                                                        
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA      BASE SCREEN FOR SYSTEM + THIS PROG           
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         SPACE 1                                                                
         CLI   MODE,VALKEY         VALIDATE RECORD KEY                          
         BE    VKEY                                                             
         CLI   MODE,LISTRECS       LIST RECORDS                                 
         BE    LIST                                                             
         CLI   MODE,PRINTREP       PRINT REPORT                                 
         BNE   CHKMODE                                                          
         LA    R1,HEDSPECS                                                      
         ST    R1,SPECS                                                         
         LA    R1,HOOK                                                          
         ST    R1,HEADHOOK                                                      
         B     REP                                                              
*                                                                               
CHKMODE  LA    R3,LISTDIR                                                       
         ZIC   R0,LISTNUM                                                       
         LTR   R0,R0                                                            
         BZ    XIT                                                              
         SR    R4,R4                                                            
CHK10    CLI   0(R3),C' '                                                       
         BNH   CHK20                                                            
         CLI   0(R3),C'P'                                                       
         BNE   BADSELCT                                                         
CHK20    LA    R3,6(R3)                                                         
         LA    R4,1(R4)                                                         
         BCT   R0,CHK10                                                         
         B     XIT                                                              
         SPACE 1                                                                
BADSELCT MVC   CONHEAD(L'BADSEL),BADSEL    USE MY OWN ERROR MSG                 
         MVI   0(R3),0             CLEAR OUT ERROR FROM LISTDIR                 
         LA    R2,OVUSELH          POINT CURSOR TO FIELD WITH ERROR             
         LTR   R4,R4                                                            
         BZ    MYEND               ERROR IS IN FIRST FIELD                      
         SPACE 1                                                                
BAD10    ZIC   R0,0(R2)            BUMP TO NEXT SELECT FIELD                    
         AR    R2,R0                                                            
         IC    R0,0(R2)                                                         
         AR    R2,R0                                                            
         BCT   R4,BAD10                                                         
         B     MYEND                                                            
         EJECT                                                                  
*              VALIDATE KEY                                                     
         SPACE 3                                                                
VKEY     LA    R4,KEY                                                           
         XC    KEY,KEY                                                          
         USING ROVRKEY,R4                                                       
         MVI   ROVRKTYP,X'22'                                                   
         MVC   ROVRKREP,AGENCY                                                  
         LA    R2,OVURPTH          VALIDATE REPORT                              
         GOTO1 VALIRPT                                                          
         MVC   ROVRKRPT,WORK                                                    
*                                                                               
         LA    R2,OVUSVCH          VALIDATE SERVICE                             
         GOTO1 VALISVC                                                          
         MVC   ROVRKSVC,WORK                                                    
*                                                                               
         B     XIT                                                              
         SPACE 1                                                                
         EJECT                                                                  
*              LIST RECORDS                                                     
         SPACE 3                                                                
LIST     LA    R4,KEY                                                           
         OC    KEY,KEY             WAS A KEY PROVIDED?                          
         BNZ   LIST40                                                           
         USING ROVRKEY,R4                                                       
         MVI   ROVRKTYP,X'22'                                                   
         MVC   ROVRKREP,AGENCY     REP                                          
         MVC   ROVRKRPT,OVURPT     REPORT                                       
         MVC   ROVRKSVC,OVUSVC     SERVICE                                      
*                                                                               
         GOTO1 HIGH                                                             
         B     LIST40                                                           
         DROP  R4                                                               
         SPACE 1                                                                
LIST20   GOTO1 SEQ                                                              
         SPACE 1                                                                
LIST40   CLC   KEY(20),KEYSAVE     CHECKMAIN C/B                                
         BNE   XIT                                                              
         CLC   KEY+23(4),=C'0001'  ONLY WANT 1ST SCREEN                         
         BE    LIST60                                                           
         MVC   KEY+20(3),=X'FFFFFF'   SKIP TO NEXT MARKET                       
         B     LIST20                                                           
LIST60   OI    DMINBTS,X'08'       PASS DELETES                                 
         GOTO1 GETREC                                                           
         NI    DMINBTS,X'FF'-X'08'                                              
         MVC   LISTAR,SPACES                                                    
         LA    R3,LISTAR                                                        
         LA    R5,OVUHED                                                        
         L     R6,AIO                                                           
         USING ROVRREC,R6                                                       
*                                                                               
* SEE IF REC DELETED - IF SO, SKIP                                              
         TM    ROVRCNTL,X'80'      DELETED?                                     
         BNZ   LIST20                                                           
         OI    OVUHEDH+6,X'80'                                                  
         MVC   0(3,R5),=C'SEL'                                                  
         SPACE 1                                                                
         MVC   0(3,R3),ROVRKMKT    MARKET                                       
         MVC   8(6,R5),=C'MARKET'                                               
         SPACE 1                                                                
         LA    R3,10(R3)           DATE                                         
         LA    R5,18(R5)                                                        
         OC    ROVRDATE,ROVRDATE                                                
         BZ    LIST65                                                           
         GOTO1 DATCON,DMCB,(3,ROVRDATE),(8,0(R3))                               
         MVC   0(4,R5),=C'DATE'                                                 
         SPACE 1                                                                
         LA    R3,12(R3)           TIME                                         
         LA    R5,12(R5)                                                        
         UNPK  DUB,ROVRTIME                                                     
         MVC   0(2,R3),DUB+2                                                    
         MVI   2(R3),C'.'                                                       
         MVC   3(2,R3),DUB+4                                                    
         MVC   0(4,R5),=C'TIME'                                                 
         SPACE 1                                                                
LIST65   DS    0H                                                               
         GOTO1 LISTMON                                                          
         B     LIST20                                                           
         DROP  R6                                                               
         EJECT                                                                  
*  PRINT REPORT                                                                 
         SPACE 2                                                                
REP      DS    0H'0'                                                            
         MVI   FORCEHED,C'Y'                                                    
REP10    L     R6,AIO                                                           
         MVI   ELCODE,X'02'                                                     
         BAS   RE,GETEL                                                         
         BE    REP18                                                            
         DC    H'0'                                                             
         SPACE 1                                                                
REP15    BAS   RE,NEXTEL                                                        
         BNE   REP20                                                            
         SPACE 1                                                                
         USING ROVRTXEL,R6                                                      
REP18    CLI   ROVRTXL,C'X'          INDICATES END OF REPORT                    
         BE    XIT                                                              
         CLI   ROVRTXL,C'1'          INDICATES NEW PAGE                         
         BNE   *+8                                                              
         MVI   FORCEHED,C'Y'                                                    
         MVC   P(66),ROVRTXL+1     1ST HALF OF LINE                             
         MVC   P+66(66),ROVRTXR+1  2ND HALF OF LINE                             
         GOTO1 SPOOL,DMCB,(R8)                                                  
         B     REP15                                                            
         DROP  R6                                                               
         SPACE 1                                                                
REP20    LA    R4,KEY                                                           
         OC    KEY,KEY             KEY SHOULD HAVE BEEN PROVIDED                
         BNZ   *+6                                                              
         DC    H'0'                                                             
         GOTO1 SEQ                                                              
         CLC   KEY(23),KEYSAVE     GET NEXT SCREEN FOR THIS MKT                 
         BNE   XIT                                                              
         GOTO1 GETREC                                                           
         B     REP10                                                            
         EJECT                                                                  
BUMP     ZIC   R1,0(R2)                                                         
         AR    R2,R1                                                            
         BR    RE                                                               
         SPACE 1                                                                
XIT      XIT1                                                                   
         EJECT                                                                  
*              HEADLINE ROUTINES                                                
         SPACE 3                                                                
HOOK     NTR1                                                                   
         B     XIT                                                              
         EJECT                                                                  
         GETEL R6,DATADISP,ELCODE                                               
         SPACE 2                                                                
SPLAT    NTR1                                                                   
         GOTO1 SPOOL,DMCB,(R8)                                                  
         B     XIT                                                              
         SPACE 2                                                                
MYEND    MVI   ERROR,X'FE'         USING MY OWN ERROR MSG                       
         MVI   GENSTAT2,USMYOK                                                  
         GOTO1 ERREX2                                                           
         SPACE 2                                                                
TRAPERR  GOTO1 ERREX                                                            
         SPACE 2                                                                
*   MY OWN ERROR MESSAGE                                                        
BADSEL   DC    C'INVALID INPUT -- HIT ENTER TO CONTINUE'                        
         SPACE 2                                                                
RELO     DS    A                                                                
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
***********                                                                     
*******THIS PROGRAM DOESN'T USE ANY HEDSPECS******                              
***********                                                                     
         SPACE 2                                                                
HEDSPECS DC    X'00'               END MARKER FOR SPECS                         
         SPACE 1                                                                
         EJECT                                                                  
* DDSPOOLD                                                                      
* DDSPLWORKD                                                                    
* RESFMFFD                                                                      
         PRINT OFF                                                              
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE RESFMFFD                                                       
         PRINT ON                                                               
         ORG   CONTAGH                                                          
* RESFME2D                                                                      
       ++INCLUDE RESFME2D                                                       
         EJECT                                                                  
* DDGENTWA                                                                      
       ++INCLUDE DDGENTWA                                                       
         EJECT                                                                  
* REGENOVR                                                                      
         DSECT                                                                  
       ++INCLUDE REGENOVR                                                       
         EJECT                                                                  
*RESFMWORKD                                                                     
         PRINT OFF                                                              
       ++INCLUDE RESFMWORKD                                                     
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'017RESFM02   05/01/02'                                      
         END                                                                    
