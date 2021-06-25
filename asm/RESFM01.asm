*          DATA SET RESFM01    AT LEVEL 007 AS OF 10/08/90                      
*PHASE T81801A,*                                                                
         TITLE 'T81801 - OVERNIGHT UPLOAD'                                      
**********************************************************************          
*                                                                    *          
*        RESFM01 (T81801) --- OVERNIGHT UPLOAD                       *          
*                                                                    *          
* ------------------------------------------------------------------ *          
* UPDATE HISTORY:                                                    *          
*                                                                    *          
* ???????  (???) --- HISTORY LOST                                    *          
*                                                                    *          
* 08OCT90  (EFJ) --- TOMBSTONE ADDED, PHASE CARD CHANGED TO 'A'      *          
*                                                                    *          
**********************************************************************          
T81801   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**1801**                                                       
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
         CLI   MODE,VALREC         VALIDATE RECORD                              
         BE    VREC                                                             
         CLI   MODE,DISPKEY        DISPLAY KEY                                  
         BE    DKEY                                                             
         CLI   MODE,DISPREC        DISPLAY RECORD                               
         BE    DREC                                                             
         CLI   MODE,LISTRECS       LIST RECORDS                                 
         BE    LIST                                                             
         B     XIT                                                              
         EJECT                                                                  
*              VALIDATE AND DISPLAY KEY                                         
         SPACE 3                                                                
VKEY     LA    R2,OVLFLGH                                                       
         MVI   OVLFLG,C'0'         ASSUME ERROR                                 
         OI    6(R2),X'80'                                                      
         LA    R4,KEY                                                           
         XC    KEY,KEY                                                          
         USING ROVRKEY,R4                                                       
         MVI   ROVRKTYP,X'22'                                                   
         MVC   ROVRKREP,AGENCY                                                  
         LA    R2,OVLRPTH          VALIDATE REPORT                              
         GOTO1 VALIRPT                                                          
         MVC   ROVRKRPT,WORK                                                    
*                                                                               
         LA    R2,OVLSVCH          VALIDATE SERVICE                             
         GOTO1 VALISVC                                                          
         MVC   ROVRKSVC,WORK                                                    
*                                                                               
         LA    R2,OVLMKTH          OBTAIN MARKET                                
         GOTO1 ANY                                                              
         MVC   ROVRKMKT,WORK                                                    
*                                                                               
         CLI   ACTNUM,ACTLIST      IF NOT ACTION LIST                           
         BE    VKEY20                                                           
         LA    R2,OVRSCNH          VALIDATE SCREEN                              
         GOTO1 ANY                                                              
         MVI   ERROR,2                                                          
         CLI   5(R2),4             MUST BE 4 NUMBERS                            
         BNE   TRAPERR                                                          
         MVI   ERROR,3                                                          
         TM    4(R2),X'08'         MUST BE NUMERIC                              
         BZ    TRAPERR                                                          
         MVC   ROVRKSCN,WORK                                                    
*                                                                               
VKEY20   MVI   OVLFLG,C'1'         NO ERRORS                                    
         B     XIT                                                              
         SPACE 1                                                                
DKEY     LA    R4,KEY              DISPLAY KEY (FOR SELECT)                     
         LA    R2,OVRRPTH          REPORT                                       
         MVC   8(2,R2),ROVRKRPT                                                 
         OI    6(R2),X'80'                                                      
*                                                                               
         LA    R2,OVRSVCH          SERVICE                                      
         MVC   8(3,R2),ROVRKSVC                                                 
         OI    6(R2),X'80'                                                      
*                                                                               
         LA    R2,OVRMKTH          MARKET                                       
         MVC   8(3,R2),ROVRKMKT                                                 
         OI    6(R2),X'80'                                                      
*                                                                               
         LA    R2,OVRSCNH          SCREEN NUMBER                                
         MVC   8(4,R2),ROVRKSCN                                                 
         OI    6(R2),X'80'                                                      
*                                                                               
         B     XIT                                                              
         EJECT                                                                  
*              VALIDATE AND DISPLAY RECORD                                      
         SPACE 3                                                                
VREC     LA    R2,OVRFLGH                                                       
         MVI   OVRFLG,C'0'         ASSUME ERROR                                 
         OI    6(R2),X'80'                                                      
         SPACE 1                                                                
DREC     LA    R2,OVRTXTH          DISPLAY                                      
DREC20   MVI   ELCODE,X'02'                                                     
         MVI   MAX,12                                                           
         GOTO1 VALITXT                                                          
         MVI   OVRFLG,C'1'         NO ERRORS                                    
         B     XIT                                                              
         EJECT                                                                  
*              LIST RECORDS                                                     
         SPACE 3                                                                
LIST     LA    R5,OVLHED           FILL IN SCREEN HEADINGS                      
         MVC   0(3,R5),=C'SEL'                                                  
         MVC   8(6,R5),=C'SCREEN'                                               
         LA    R5,18(R5)                                                        
         MVC   0(4,R5),=C'DATE'                                                 
         LA    R5,12(R5)                                                        
         MVC   0(4,R5),=C'TIME'                                                 
         OI    OVLHEDH+6,X'80'                                                  
         SPACE 1                                                                
         LA    R4,KEY                                                           
         OC    KEY,KEY             WAS A KEY PROVIDED?                          
         BNZ   LIST40                                                           
         USING ROVRKEY,R4                                                       
         MVI   ROVRKTYP,X'22'                                                   
         MVC   ROVRKREP,AGENCY     REP                                          
         MVC   ROVRKRPT,OVLRPT     REPORT                                       
         MVC   ROVRKSVC,OVLSVC     SERVICE                                      
         MVC   ROVRKMKT,OVLMKT     MARKET                                       
*                                                                               
         GOTO1 HIGH                                                             
         CLC   KEY(23),KEYSAVE     MAKE SURE WE FOUND ONE                       
         BE    LIST40                                                           
*                                                                               
         MVI   OVLFLG,C'0'         INDICATE ERROR                               
         LA    R2,OVLMKTH          IN MARKET FIELD                              
         MVI   ERROR,2             INVALID INPUT FIELD                          
         B     TRAPERR                                                          
         DROP  R4                                                               
         SPACE 1                                                                
LIST20   GOTO1 SEQ                                                              
         SPACE 1                                                                
LIST40   CLC   KEY(23),KEYSAVE     CHECKMAIN C/B                                
         BNE   XIT                                                              
         GOTO1 GETREC                                                           
         MVC   LISTAR,SPACES       SHOW SCREEN                                  
         LA    R3,LISTAR                                                        
         L     R6,AIO                                                           
         USING ROVRREC,R6                                                       
         MVC   0(4,R3),ROVRKSCN                                                 
         SPACE 1                                                                
         LA    R3,10(R3)           DATE                                         
         OC    ROVRDATE,ROVRDATE                                                
         BZ    LIST50                                                           
         GOTO1 DATCON,DMCB,(3,ROVRDATE),(8,0(R3))                               
         SPACE 1                                                                
         LA    R3,12(R3)           TIME                                         
         UNPK  DUB,ROVRTIME                                                     
         MVC   0(2,R3),DUB+2                                                    
         MVI   2(R3),C'.'                                                       
         MVC   3(2,R3),DUB+4                                                    
         SPACE 1                                                                
LIST50   GOTO1 LISTMON                                                          
         B     LIST20                                                           
         DROP  R6                                                               
         EJECT                                                                  
BUMP     ZIC   R1,0(R2)                                                         
         AR    R2,R1                                                            
         BR    RE                                                               
         SPACE 1                                                                
XIT      XIT1                                                                   
         SPACE 1                                                                
         GETEL R6,DATADISP,ELCODE                                               
         SPACE 1                                                                
TRAPERR  GOTO1 ERREX                                                            
         B     XIT                                                              
         SPACE 1                                                                
SPLAT    NTR1                                                                   
         GOTO1 SPOOL,DMCB,(R8)                                                  
         B     XIT                                                              
         SPACE 3                                                                
RELO     DS    A                                                                
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
* RESFMF1D                                                                      
       ++INCLUDE RESFMF1D                                                       
         ORG   CONTAGH                                                          
* RESFME1D                                                                      
       ++INCLUDE RESFME1D                                                       
         EJECT                                                                  
* REGENOVR                                                                      
       ++INCLUDE REGENOVR                                                       
         EJECT                                                                  
*RESFMWORKD                                                                     
         PRINT OFF                                                              
       ++INCLUDE RESFMWORKD                                                     
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'007RESFM01   10/08/90'                                      
         END                                                                    
