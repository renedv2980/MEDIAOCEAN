*          DATA SET RESFM21    AT LEVEL 004 AS OF 08/12/92                      
*PHASE T81821A,*                                                                
         TITLE 'T81821 - OVERNIGHT RECORD ADD/DELETE'                           
**********************************************************************          
*                                                                    *          
*        RESFM21 (T81821) --- OVERNIGHT REC ADD DELETE               *          
*                                                                    *          
* ------------------------------------------------------------------ *          
* UPDATE HISTORY:                                                    *          
*                                                                    *          
* 17DEC90  (EFJ) --- INITIAL DEVELOPMENT                             *          
*                                                                    *          
* 12AUG92  (EFJ) --- FIX BUG - ONLY DELETING 1ST REC (IT TOOK 2      *          
*                    YEARS TO NOTICE THIS???)                        *          
*                                                                    *          
**********************************************************************          
T81821   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**1821**                                                       
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     R8,ASPOOLD          GENERAL PRINT AREAS                          
         USING SPOOLD,R8                                                        
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA      BASE SCREEN FOR SYSTEM + THIS PROG           
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         L     R7,AIO1                                                          
         ST    R7,AIO                                                           
         USING ROVRREC,R7                                                       
         OI    GENSTAT1,RDUPAPPL   APPL CONTROLS RD FOR UPDATE                  
         MVI   IOOPT,C'Y'          MAINT PHASE WILL HANDLE I/O                  
*                                                                               
* MAKE SURE DDS TERMINAL                                                        
         CLI   DDS,C'Y'                                                         
         BE    *+18                                                             
         LA    R2,CONACTH                                                       
         MVC   RERROR,=AL2(SECLOCK)                                             
         B     ERREND                                                           
*                                                                               
         CLI   MODE,VALKEY         VALIDATE RECORD KEY                          
         BE    VKEY                                                             
EXXMOD   XMOD1                     EVERYTHING IN VALKEY                         
*                                                                               
EXIT     XIT1                                                                   
*                                                                               
ERREND   GOTO1 MYERROR             DO A GETTXT CALL                             
         EJECT                                                                  
*                                                                               
*  VALIDATE KEY                                                                 
VKEY     DS    0H                                                               
         MVC   RERROR,=AL2(INVALID)                                             
         LA    R2,OVCMKTH                                                       
         CLI   5(R2),3             MARKET IS 3 CHARS                            
         BNE   ERREND                                                           
         LA    R2,OVCSVCH                                                       
         CLC   =C'NSI',8(R2)                                                    
         BE    *+14                                                             
         CLC   =C'ARB',8(R2)                                                    
         BNE   ERREND                                                           
*                                                                               
* AT LEAST ONE FIELD OF REPORT SELECT MUST BE ENTERED                           
* WILL TAKE FIRST REPORT SELECTED                                               
         LA    R1,9                FOR BCT (MO,TU,WE,TH,FR,SA,SU,OT,RA)         
         SR    RE,RE                                                            
         LA    R2,OVC1STH          FIRST LABEL FIELD                            
VK10     DS    0H                                                               
         MVC   REPORT,8(R2)                                                     
         IC    RE,0(R2)                                                         
         AR    R2,RE               R2 TO SELECT FIELD                           
         CLI   5(R2),0             ANY INPUT?                                   
         BE    VK20                                                             
         CLI   8(R2),C'S'                                                       
         BE    VK100               GO ADD RECS FOR THIS REPORT                  
         B     ERREND                                                           
VK20     IC    RE,0(R2)                                                         
         AR    R2,RE               R2 TO NEXT REPORT FIELD                      
         BCT   R1,VK10                                                          
         LA    R2,OVCMOH           FIRST LABEL FIELD                            
         B     ERREND                                                           
*                                                                               
* NOW EITHER ADD OR DELETE RECS                                                 
VK100    DS    0H                                                               
         MVI   8(R2),C' '          BLANK OUT SELECT FIELD                       
         OI    6(R2),X'80'         XMIT                                         
         CLI   ACTNUM,ACTADD                                                    
         BE    OVADD                                                            
         CLI   ACTNUM,ACTDEL                                                    
         BE    OVDEL                                                            
         LA    R2,CONACTH                                                       
         B     ERREND              ONLY VALID ACTIONS                           
         EJECT                                                                  
OVADD    DS    0H                                                               
*                                                                               
* BUILD REC                                                                     
         XCEF  ROVRREC,1000                                                     
         MVI   ROVRKTYP,X'22'                                                   
         MVC   ROVRKREP,AGENCY                                                  
         MVC   ROVRKSVC,OVCSVC                                                  
         MVC   ROVRKMKT,OVCMKT                                                  
         MVC   ROVRKRPT,REPORT                                                  
         MVC   ROVRLEN,=X'036D'                                                 
*                                                                               
* LEAVE ROOM FOR X'01'                                                          
         MVC   ROVRCODE(2),=X'0114'                                             
*                                                                               
* BUILD 6 TEXT (X'02') ELEMENTS                                                 
         LA    R6,54(R7)                                                        
         USING ROVRTXEL,R6                                                      
         MVI   BYTE,1                                                           
         SR    RE,RE                                                            
OA10     MVC   ROVRTXCD(2),=X'0289'                                             
         MVC   ROVRTXSQ,BYTE                                                    
         MVC   ROVRTXL,SPACES                                                   
         MVC   ROVRTXR,SPACES                                                   
         LA    R6,137(R6)                                                       
         IC    RE,BYTE                                                          
         LA    RE,1(RE)                                                         
         STC   RE,BYTE                                                          
         CLI   BYTE,7                                                           
         BL    OA10                                                             
         DROP  R6                                                               
*                                                                               
         LA    R3,120              NUMBER OF SCREENS                            
         SR    R5,R5               SCREEN NUMBER                                
OA30     DS    0H                                                               
         LA    R5,1(R5)                                                         
         CVD   R5,DUB                                                           
         UNPK  ROVRKSCN,DUB                                                     
         OI    ROVRKSCN+3,X'F0'                                                 
         XC    KEY,KEY                                                          
         MVC   KEY(27),ROVRKEY                                                  
         MVI   RDUPDATE,C'Y'                                                    
         OI    DMINBTS,X'08'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(27),ROVRKEY                                                  
         BNE   OA100               REC DOESN'T EXIST                            
*                                                                               
* REC IS HERE, JUST MAKE SURE NOT DELETED                                       
         TM    KEY+27,X'80'        DELETED?                                     
         BZ    OA40                                                             
         NI    KEY+27,X'FF'-X'80'                                               
         GOTO1 WRITE               WRITE KEY BACK TO DIR                        
OA40     MVI   RDUPDATE,C'Y'                                                    
         OI    DMINBTS,X'08'                                                    
         MVC   AIO,AIO2            DON'T OVERWRITE 'NEW' REC                    
         L     R7,AIO                                                           
         GOTO1 GETREC                                                           
         TM    ROVRCNTL,X'80'                                                   
         BZ    OA50                                                             
         NI    ROVRCNTL,X'FF'-X'80'                                             
         GOTO1 PUTREC                                                           
OA50     MVC   AIO,AIO1                                                         
         L     R7,AIO                                                           
         B     OA120                                                            
OA100    GOTO1 ADDREC                                                           
OA120    BCT   R3,OA30                                                          
         B     EXXMOD                                                           
         EJECT                                                                  
*                                                                               
OVDEL    DS    0H                                                               
         MVC   RERROR,=AL2(NOTFOUND)                                            
         XC    KEY,KEY                                                          
         MVI   KEY,X'22'                                                        
         MVC   KEY+13(2),AGENCY                                                 
         MVC   KEY+15(2),REPORT                                                 
         MVC   KEY+17(3),OVCSVC                                                 
         MVC   KEY+20(3),OVCMKT                                                 
         MVC   KEY+23(4),=C'0001'  SCREEN 1 TO START                            
         MVI   RDUPDATE,C'Y'                                                    
         OI    DMINBTS,X'08'                                                    
         GOTO1 READ                                                             
         CLC   KEYSAVE(27),KEY                                                  
         BNE   ERREND              REC NOT FOUND                                
         LA    R3,120              NUMBER OF SCREENS                            
OV10     OI    KEY+27,X'80'        MARK DIR KEY AS DELETED                      
         GOTO1 WRITE                                                            
         MVI   RDUPDATE,C'Y'                                                    
         OI    DMINBTS,X'08'                                                    
         GOTO1 GETREC                                                           
         OI    ROVRCNTL,X'80'      TURN ON DELETE BIT                           
         GOTO1 PUTREC              DELETE REC                                   
         MVI   RDUPDATE,C'Y'                                                    
         OI    DMINBTS,X'08'                                                    
         GOTO1 SEQ                                                              
         CLC   KEY(23),ROVRKEY                                                  
         BNE   EXXMOD              IN CASE ONLY 100 SCREENS                     
         BCT   R3,OV10                                                          
         B     EXXMOD                                                           
         EJECT                                                                  
MKTTBL   DS    0CL2                                                             
         DC    C'MOTUWETHFRSASUOTRA'                                            
         DC    X'FF'                                                            
*                                                                               
REPORT   DS    CL2                 REPORT CODE (DAY)                            
         EJECT                                                                  
* DDSPOOLD                                                                      
* DDSPLWORKD                                                                    
* RESFMFFD                                                                      
         PRINT OFF                                                              
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE RESFMFFD                                                       
         PRINT ON                                                               
* RESFMB2D                                                                      
         ORG   CONTAGH                                                          
       ++INCLUDE RESFMB2D                                                       
         EJECT                                                                  
* REGENOVR                                                                      
       ++INCLUDE REGENOVR                                                       
         EJECT                                                                  
*RESFMWORKD                                                                     
         PRINT OFF                                                              
       ++INCLUDE RESFMWORKD                                                     
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'004RESFM21   08/12/92'                                      
         END                                                                    
