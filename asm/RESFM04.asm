*          DATA SET RESFM04    AT LEVEL 023 AS OF 05/01/02                      
*PHASE T81804A,*                                                                
         TITLE 'T81804 - PROGRAM TYPE RECORD'                                   
**********************************************************************          
*                                                                    *          
*        RESFM04 (T81804) --- PROGRAM TYPE RECORD                    *          
*                                                                    *          
* ------------------------------------------------------------------ *          
* UPDATE HISTORY:                                                    *          
*                                                                    *          
* ???????  (???) --- HISTORY LOST                                    *          
*                                                                    *          
* 08OCT90  (EFJ) --- TOMBSTONE ADDED, PHASE CARD CHANGED TO 'A'      *          
*                                                                    *          
**********************************************************************          
T81804   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**1803**                                                       
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     R8,ASPOOLD          GENERAL PRINT AREAS                          
         USING SPOOLD,R8                                                        
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA      BASE SCREEN FOR SYSTEM + THIS PROG           
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         SPACE                                                                  
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
         CLI   MODE,PRINTREP       PRINT LIST RECORDS                           
         BE    LIST                                                             
*                                                                               
XIT      XIT1                                                                   
         EJECT                                                                  
*              VALIDATE AND DISPLAY KEY                                         
         SPACE 3                                                                
VKEY     LA    R2,PGTPTCH                                                       
         XC    SVPGT,SVPGT                                                      
         CLI   ACTNUM,ACTLIST      IF ACTION LIST                               
         BE    VK20                                                             
         CLI   ACTNUM,ACTREP       OR ACTION REPORT THEN                        
         BNE   *+12                                                             
VK20     CLI   5(R2),0             OK IF BLANK                                  
         BE    VKEXT                                                            
*                                                                               
         LA    R4,KEY                                                           
         XC    KEY,KEY                                                          
         USING RPGTKEY,R4                                                       
         MVI   RPGTKTYP,X'25'                                                   
         MVC   RPGTKREP,AGENCY                                                  
         LA    R2,PGTPTCH          VALIDATE PROGRAM TYPE CODE                   
         GOTO1 ANY                                                              
         MVC   RPGTKPGT,8(R2)                                                   
         MVC   SVPGT,8(R2)                                                      
*                                                                               
VKEXT    B     XIT                                                              
         EJECT                                                                  
* DISPLAY KEY                                                                   
DKEY     LA    R4,KEY                                                           
         LA    R2,PGTPTCH          DISPLAY KEY (FOR SELECT)                     
         MVC   8(1,R2),RPGTKPGT                                                 
         OI    6(R2),X'80'                                                      
*                                                                               
         B     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
*              VALIDATE AND DISPLAY RECORD                                      
         SPACE 3                                                                
VREC     EQU   *                                                                
         L     R6,AIO1                                                          
         USING RPGTRECD,R6                                                      
         LA    R2,PGTPTNH                                                       
         GOTO1 ANY                                                              
         MVC   RPGTCODE(2),=X'0114'                                             
         MVC   RPGTNAME,8(R2)                                                   
*                                                                               
         B     XIT                                                              
         DROP  R6                                                               
         EJECT                                                                  
DREC     EQU   *                                                                
         L     R6,AIO1                                                          
         USING RPGTRECD,R6                                                      
         LA    R2,PGTPTNH                                                       
         MVC   8(12,R2),RPGTNAME                                                
         OI    6(R2),X'80'                                                      
*                                                                               
         B     XIT                                                              
         DROP  R6                                                               
         EJECT                                                                  
*              LIST RECORDS                                                     
         SPACE 3                                                                
LIST     EQU   *                                                                
         LA    R2,LPTSELH                                                       
         BAS   RE,CLRSCRN                                                       
         CLI   MODE,PRINTREP                                                    
         BNE   LS100                                                            
         LA    R1,HEADING                                                       
         ST    R1,SPECS                                                         
         LA    R1,HDRTN                                                         
         ST    R1,HEADHOOK                                                      
*                                                                               
LS100    LA    R4,KEY                                                           
         OC    KEY,KEY             WAS A KEY PROVIDED?                          
         BNZ   LS220                                                            
         USING RPGTKEY,R4                                                       
         MVI   RPGTKTYP,X'25'                                                   
         MVC   RPGTKREP,AGENCY     REP                                          
         CLI   MODE,PRINTREP                                                    
         BE    *+10                NO FILTERING FOR REPORT                      
         MVC   RPGTKPGT,SVPGT      PROGRAM TYPE                                 
*                                                                               
         GOTO1 HIGH                                                             
         B     LS220                                                            
         DROP  R4                                                               
*                                                                               
LS200    GOTO1 SEQ                                                              
LS220    CLC   KEY(26),KEYSAVE     CHECKMAIN C/B                                
         BNE   XIT                                                              
         GOTO1 GETREC                                                           
         MVC   LISTAR,SPACES       SHOW SCREEN                                  
         LA    R3,LISTAR                                                        
         USING LISTD,R3                                                         
         L     R6,AIO                                                           
         USING RPGTRECD,R6                                                      
         MVC   LISTCDE,RPGTKPGT                                                 
         MVC   LISTNAM,RPGTNAME                                                 
*                                                                               
LS400    CLI   MODE,PRINTREP                                                    
         BE    LS500                                                            
         SPACE                                                                  
         MVC   DMDSKADD,KEY+28                                                  
         GOTO1 LISTMON                                                          
         B     LS200                                                            
         SPACE                                                                  
LS500    MVC   P(46),LISTAR                                                     
         GOTO1 SPOOL,DMCB,(R8)                                                  
         B     LS200                                                            
         DROP  R6,R3                                                            
         EJECT                                                                  
CLRSCRN  NTR1                                                                   
*                                                                               
*        ROUTINE TO CLEAR THE SCREEN                                            
*        FROM FIELD AT R2                                                       
*                                                                               
         SR    RE,RE                                                            
*                                                                               
CS010    IC    RE,0(R2)                                                         
         SH    RE,=H'9'                                                         
         EX    RE,CSCLC                                                         
         BE    CS020                                                            
         EX    RE,CSOC                                                          
         BZ    CS020                                                            
         EX    RE,CSXC                                                          
         OI    6(R2),X'80'                                                      
*                                                                               
CS020    LA    R2,9(RE,R2)                                                      
         CLI   0(R2),9                                                          
         BH    CS010                                                            
         B     XIT                                                              
*                                                                               
CSCLC    CLC   8(0,R2),SPACES                                                   
CSOC     OC    8(0,R2),8(R2)                                                    
CSXC     XC    8(0,R2),8(R2)                                                    
         EJECT                                                                  
HEADING  DS    0H                                                               
         SSPEC H1,1,REQUESTOR                                                   
         SSPEC H2,1,AGYNAME                                                     
         SSPEC H1,45,C'PROGRAM TYPE RECORDS'                                    
         SSPEC H2,45,C'--------------------'                                    
         SSPEC H1,93,RUN                                                        
         SSPEC H2,93,REPORT                                                     
         SSPEC H2,109,PAGE                                                      
         DC    X'00'                                                            
*                                                                               
HDRTN    NTR1                                                                   
         LA    R3,H6                                                            
         MVC   0(29,R3),LPTHDS                                                  
         LA    R3,H7                                                            
         MVC   0(7,R3),DASH                                                     
         MVC   9(12,R3),DASH                                                    
         B     XIT                                                              
         EJECT                                                                  
         GETEL R6,DATADISP,ELCODE                                               
         SPACE                                                                  
TRAPERR  GOTO1 ERREX                                                            
         SPACE 3                                                                
RELO     DS    A                                                                
DASH     DC    47C'-'                                                           
         EJECT                                                                  
         PRINT GEN                                                              
LISTD    DSECT                                                                  
LISTCDE  DS    CL1                                                              
         DS    CL8                                                              
LISTNAM  DS    CL12                                                             
         EJECT                                                                  
* DDSPOOLD                                                                      
* DDSPLWORKD                                                                    
* RESFMFFD                                                                      
         PRINT OFF                                                              
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE RESFMFFD                                                       
         SPACE 2                                                                
         PRINT ON                                                               
         ORG   CONTAGH                                                          
* RESFMF4D                                                                      
       ++INCLUDE RESFMF4D                                                       
         ORG   CONTAGH                                                          
* RESFME4D                                                                      
       ++INCLUDE RESFME4D                                                       
         EJECT                                                                  
* REGENPGT                                                                      
       ++INCLUDE REGENPGT                                                       
         EJECT                                                                  
* RESFMWORKD                                                                    
         SPACE 2                                                                
         PRINT OFF                                                              
       ++INCLUDE RESFMWORKD                                                     
         PRINT ON                                                               
         ORG   SYSSPARE                                                         
SVPGT    DS    CL1                                                              
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'023RESFM04   05/01/02'                                      
         END                                                                    
