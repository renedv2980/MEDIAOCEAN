*          DATA SET RESFM03    AT LEVEL 032 AS OF 05/01/02                      
*PHASE T81803A,*                                                                
         TITLE 'T81803 - DAYPART RECORD'                                        
**********************************************************************          
*                                                                    *          
*        RESFM03 (T81803) --- DAYPART RECORD                         *          
*                                                                    *          
* ------------------------------------------------------------------ *          
* UPDATE HISTORY:                                                    *          
*                                                                    *          
* ???????  (???) --- HISTORY LOST                                    *          
*                                                                    *          
* 08OCT90  (EFJ) --- TOMBSTONE ADDED, PHASE CARD CHANGED TO 'A'      *          
*                                                                    *          
**********************************************************************          
T81803   CSECT                                                                  
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
VKEY     LA    R2,DPTDTCH                                                       
         XC    SVDPT,SVDPT                                                      
         CLI   ACTNUM,ACTLIST      IF ACTION LIST                               
         BE    VK20                                                             
         CLI   ACTNUM,ACTREP       OR ACTION REPORT, THEN                       
         BNE   *+12                                                             
VK20     CLI   5(R2),0             OK IF BLANK                                  
         BE    VKEXT                                                            
*                                                                               
         LA    R4,KEY                                                           
         XC    KEY,KEY                                                          
         USING RDPTKEY,R4                                                       
         MVI   RDPTKTYP,X'24'                                                   
         MVC   RDPTKREP,AGENCY                                                  
         LA    R2,DPTDTCH          VALIDATE DAYPART                             
         GOTO1 ANY                                                              
         MVC   RDPTKDPT,8(R2)                                                   
         MVC   SVDPT,8(R2)                                                      
*                                                                               
VKEXT    B     XIT                                                              
         EJECT                                                                  
* DISPLAY KEY                                                                   
DKEY     LA    R4,KEY                                                           
         LA    R2,DPTDTCH          DISPLAY KEY (FOR SELECT)                     
         MVC   8(1,R2),RDPTKDPT                                                 
         OI    6(R2),X'80'                                                      
*                                                                               
         B     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
*              VALIDATE AND DISPLAY RECORD                                      
         SPACE 3                                                                
VREC     EQU   *                                                                
         L     R6,AIO1                                                          
         USING RDPTRECD,R6                                                      
         LA    R2,DPTDTNH                                                       
         GOTO1 ANY                                                              
         MVC   RDPTCODE(2),=X'010A'                                             
         MVC   RDPTNAME,8(R2)                                                   
*                                                                               
* CHECK FOR ANY DUPLICATES                                                      
         LA    R5,5                                                             
         LA    R2,DPTSCCH                                                       
*                                                                               
VR100    CLI   5(R2),0                                                          
         BE    VR200                                                            
         LR    R4,R5                                                            
         LR    R3,R2                                                            
VR120    ZIC   RE,0(R3)                                                         
         AR    R3,RE                                                            
         IC    RE,0(R3)                                                         
         AR    R3,RE                                                            
         CLC   8(1,R2),8(R3)                                                    
         BNE   *+12                                                             
         MVI   ERROR,DUPSDPT                                                    
         B     TRAPERR                                                          
         BCT   R4,VR120                                                         
*                                                                               
VR200    ZIC   RE,0(R2)                                                         
         AR    R2,RE                                                            
         IC    RE,0(R2)                                                         
         AR    R2,RE                                                            
         BCT   R5,VR100                                                         
*                                                                               
         MVI   ELCODE,X'02'                                                     
         GOTO1 HELLO,DMCB,(C'D',=C'REPFIL  '),(ELCODE,0(R6)),0                  
*                                                                               
* BUILD ELEMENTS FOR SECONDARY DAYPARTS & NAMES                                 
         LA    R5,6                                                             
         LA    R2,DPTSCCH                                                       
         XC    WORK,WORK                                                        
         MVC   WORK(2),=X'0208'                                                 
VR300    CLI   5(R2),0                                                          
         BE    VR400                                                            
         MVC   WORK+2(1),8(R2)                                                  
         ZIC   RE,0(R2)                                                         
         AR    R2,RE                                                            
         CLI   5(R2),0                                                          
         BNE   *+12                                                             
VRMIS    MVI   ERROR,MISSING                                                    
         B     TRAPERR                                                          
         MVC   WORK+3(5),8(R2)                                                  
         GOTO1 HELLO,DMCB,(C'P',=C'REPFIL  '),0(R6),WORK                        
*                                                                               
VR400    ZIC   RE,0(R2)                                                         
         AR    R2,RE                                                            
         BCT   R5,VR300                                                         
*                                                                               
         B     XIT                                                              
         DROP  R6                                                               
         EJECT                                                                  
DREC     EQU   *                                                                
         L     R6,AIO1                                                          
         USING RDPTRECD,R6                                                      
         LA    R2,DPTDTNH                                                       
         MVC   8(5,R2),RDPTNAME                                                 
         OI    6(R2),X'80'                                                      
         LA    R2,DPTSCCH                                                       
         LR    R3,R2                                                            
         LA    R0,6                                                             
DR100    XC    8(1,R3),8(R3)                                                    
         OI    6(R3),X'80'                                                      
         ZIC   RE,0(R3)                                                         
         AR    R3,RE                                                            
         XC    8(5,R3),8(R3)                                                    
         OI    6(R3),X'80'                                                      
         ZIC   RE,0(R3)                                                         
         AR    R3,RE                                                            
         BCT   R0,DR100                                                         
*                                                                               
         MVI   ELCODE,X'02'                                                     
         BAS   RE,GETEL                                                         
         B     *+8                                                              
DR200    BAS   RE,NEXTEL                                                        
         BNE   DREXT                                                            
         MVC   8(1,R2),2(R6)                                                    
         OI    6(R2),X'80'                                                      
         ZIC   RE,0(R2)                                                         
         AR    R2,RE                                                            
         MVC   8(5,R2),3(R6)                                                    
         OI    6(R2),X'80'                                                      
         IC    RE,0(R2)                                                         
         AR    R2,RE                                                            
         B     DR200                                                            
*                                                                               
DREXT    B     XIT                                                              
         DROP  R6                                                               
         EJECT                                                                  
*              LIST RECORDS                                                     
         SPACE 3                                                                
LIST     EQU   *                                                                
         LA    R2,LDTSELH                                                       
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
         USING RDPTKEY,R4                                                       
         MVI   RDPTKTYP,X'24'                                                   
         MVC   RDPTKREP,AGENCY     REP                                          
         CLI   MODE,PRINTREP                                                    
         BE    *+10                NO FILTERING FOR REPORT                      
         MVC   RDPTKDPT,SVDPT                                                   
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
         USING RDPTRECD,R6                                                      
         MVC   LISTCDE,RDPTKDPT                                                 
         MVC   LISTNAM,RDPTNAME                                                 
         LA    R4,LISTSDPT                                                      
         MVI   ELCODE,X'02'                                                     
         BAS   RE,GETEL                                                         
         BE    LS320                                                            
         B     LS400                                                            
LS300    BAS   RE,NEXTEL                                                        
         BNE   LS400                                                            
         MVI   0(R4),C','                                                       
         LA    R4,1(R4)                                                         
LS320    MVC   0(5,R4),3(R6)                                                    
         LA    R0,5                MAX LENGTH =5                                
LS340    CLI   0(R4),0                                                          
         BE    LS300                                                            
         LA    R4,1(R4)                                                         
         BCT   R0,LS340                                                         
         B     LS300                                                            
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
HEADING  DS    0H                                                               
         SSPEC H1,1,REQUESTOR                                                   
         SSPEC H2,1,AGYNAME                                                     
         SSPEC H1,50,C'DAYPART RECORDS'                                         
         SSPEC H2,50,C'---------------'                                         
         SSPEC H1,93,RUN                                                        
         SSPEC H2,93,REPORT                                                     
         SSPEC H2,109,PAGE                                                      
         DC    X'00'                                                            
*                                                                               
HDRTN    NTR1                                                                   
         LA    R3,H8                                                            
         MVC   0(3,R3),=C'DPT'                                                  
         MVC   6(29,R3),LDTHDS                                                  
         MVC   132(3,R3),DASH                                                   
         MVC   138(12,R3),DASH                                                  
         MVC   151(27,R3),DASH                                                  
         B     XIT                                                              
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
         GETEL R6,DATADISP,ELCODE                                               
         SPACE                                                                  
TRAPERR  GOTO1 ERREX                                                            
         SPACE 3                                                                
RELO     DS    A                                                                
DASH     DC    47C'-'                                                           
         LTORG                                                                  
         EJECT                                                                  
LISTD    DSECT                                                                  
LISTCDE  DS    CL1                                                              
         DS    CL6                                                              
LISTNAM  DS    CL5                                                              
         DS    CL8                                                              
LISTSDPT DS    CL28                                                             
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
* RESFMF3D                                                                      
       ++INCLUDE RESFMF3D                                                       
         ORG   CONTAGH                                                          
* RESFME3D                                                                      
       ++INCLUDE RESFME3D                                                       
         EJECT                                                                  
* REGENDPT                                                                      
       ++INCLUDE REGENDPT                                                       
         EJECT                                                                  
* RESFMWORKD                                                                    
         PRINT OFF                                                              
       ++INCLUDE RESFMWORKD                                                     
         PRINT ON                                                               
         ORG   SYSSPARE                                                         
SVDPT    DS    CL1                                                              
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'032RESFM03   05/01/02'                                      
         END                                                                    
