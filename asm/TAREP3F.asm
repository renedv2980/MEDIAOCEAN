*          DATA SET TAREP3F    AT LEVEL 015 AS OF 04/08/14                      
*PHASE T7033FA                                                                  
         TITLE 'T7033F - PAYROLL CROSS REFERENCE REPORT'                        
T7033F   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T7033F                                                         
         L     RC,0(R1)                                                         
         USING GEND,RC             RC=A(CONTROLLER W/S)                         
         L     RA,ATWA                                                          
         USING T703FFD,RA          RA=A(SCREEN)                                 
         L     R9,ASUBSYSD                                                      
         USING SUBSYSD,R9          R9=A(SYSTEM W/S)                             
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8           R8=A(SPOOL DSECT)                            
         EJECT                                                                  
*              MODE CONTROLLED ROUTINES                                         
         SPACE 1                                                                
         GOTO1 INITIAL,DMCB,0      INITIALIZE                                   
         SPACE 1                                                                
         CLI   MODE,VALREC         VALIDATE SCREEN                              
         BE    VKEY                                                             
         CLI   MODE,DISPREC                                                     
         BE    VKEY                                                             
         CLI   MODE,PRINTREP       PROCESS REPORT                               
         BE    PREP                                                             
         B     XIT                                                              
         EJECT                                                                  
*              VALIDATE SCREEN                                                  
         SPACE 1                                                                
VKEY     XC    TIFILTS,TIFILTS     CLEAR SYSIO FILTERS                          
*                                                                               
         GOTO1 RECVAL,DMCB,(X'40',TLAYCDQ),(X'08',SCRAGYH),SCRAGYNH             
         CLI   SCRAGY,C'@'                                                      
         BE    VK10                                                             
         MVC   TIFAGY,TGAGY                                                     
         B     VK20                                                             
*                                                                               
VK10     MVC   TIFAGY,TGLST                                                     
         NI    TIFAGY,X'7F'        TURN OFF X'80' FOR SYSIO = FLIST             
*                                                                               
VK20     LA    R2,SCRPDDH          VALIDATE DETAIL PERIOD                       
         GOTO1 ANY                                                              
         LA    R3,BLOCK                                                         
         USING PERVALD,R3                                                       
         GOTO1 PDVAL,DMCB,(R3)     GO TO SYSTEM PERIOD VAL. ROUTINE             
         MVC   TIQPSTR,PVALPSTA    SET FOR SYSIO                                
         MVC   TIQPEND,PVALPEND                                                 
*                                                                               
         LA    R2,SCRTITH          TITLE                                        
         MVC   TITLE(L'DEFTITLE),DEFTITLE  SET DEFAULT TITLE                    
         CLI   5(R2),0                                                          
         BE    *+10                                                             
         MVC   TITLE,8(R2)                                                      
         OC    TITLE,SPACES                                                     
         GOTO1 CENTER,DMCB,TITLE,L'TITLE                                        
*                                                                               
         LA    R2,SCRSTITH         SUB-TITLE                                    
         MVC   SUBTITLE,8(R2)                                                   
         OC    SUBTITLE,SPACES                                                  
         CLI   5(R2),0                                                          
         BE    VK30                                                             
         GOTO1 CENTER,DMCB,SUBTITLE,L'SUBTITLE                                  
*                                                                               
VK30     LA    R2,SCROPTH          VALIDATE OPTIONS                             
         GOTO1 VALOPTS                                                          
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE CONTROLS REPORT GENERATION                               
         SPACE 1                                                                
PREP     BAS   RE,INIT             INTIALIZE                                    
******** GOTO1 DYNALLOC,DMCB,(X'FF',=CL8'FILEIN'),=CL30'DHAB.TXR.DATA'          
         GOTO1 DYNALLOC,DMCB,(X'FF',=CL8'FILEIN'),                     +        
               (0,=CL35'DHAB.TXR.DATA')                                         
*                                                                               
         LA    R2,FILEIN                                                        
         OPEN  ((R2),INPUT)                                                     
         LA    R3,TIFSSN                                                        
*                                                                               
PREP10   GET   (R2),(R3)                                                        
         GOTO1 TASYSIO,DMCB,TASYSIOD  OFF TO SYSIO TO DO I/O                    
         B     PREP10                                                           
*                                                                               
PREP20   BAS   RE,OUTDRIVE         PRINT THE REPORT                             
         B     XIT                                                              
         EJECT                                                                  
*              INITIALIZATION ROUTINES FOR REPORT                               
         SPACE 1                                                                
INIT     NTR1                                                                   
         L     R1,TWADCONS                                                      
         USING TWADCOND,R1                                                      
         MVC   DYNALLOC,TDYNALLO                                                
         DROP  R1                                                               
*                                                                               
         MVC   TIACOMFC,ACOMFACS   SET UP SYSIO BLOCK                           
         LA    R1,IOHOOK           A(I/O HOOK)                                  
         ST    R1,TIHOOK                                                        
         MVC   TIACCS,TWAACCS      LIMIT ACCESS                                 
         MVC   TIAUTH,TWAAUTH      AUTHORIZATION                                
         MVC   TIUSERID,TWAORIG    REQUESTING ID                                
         MVC   TIQSTAFF,TGCTSTAF   STAFF ID                                     
         MVI   TIREAD,TLCKCDQ      READ CHECK RECORDS                           
         MVI   TIQDTYPE,TIQDCHK    SET FILTERING ON CHECK DATE                  
         SPACE 1                                                                
         GOTO1 CALLOV,DMCB,0,X'D9070297'  LOAD DPG PROGRAM                      
         MVC   ADPGPROG,0(R1)                                                   
         GOTO1 INITDRIV            DRIVER INITIALIZATION ROUTINES               
*                                                                               
         MVI   ROWWIDTH,80         SET ROW WIDTH TO GET EXPAND TOTALS           
*                                                                               
         L     R2,AGLOBAL          R2=A(DRIVER W/S)                             
         USING GLOBALD,R2                                                       
         OI    GLINDS,GLPALDET     SET WE WANT ALL DETAILS                      
         LA    R1,HOOK             SET A(HEADLINE HOOK)                         
         ST    R1,GLAHOOK                                                       
*                                                                               
         BAS   RE,INTDRIVE         INITIALIZE DRIVER                            
         B     XIT                                                              
         EJECT                                                                  
*              PROCESS RECORDS FROM SYSIO                                       
         SPACE 1                                                                
IOHOOK   NTR1                                                                   
         CLI   TIMODE,PROCREC      PROCESS RECORD (CHECK)                       
         BNE   XIT                                                              
         BAS   RE,INDRIVE                                                       
         B     XIT                                                              
         EJECT                                                                  
*              DRIVER INTERFACE ROUTINES                                        
         SPACE 2                                                                
INTDRIVE DS    0H                                                               
         MVI   BYTE,GLINIT                                                      
         B     ALLDRIVE                                                         
*                                                                               
INDRIVE  DS    0H                                                               
         MVI   BYTE,GLINPUT                                                     
         B     ALLDRIVE                                                         
*                                                                               
OUTDRIVE DS    0H                                                               
         MVI   BYTE,GLOUTPUT                                                    
         B     ALLDRIVE                                                         
*                                  BYTE=MODE                                    
ALLDRIVE NTR1                                                                   
         L     R2,AGLOBAL                                                       
         USING GLOBALD,R2                                                       
         MVC   GLMODE,BYTE                                                      
         GOTO1 DRIVER,DMCB,(R2)                                                 
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE HANDLES RECORD TRACES                                    
         SPACE 1                                                                
MYTRACE  NTR1                                                                   
         CLI   TRACEOPT,C'Y'                                                    
         BNE   XIT                                                              
         L     R2,0(R1)                                                         
         ZIC   R3,0(R1)                                                         
         MVC   WORK,0(R2)          MOVE LITERAL TO WORK                         
         LA    R4,WORK(R3)                                                      
         MVC   0(7,R4),=C' (AIO?)' ADD I/O AREA LITERAL                         
         LA    R3,7(R3)            BUMP L'LITERAL                               
         CLC   AIO,AIO1                                                         
         BNE   *+8                                                              
         MVI   5(R4),C'1'          SET CURRENT I/O AREA LITERAL                 
         CLC   AIO,AIO2                                                         
         BNE   *+8                                                              
         MVI   5(R4),C'2'                                                       
         CLC   AIO,AIO3                                                         
         BNE   *+8                                                              
         MVI   5(R4),C'3'                                                       
         GOTO1 TRACE,DMCB,AIO,0,WORK,(R3)                                       
         B     XIT                                                              
         SPACE 3                                                                
*              ROUTINE HANDLES STORAGE TRACES                                   
         SPACE 1                                                                
MYTRACE2 NTR1                                                                   
         CLI   TRACEOPT,C'Y'                                                    
         BNE   XIT                                                              
         LM    R2,R3,0(R1)                                                      
         ZIC   R4,0(R1)                                                         
         GOTO1 TRACE,DMCB,(R3),0,(R2),(R4)                                      
         B     XIT                                                              
         EJECT                                                                  
*              HEADLINE HOOK (HEADHOOK)                                         
         SPACE 1                                                                
HOOK     NTR1                                                                   
         L     R2,AGLOBAL                                                       
         USING GLOBALD,R2                                                       
         CLI   GLHOOK,GLHEAD       PROCESSING HEADINGS                          
         BNE   HKX                                                              
         GOTO1 GENHEAD             TAREPGEN WILL HANDLE                         
*                                                                               
HKX      B     XIT                                                              
         EJECT                                                                  
*                                                                               
MISSERR  MVI   ERROR,MISSING       MISSING INPUT FIELD                          
         B     THEEND                                                           
*                                                                               
THEEND   GOTO1 ERREX                                                            
*                                                                               
YES      XR    RC,RC                                                            
NO       LTR   RC,RC                                                            
XIT      XIT1                                                                   
         SPACE 3                                                                
         GETEL R4,DATADISP,ELCODE                                               
         EJECT                                                                  
*              CONSTANTS, ETC.                                                  
         SPACE 1                                                                
DEFTITLE DC    C'PAYROLL CROSS REFERENCE REPORT'                                
         SPACE 1                                                                
         LTORG                                                                  
         SPACE 2                                                                
FILEIN   DCB   DDNAME=FILEIN,DSORG=PS,EODAD=PREP20,MACRF=GM                     
         EJECT                                                                  
       ++INCLUDE TAREPFFD                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE TAREPDFD                                                       
         EJECT                                                                  
         ORG   SCRWORK                                                          
DYNALLOC DS    A                                                                
         SPACE 3                                                                
* DDGENTWA   (MUST FOLLOW LAST SCREEN)                                          
* DDPERVAL                                                                      
* DDSPOOLD                                                                      
* DDSPLWORKD                                                                    
* DDBIGBOX                                                                      
* DDTWADCONS                                                                    
* TAGENFILE                                                                     
* TASYSDSECT                                                                    
* TASYSEQUS                                                                     
* DRGLOBAL                                                                      
* TAREPWORKD                                                                    
         PRINT OFF                                                              
       ++INCLUDE DDGENTWA                                                       
       ++INCLUDE DDPERVALD                                                      
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE DDBIGBOX                                                       
TWADCOND DSECT                                                                  
       ++INCLUDE DDTWADCONS                                                     
       ++INCLUDE TAGENFILE                                                      
       ++INCLUDE TASYSDSECT                                                     
       ++INCLUDE TASYSEQUS                                                      
       ++INCLUDE DRGLOBAL                                                       
       ++INCLUDE TAREPWORKD                                                     
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'015TAREP3F   04/08/14'                                      
         END                                                                    
