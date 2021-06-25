*          DATA SET TAGENC7    AT LEVEL 027 AS OF 07/20/12                      
*PHASE T702C7C,*                                                                
         TITLE 'T702C7 - EPISODE MAINTENANCE'                                   
T702C7   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T702C7                                                         
         L     RC,0(R1)            RC=GENCON STORAGE AREA                       
         USING GEND,RC                                                          
         L     RA,ATWA             RA=A(TWA)                                    
         USING T702FFD,RA                                                       
         L     R9,ASYSD            R9=ROOT STORAGE AREA                         
         USING SYSD,R9                                                          
         L     R8,ASPOOLD          R8=SPOOL DSECT                               
         USING SPOOLD,R8                                                        
         EJECT                                                                  
*              MODE CONTROLLED ROUTINES                                         
         SPACE 2                                                                
         GOTO1 INITIAL,DMCB,PFTAB  INITIALIZE                                   
         SPACE 3                                                                
         CLI   MODE,VALKEY         VALIDATE KEY                                 
         BNE   *+12                                                             
         BAS   RE,VKEY                                                          
         B     MAINX                                                            
*                                                                               
         CLI   MODE,DISPKEY        DISPLAY KEY FOR SELECT                       
         BNE   *+12                                                             
         BAS   RE,DKEY                                                          
         B     MAINX                                                            
*                                                                               
         CLI   MODE,DISPREC        IF DISPLAY RECORD                            
         BE    MAIN15                                                           
         CLI   MODE,XRECADD        OR RECORD ADDED                              
         BE    MAIN15                                                           
         CLI   MODE,XRECDEL        OR DELETED                                   
         BE    MAIN15                                                           
         CLI   MODE,XRECREST       OR RESTORED                                  
         BE    MAIN15                                                           
         CLI   MODE,XRECPUT        OR CHANGED                                   
         BNE   MAIN20                                                           
MAIN15   BAS   RE,DISPLAY          (RE-)DISPLAY THE RECORD                      
         B     MAINX                                                            
         SPACE 3                                                                
MAIN20   CLI   MODE,VALREC         VALIDATE RECORD                              
         BNE   MAINX                                                            
         BAS   RE,BLDREC           BUILD IT                                     
         B     MAINX                                                            
         SPACE 3                                                                
MAINX    B     XIT                                                              
*                                                                               
XIT      XIT1                                                                   
         EJECT                                                                  
*              VALIDATE THE KEY                                                 
         SPACE 1                                                                
VKEY     NTR1                                                                   
         GOTO1 RECVAL,DMCB,TLAYCDQ,(X'20',SEPAGYH)       AGENCY                 
         GOTO1 RAVPPLSA,DMCB,0     SEE IF REC / ACT INVALID FOR P+              
         JNE   ERPPLSI                                                          
         SPACE 1                                                                
         LA    R2,SEPEPIH                                                       
         GOTO1 ANY                                                              
         CLI   ACTNUM,ACTADD       IF ADD                                       
         BNE   VKEY10                                                           
         CLI   5(R2),5             EPISODE NUMBER MUST BE 5 CHARS               
         BNE   FLDINV                                                           
VKEY10   GOTO1 RECVAL,DMCB,TLEPCDQ,(X'40',(R2)) EPISODE                         
         B     XIT                                                              
         SPACE 3                                                                
*              DISPLAY THE KEY                                                  
         SPACE 1                                                                
DKEY     NTR1                                                                   
         MVC   SEPAGY,TGAGY        AGENCY                                       
         OI    SEPAGYH+6,X'80'                                                  
         MVC   SEPEPI,TGEPI        EPISODE NUMBER                               
         OI    SEPEPIH+6,X'80'                                                  
         B     XIT                                                              
         EJECT                                                                  
*              BUILD THE RECORD                                                 
         SPACE 1                                                                
BLDREC   NTR1                                                                   
         MVI   ELCODE,TAEIELQ      DELETE OLD EPISODE DATE ELEMENT              
         GOTO1 REMELEM                                                          
*                                                                               
         LA    R4,ELEMENT          ADD NEW ELEMENT                              
         USING TAEID,R4                                                         
         MVI   TAEIEL,TAEIELQ                                                   
         MVI   TAEILEN,TAEILNQ                                                  
         LA    R2,SEPWDTH          WORK DATE INPUT                              
         GOTO1 DTVAL,DMCB,TAEIWDT                                               
         LA    R2,SEPADTH          AIR DATE INPUT                               
         GOTO1 DTVAL,DMCB,TAEIADT                                               
         GOTO1 ADDELEM                                                          
*                                                                               
         GOTO1 ACTVIN,DMCB,0       LAST CHANGED                                 
         B     XIT                                                              
         SPACE 3                                                                
*              DISPLAY THE RECORD                                               
         SPACE 1                                                                
DISPLAY  NTR1                                                                   
         L     R6,AIO                                                           
         LR    R4,R6                                                            
         MVI   ELCODE,TAEIELQ                                                   
         BAS   RE,GETEL                                                         
         BNE   DISPX                                                            
         USING TAEID,R4                                                         
         OC    TAEIWDT,TAEIWDT     WORK DATE                                    
         BZ    DISP10                                                           
         GOTO1 DATCON,DMCB,(1,TAEIWDT),(5,SEPWDT)                               
         OI    SEPWDTH+6,X'80'     TRAMSIT                                      
*                                                                               
DISP10   OC    TAEIADT,TAEIADT     AIR DATE                                     
         BZ    DISP20                                                           
         GOTO1 DATCON,DMCB,(1,TAEIADT),(5,SEPADT)                               
         OI    SEPADTH+6,X'80'     TRAMSIT                                      
*                                                                               
DISP20   GOTO1 ACTVOUT,DMCB,SEPLCHGH  LAST CHANGED                              
DISPX    B     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
*              EXITS                                                            
FLDINV   MVI   ERROR,INVALID                                                    
         B     ERRXIT                                                           
*                                                                               
FLDMISS  MVI   ERROR,MISSING                                                    
         B     ERRXIT                                                           
*                                                                               
ERPPLSI  MVC   MYMSGNO,=Y(ERRIAPPA)   RECORD / ACTION INVALID FOR P+            
         J     ERREND                                                           
                                                                                
ERREND   MVI   MYMTYP,GTMERR       ERROR MESSAGE EXIT                           
         OI    GENSTAT2,USGETTXT                                                
         J     ERRXIT                                                           
                                                                                
ERRXIT   XC    DMCB,DMCB                                                        
         GOTO1 EXIT,DMCB                                                        
         SPACE 2                                                                
         GETEL R4,DATADISP,ELCODE                                               
         SPACE 2                                                                
*              CONSTANTS, ETC.                                                  
         SPACE 2                                                                
COMPLM   DC    X'FFFFFFFFFFFF'                                                  
         SPACE                                                                  
PFTAB    DS    0C                  PF KEYS TABLE                                
         DC    AL1(PF13X-*,13,0,0,0)                                            
         DC    CL3' ',CL8'EPISODE ',CL8'LIST    '                               
PF13X    EQU   *                                                                
         DC    X'FF'                                                            
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
       ++INCLUDE TAGENFFD                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE TASCRC7D                                                       
         EJECT                                                                  
* DDGENTWA                                                                      
* TAGENWORKD                                                                    
* TAGENFILE                                                                     
* TASYSEQUS                                                                     
* TASYSDSECT                                                                    
* DDSPOOLD                                                                      
* DDSPLWORKD                                                                    
         PRINT OFF                                                              
       ++INCLUDE DDGENTWA                                                       
       ++INCLUDE TAGENWORKD                                                     
       ++INCLUDE TAGENFILE                                                      
       ++INCLUDE TASYSEQUS                                                      
       ++INCLUDE TASYSDSECT                                                     
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE FAGETTXTD                                                      
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'027TAGENC7   07/20/12'                                      
         END                                                                    
