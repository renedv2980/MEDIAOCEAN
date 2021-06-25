*          DATA SET TAGENC3    AT LEVEL 010 AS OF 07/20/12                      
*PHASE T702C3C,*                                                                
         TITLE 'T702C3 - START EPISODE NUMBER DIS/CHANGE'                       
T702C3   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T702C3                                                         
         L     RC,0(R1)            RC=CONTROLLER STORAGE AREA                   
         USING GEND,RC                                                          
         L     RA,ATWA             RA=A(TWA)                                    
         USING T702FFD,RA                                                       
         L     R9,ASYSD            R9=ROOT STORAGE AREA                         
         USING SYSD,R9                                                          
         L     R8,ASPOOLD          R8=SPOOL STORAGE AREA                        
         USING SPOOLD,R8                                                        
         EJECT                                                                  
*              MODE CONTROL                                                     
         SPACE 2                                                                
         GOTO1 INITIAL,DMCB,0      INITIALIZE                                   
         SPACE 2                                                                
         CLI   MODE,VALKEY         VALIDATE KEY                                 
         BNE   *+12                                                             
         BAS   RE,VKEY                                                          
         B     XIT                                                              
         SPACE 1                                                                
         CLI   MODE,VALREC         BUILD RECORD                                 
         BNE   *+12                                                             
         BAS   RE,VREC                                                          
         B     XIT                                                              
         SPACE 1                                                                
         CLI   MODE,DISPREC        DISPLAY RECORD                               
         BE    MAIN20                                                           
         CLI   MODE,XRECPUT        AFTER CHANGING RECORD                        
         BNE   XIT                                                              
MAIN20   BAS   RE,DISPLAY          (RE-)DISPLAY IT                              
         B     XIT                                                              
         EJECT                                                                  
*              VALIDATE KEY                                                     
         SPACE 1                                                                
VKEY     NTR1                      VALIDATE AGENCY                              
         GOTO1 RECVAL,DMCB,TLAYCDQ,(X'28',SEPAGYH),SEPAGYNH                     
         GOTO1 RAVPPLSA,DMCB,0     SEE IF REC / ACT INVALID FOR P+              
         JNE   ERPPLSI                                                          
         B     XIT                                                              
         EJECT                                                                  
*              VALIDATE RECORD                                                  
         SPACE 1                                                                
VREC     NTR1                                                                   
         MVI   ELCODE,TASOELQ      REMOVE EPISODE ELEMENT                       
         GOTO1 REMELEM                                                          
*                                  VALIDATE NEW EPISODE NUMBER                  
         GOTO1 RECVAL,DMCB,TLEPCDQ,SEPEPIH                                      
*                                                                               
         XC    ELEMENT,ELEMENT     BUILD NEW ELEMENT                            
         LA    R3,ELEMENT                                                       
         USING TASOD,R3                                                         
         MVI   TASOEL,TASOELQ                                                   
         MVI   TASOLEN,TASOLNQ+L'TASOSEPI                                       
         MVI   TASONUM,1           ONE EPISODE                                  
******** MVC   TASOEPI,TGEPI       EPISODE NUMBER                               
         PACK  DUB,TGEPI           SET BINARY EPISODE NUMBER                    
         CVB   RE,DUB                                                           
         STH   RE,TASOEPI                                                       
         GOTO1 ADDELEM                                                          
*                                                                               
         GOTO1 ACTVIN,DMCB,(X'80',SEPLCHGH) ADD ACTIVITY INFO BY SCRN           
         B     XIT                                                              
         DROP  R3                                                               
         EJECT                                                                  
*              ROUTINE DISPLAYS RECORD                                          
         SPACE 1                                                                
DISPLAY  NTR1                                                                   
         XC    SEPEPI,SEPEPI                                                    
         OI    SEPEPIH+6,X'80'                                                  
*                                                                               
         L     R3,AIO                                                           
         MVI   ELCODE,TASOELQ      GET SOAP EPISODE ELEMENT                     
         BAS   RE,GETEL                                                         
         BNE   DISP20                                                           
         USING TASOD,R3            SOAP EPISODE NUMBERS ELEMENT                 
******** MVC   SEPEPI,TASOEPI                                                   
         LH    RE,TASOEPI          DISPLAY CHARACTER START EPISODE              
         CVD   RE,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  SEPEPI,DUB                                                       
*                                                                               
DISP20   GOTO1 ACTVOUT,DMCB,(X'80',SEPLCHGH) DISP ACTIVITY BY SCRN              
         B     XIT                                                              
         DROP  R3                                                               
         EJECT                                                                  
*              EXITS, ETC.                                                      
         SPACE 1                                                                
ERRINV   MVI   ERROR,INVALID                                                    
         B     THEEND                                                           
*                                                                               
ERPPLSI  MVC   MYMSGNO,=Y(ERRIAPPA)   RECORD / ACTION INVALID FOR P+            
         J     ERREND                                                           
                                                                                
ERREND   MVI   MYMTYP,GTMERR       ERROR MESSAGE EXIT                           
         OI    GENSTAT2,USGETTXT                                                
         J     THEEND                                                           
                                                                                
THEEND   GOTO1 EXIT,DMCB,0                                                      
         SPACE 1                                                                
XIT      XIT1                                                                   
         SPACE 3                                                                
         GETEL (R3),DATADISP,ELCODE                                             
         EJECT                                                                  
*              CONSTANTS, ETC.                                                  
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
       ++INCLUDE TAGENFFD                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE TASCRC3D                                                       
         EJECT                                                                  
* DDGENTWA  (MUST FOLLOW LAST SCREEN)                                           
* TAGENWORKD                                                                    
* TASYSDSECT                                                                    
* TASYSEQUS                                                                     
* DDSPLWORKD                                                                    
* DDSPOOLD                                                                      
* TAGENFILE                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDGENTWA                                                       
       ++INCLUDE TAGENWORKD                                                     
       ++INCLUDE TASYSDSECT                                                     
       ++INCLUDE TASYSEQUS                                                      
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE TAGENFILE                                                      
       ++INCLUDE FAGETTXTD                                                      
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'010TAGENC3   07/20/12'                                      
         END                                                                    
