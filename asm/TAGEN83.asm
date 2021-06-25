*          DATA SET TAGEN83    AT LEVEL 010 AS OF 05/01/02                      
*PHASE T70283A                                                                  
         TITLE 'T70283 - OFFICE MAINTENANCE'                                    
T70283   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T70283                                                         
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
         CLI   MODE,DISPKEY        DISPLAY KEY                                  
         BNE   *+12                                                             
         BAS   RE,DKEY                                                          
         B     XIT                                                              
         SPACE 1                                                                
         CLI   MODE,VALREC         BUILD RECORD                                 
         BNE   *+12                                                             
         BAS   RE,BLDREC                                                        
         B     XIT                                                              
         SPACE 1                                                                
         CLI   MODE,DISPREC        DISPLAY RECORD                               
         BE    MN10                                                             
         CLI   MODE,XRECADD                                                     
         BE    MN10                                                             
         CLI   MODE,XRECPUT                                                     
         BNE   XIT                                                              
MN10     BAS   RE,DISPLAY                                                       
         SPACE 1                                                                
XIT      XIT1                                                                   
         EJECT                                                                  
*              VALIDATE KEY                                                     
         SPACE 1                                                                
VKEY     NTR1                                                                   
         GOTO1 RECVAL,DMCB,TLOFCDQ,(X'40',SOFOFFH)  BUILD KEY                   
         B     XIT                                                              
         SPACE 3                                                                
*              DISPLAY KEY                                                      
         SPACE 1                                                                
DKEY     NTR1                                                                   
         L     R3,AIO              R3=A(RECORD)                                 
         USING TLOFD,R3                                                         
         MVC   SOFOFF,TLOFOFF      OFFICE CODE                                  
         OI    SOFOFFH+6,X'80'                                                  
         B     XIT                                                              
         SPACE 3                                                                
         GETEL (R3),DATADISP,ELCODE                                             
         EJECT                                                                  
*              ROUTINE BUILDS RECORD                                            
         SPACE 1                                                                
BLDREC   NTR1                                                                   
         MVC   SVKEY,KEY           SAVE KEY                                     
         SPACE 1                                                                
         GOTO1 NAMIN,DMCB,TANAELQ,SOFNAMEH   GET NAME                           
         SPACE 1                                                                
         GOTO1 ADDRIN,DMCB,SOFADDRH              ADDRESS                        
         SPACE 1                                                                
         MVI   ELCODE,TAOFELQ                                                   
         GOTO1 REMELEM                                                          
         SPACE 1                                                                
         LA    R3,ELEMENT                                                       
         XC    ELEMENT,ELEMENT                                                  
         USING TAOFD,R3            BUILD OFFICE DETAILS ELEMENT                 
         MVI   TAOFEL,TAOFELQ                                                   
         MVI   TAOFLEN,TAOFLNQ                                                  
         SPACE 1                                                                
         LA    R2,SOFAREAH                                                      
         GOTO1 ANY                                                              
         MVC   TAOFTEL(3),WORK     SAVE AREA CODE                               
         MVC   TAOFTEL+3(3),SOFTEL      TELEPHONE                               
         MVC   TAOFTEL+6(4),SOFTEL2                                             
         OC    TAOFTEL,SPACES                                                   
         SPACE 1                                                                
         MVC   AIO,AIO2               USE ALTERNATE I/O AREA                    
         GOTO1 USERVAL,DMCB,SOFPQIDH  VALIDATE CONTROL FILE ID                  
         MVC   AIO,AIO1               RESTORE A(OFFICE RECORD)                  
         SPACE 1                                                                
         MVC   TAOFIDNO,TGUSER     SAVE USER ID NUMBER                          
         MVC   TAOFIDCD,TGUSERID   AND CODE                                     
         SPACE 1                                                                
         GOTO1 ADDELEM             ADD THE ELEMENT                              
         SPACE 1                                                                
         GOTO1 ACTVIN,DMCB,0                                                    
         SPACE 1                                                                
         MVC   KEY,SVKEY           RESTORE OFFICE KEY                           
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE DISPLAYS RECORD                                          
         SPACE 1                                                                
DISPLAY  NTR1                                                                   
         TWAXC SOFNAMEH                                                         
         SPACE 1                                                                
         MVC   SVKEY,KEY           SAVE KEY                                     
         SPACE 1                                                                
         GOTO1 CHAROUT,DMCB,TANAELQ,SOFNAMEH                                    
         GOTO1 (RF),(R1),TAADELQ,(4,SOFADDRH)                                   
         SPACE 1                                                                
         XC    SOFIDNM,SOFIDNM     PRE-CLEAR ID NAME                            
         OI    SOFIDNMH+6,X'80'                                                 
         L     R3,AIO                                                           
         MVI   ELCODE,TAOFELQ      LOOK FOR OFFICE DETAILS EL.                  
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING TAOFD,R3                                                         
         MVC   SOFAREA,TAOFTEL     DISPLAY AREA CODE                            
         MVC   SOFTEL,TAOFTEL+3            TELEPHONE                            
         MVC   SOFTEL2,TAOFTEL+6                                                
         SPACE 1                                                                
         MVC   SOFPQID,TAOFIDCD    DISPLAY ID CODE                              
         XC    ELEM(10),ELEM       BUILD USERID KEY                             
         MVC   ELEM+8(2),TAOFIDNO                                               
         MVC   AIO,AIO2                                                         
         GOTO1 USERVAL,DMCB,(X'C0',ELEM),SOFIDNMH  DISPLAY ID NAME              
         MVC   AIO,AIO1                                                         
         SPACE 1                                                                
         GOTO1 ACTVOUT,DMCB,SOFACTVH                                            
         SPACE 1                                                                
         MVC   KEY,SVKEY           RESTORE OFFICE KEY                           
         B     XIT                                                              
         EJECT                                                                  
*              CONSTANTS, ETC.                                                  
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
       ++INCLUDE TAGENFFD                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE TASCR83D                                                       
         SPACE 3                                                                
SVKEY    DS    CL(L'KEY)                                                        
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
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'010TAGEN83   05/01/02'                                      
         END                                                                    
