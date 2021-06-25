*          DATA SET TAGENA1    AT LEVEL 005 AS OF 07/20/12                      
*PHASE T702A1C                                                                  
         TITLE 'T702A1 - ESTIMATE COPY'                                         
T702A1   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T702A1                                                         
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
         CLI   MODE,VALKEY         VALIDATE SCREEN                              
         BNE   *+12                                                             
         BAS   RE,VKEY                                                          
         B     XIT                                                              
         SPACE 1                                                                
         CLI   MODE,VALREC         COPY RECORD                                  
         BNE   XIT                                                              
         BAS   RE,COPY                                                          
         B     COPIED              GIVE COMPLETION MESSAGE                      
         EJECT                                                                  
*              VALIDATE KEY                                                     
         SPACE 1                                                                
VKEY     NTR1                                                                   
         GOTO1 RECVAL,DMCB,TLAYCDQ,(X'28',SESAGYH),SESAGYNH  AGENCY             
         GOTO1 RAVPPLSA,DMCB,0     SEE IF REC / ACT INVALID FOR P+              
         JNE   ERPPLSI                                                          
         SPACE 1                                                                
         LA    R3,TLESCDQ          SET ESTIMATE RECORD CODE                     
         CLI   RECNUM,ES                                                        
         BE    VK10                                                             
         LA    R3,TLSSCDQ          ELSE SET SESSION ESTIMATE                    
         MVI   TGTYPE,TLSSTYPT     ASSUME TV                                    
         CLI   RECNUM,ET                                                        
         BE    *+8                                                              
         MVI   TGTYPE,TLSSTYPR     ELSE IT MUST BE RADIO                        
         SPACE 1                                                                
VK10     GOTO1 RECVAL,DMCB,(R3),(X'08',SESESTFH),SESESTNH FROM EST              
         SPACE 1                                                                
         LA    R2,SESESTTH                                  TO EST              
         GOTO1 ANY                                                              
         MVC   AIO,AIO2                                                         
         GOTO1 RECVAL,DMCB,(R3),(X'04',(R2))                                    
         BE    ONFILE                                                           
         MVC   AIO,AIO1                                                         
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO CONTROL RECORD COPYING                                
         SPACE 1                                                                
*                                  AIO HAS A(FIRST RECORD TO COPY)              
*                                  TGEST HAS NEW ESTIMATE CODE                  
COPY     NTR1                                                                   
COP4     L     R3,AIO                                                           
         CLI   RECNUM,ES           IF ESTIMATE RECORD TYPE                      
         BNE   *+14                                                             
         USING TLESD,R3                                                         
         MVC   TLESEST,TGEST       SET NEW ESTIMATE CODE                        
         B     *+10                                                             
         USING TLSSD,R3                                                         
         MVC   TLSSEST,TGEST       ELSE SET NEW SESSION ESTIMATE CODE           
         SPACE 1                                                                
         LR    R4,R3                                                            
         MVI   ELCODE,TAACELQ      IF ACTIVITY EL. ON THIS RECORD               
         BAS   RE,GETEL                                                         
         BNE   COP8                                                             
         GOTO1 ACTVIN,DMCB,0       ADD NEW ACTIVITY ELEMENT                     
         SPACE 1                                                                
COP8     BAS   RE,MYADDREC         ADD THE RECORD TO THE FILE                   
         SPACE 1                                                                
         USING TLESD,R3                                                         
         MVC   KEY,TLESKEY         MOVE KEY JUST ADDED FROM AIO TO KEY          
         LA    R3,KEY                                                           
         CLI   RECNUM,ES           IF ESTIMATE RECORD TYPE                      
         BNE   COP10                                                            
         MVC   TLESEST,SESESTF     RESTORE 'FROM' ESTIMATE CODE                 
         OC    TLESEST,SPACES                                                   
         B     COP20                                                            
         SPACE 1                                                                
         USING TLSSD,R3                                                         
COP10    MVC   TLSSEST,SESESTF     ELSE RESTORE 'FROM' SESSION EST CODE         
         OC    TLSSEST,SPACES                                                   
         SPACE 1                                                                
COP20    GOTO1 HIGH                RE-READ RECORD WE JUST COPIED                
         SPACE 1                                                                
         GOTO1 SEQ                 GET NEXT 'FROM' RECORD                       
         SPACE 1                                                                
         CLI   RECNUM,ES           IF ESTIMATE RECORD TYPE                      
         BNE   COP30                                                            
         USING TLESD,R3                                                         
         CLC   TLESKEY(TLESSEQ-TLESD),KEYSAVE  TEST IF SAME ESTIMATE            
         BE    COP40                                                            
         B     XIT                 NO MORE                                      
         SPACE 1                                                                
         USING TLSSD,R3                                                         
COP30    CLC   TLSSKEY(TLSSSEQ-TLSSD),KEYSAVE  ELSE TEST SAME SESS EST.         
         BNE   XIT                 NO MORE                                      
         SPACE 1                                                                
COP40    GOTO1 GETREC              MORE - GET FILE RECORD                       
         B     COP4                GO COPY IT                                   
         EJECT                                                                  
*              ROUTINE CONTROLS ADDING NEW ESTIMATE RECORD TO FILE              
         SPACE 1                                                                
         USING TLESD,R3            R3=A(ESTIMATE RECORD)                        
MYADDREC NTR1                                                                   
         MVC   KEY,TLESKEY         SEE IF RECORD ON FILE MARKED DELETED         
         OI    DMINBTS,X'08'       SET READ FOR DELETED                         
         MVI   RDUPDATE,C'Y'       AND FOR UPDATE                               
         GOTO1 HIGH                LOOK FOR RECORD ALREADY ON FILE              
         SPACE 1                                                                
         LA    R4,KEY              R4=A(DIRECTORY KEY)                          
         USING TLDRD,R4                                                         
         CLC   TLDRKEY,KEYSAVE     TEST WE FOUND RECORD                         
         BNE   MYADR4                                                           
         TM    TLDRSTAT,X'80'      IT HAD BETTER BE DELETED                     
         BO    *+6                                                              
         DC    H'0'                                                             
         NI    TLDRSTAT,X'7F'      TURN OFF DELETE BIT                          
         GOTO1 WRITE               AND WRITE IT BACK                            
         SPACE 1                                                                
         MVC   AIO,AIO3            NOW SET TO GET THE RECORD                    
         MVI   RDUPDATE,C'Y'       READ FOR UPDATE                              
         GOTO1 GETREC              GET THE RECORD SO THAT WE CAN ...            
         ST    R3,AIO                                                           
         GOTO1 PUTREC              WRITE NEW ONE BACK OVER DELETED ONE          
         B     MYADRX                                                           
         SPACE 1                                                                
MYADR4   MVC   TLDRKEY,KEYSAVE                                                  
         GOTO1 ADDREC              OK TO ADD THE RECORD                         
         SPACE 1                                                                
MYADRX   NI    DMINBTS,X'F7'                                                    
         B     XIT                                                              
         EJECT                                                                  
*              EXITS, ETC.                                                      
         SPACE 1                                                                
ONFILE   MVI   ERROR,RECEXIST      RECORD ALREADY EXISTS                        
         B     THEEND                                                           
         SPACE 1                                                                
COPIED   MVI   MYMSGNO1,62         SET COMPLETION MESSAGE                       
         OI    GENSTAT2,USGETTXT                                                
         LA    R2,CONRECH                                                       
         B     THEEND                                                           
         SPACE 1                                                                
ERPPLSI  MVC   MYMSGNO,=Y(ERRIAPPA)   RECORD / ACTION INVALID FOR P+            
         J     ERREND                                                           
                                                                                
ERREND   MVI   MYMTYP,GTMERR       ERROR MESSAGE EXIT                           
         OI    GENSTAT2,USGETTXT                                                
         J     THEEND                                                           
                                                                                
THEEND   GOTO1 EXIT,DMCB,0                                                      
         SPACE 1                                                                
YES      XR    RC,RC                                                            
NO       LTR   RC,RC                                                            
XIT      XIT1                                                                   
         SPACE 3                                                                
         GETEL (R4),DATADISP,ELCODE                                             
         EJECT                                                                  
*              CONSTANTS, ETC.                                                  
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
       ++INCLUDE TAGENFFD                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE TASCRA1D                                                       
         EJECT                                                                  
* DDGENTWA     (MUST FOLLOW LAST SCREEN)                                        
* TAGENWORKD                                                                    
* TASYSDSECT                                                                    
* TASYSEQUS                                                                     
* TAGENEQUS                                                                     
* DDSPLWORKD                                                                    
* DDSPOOLD                                                                      
* TAGENFILE                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDGENTWA                                                       
       ++INCLUDE TAGENWORKD                                                     
       ++INCLUDE TASYSDSECT                                                     
       ++INCLUDE TASYSEQUS                                                      
       ++INCLUDE TAGENEQUS                                                      
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE TAGENFILE                                                      
       ++INCLUDE FAGETTXTD                                                      
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'005TAGENA1   07/20/12'                                      
         END                                                                    
