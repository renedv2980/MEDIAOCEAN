*          DATA SET TAGEN28    AT LEVEL 003 AS OF 05/01/02                      
*PHASE T70228A,*                                                                
         TITLE 'T70228 - ACCESS VIOLATION MAINTENANCE'                          
T70228   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T70228                                                         
         L     RC,0(R1)            RC=GENCON STORAGE AREA                       
         USING GEND,RC                                                          
         L     RA,ATWA             RA=A(TWA)                                    
         USING T702FFD,RA                                                       
         L     R9,ASYSD            R9=ROOT STORAGE AREA                         
         USING SYSD,R9                                                          
         L     R8,ASPOOLD          R8=SPOOL DSECT                               
         USING SPOOLD,R8                                                        
         EJECT                                                                  
*              ALL PROCESSING HERE                                              
         SPACE 1                                                                
         CLI   MODE,VALREC         VALIDATE THE RECORD                          
         BNE   XIT                                                              
         GOTO1 INITIAL,DMCB,0      INITIALIZE OVERLAY                           
         SPACE 1                                                                
         OI    SACMSGH+1,X'0C'     INSURE SOME FIELDS ZERO INTENSITY            
         OI    SACMSGH+6,X'80'                                                  
         OI    SACOKTH+1,X'0C'                                                  
         OI    SACOKTH+6,X'80'                                                  
         OI    SACOKH+1,X'2C'                                                   
         OI    SACOKH+6,X'80'                                                   
         SPACE 1                                                                
         LA    R2,SACLINEH         LINE-ID IS REQUIRED                          
         GOTO1 ANY                                                              
         LA    R2,SACADDRH         ADDRESS IS REQUIRED                          
         GOTO1 ANY                                                              
         OC    SACADDR,SPACES      MAKE SURE PAD WITH SPACES                    
         SPACE 1                                                                
         GOTO1 RECVAL,DMCB,TLSYCDQ,(X'30',0) GET SYSTEM RECORD                  
         SPACE 1                                                                
         L     R4,AIO              LOOK FOR ACCESS VIOLATION ELEMENT            
         MVI   ELCODE,TAAVELQ                                                   
         BAS   RE,GETEL                                                         
         B     *+8                                                              
SAC4     BAS   RE,NEXTEL                                                        
         BNE   NTFOUND                                                          
         SPACE 1                                                                
         USING TAAVD,R4                                                         
         CLC   TAAVLINE,SACLINE    MATCH ON LINE ID                             
         BNE   SAC4                                                             
         CLC   TAAVADDR,SACADDR    AND ADDRESS                                  
         BNE   SAC4                                                             
         CLI   TAAVERRS,3          IF MAXIMUM N'ERRORS NOT EXCEEDED             
         BH    *+12                                                             
         CLI   TGCTSTTY,TASTTYPP   ONLY DISPLAY FOR PROGRAMMERS                 
         BNE   SAC4                                                             
         GOTO1 DATCON,DMCB,(1,TAAVDATE),(8,SACMSG+22) DISPLAY DATE              
         SPACE 1                                                                
         GOTO1 TIMECON,DMCB,TAAVTIME,TAAVDATE,(8,SACMSG+34)                     
         NI    SACMSGH+1,X'F3'     DISPLAY MESSAGE                              
         NI    SACOKTH+1,X'FB'     OK? TAG                                      
         NI    SACOKH+1,X'D3'      AND RESPONSE FIELD                           
         SPACE 1                                                                
         LA    R2,SACOKH           RESPONSE FIELD IS REQUIRED                   
         GOTO1 ANY                                                              
         CLI   WORK,C'Y'                                                        
         BNE   FLDINV                                                           
         SPACE 1                                                                
         MVI   TAAVEL,X'FF'        SET TO DELETE ELEMENT                        
         MVI   ELCODE,X'FF'                                                     
         GOTO1 REMELEM                                                          
         SPACE 1                                                                
         GOTO1 PUTREC              WRITE BACK SYSTEM RECORD                     
         B     RESTORED                                                         
         EJECT                                                                  
*              ERRORS, EXITS, ETC.                                              
         SPACE 1                                                                
FLDINV   MVI   ERROR,INVALID       INVALID INPUT FIELD                          
         B     THEEND                                                           
         SPACE 1                                                                
NTFOUND  MVI   ERROR,NOTFOUND      RECORD NOT FOUND                             
         B     THEEND                                                           
         SPACE 1                                                                
RESTORED MVI   MYMSGNO1,12         ACCESS RESTORED                              
         OI    GENSTAT2,USGETTXT                                                
         LA    R2,SACLINEH                                                      
         B     THEEND                                                           
         SPACE 1                                                                
THEEND   GOTO1 EXIT,DMCB,0                                                      
         SPACE 2                                                                
XIT      XIT1                                                                   
         SPACE 2                                                                
         GETEL R4,DATADISP,ELCODE                                               
         EJECT                                                                  
*              CONSTANTS, ETC.                                                  
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
       ++INCLUDE TAGENFFD                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE TASCR28D                                                       
         EJECT                                                                  
* TAGENWORKD                                                                    
* TAGENFILE                                                                     
* TASYSDSECT                                                                    
* DDSPOOLD                                                                      
* DDSPLWORKD                                                                    
         PRINT OFF                                                              
       ++INCLUDE TAGENWORKD                                                     
       ++INCLUDE TAGENFILE                                                      
       ++INCLUDE TASYSDSECT                                                     
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'003TAGEN28   05/01/02'                                      
         END                                                                    
