*          DATA SET TAGEN89    AT LEVEL 007 AS OF 01/16/13                      
*PHASE T70289A,*                                                                
         TITLE 'T70289 - STARTING CHECK NUMBER MAINTENANCE'                     
T70289   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T70289                                                         
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
         SPACE 1                                                                
         GOTO1 INITIAL,DMCB,0                                                   
         SPACE 1                                                                
         CLI   MODE,VALKEY         FIRST TIME IN                                
         BNE   SYS10                                                            
         GOTO1 RECVAL,DMCB,TLSYCDQ,(X'40',0)                                    
         B     XIT                                                              
         SPACE 3                                                                
SYS10    CLI   MODE,DISPREC        IF MODE IS DISPLAY                           
         BE    SYS15                                                            
         CLI   MODE,XRECPUT        OR RECORD CHANGED                            
         BNE   SYS20                                                            
*                                                                               
SYS15    BAS   RE,DISPLAY          (RE-)DISPLAY THE RECORD                      
         B     XIT                                                              
         SPACE 3                                                                
SYS20    CLI   MODE,VALREC         VALIDATE THE RECORD                          
         BNE   XIT                                                              
         CLC   SSCLAST+1(2),=X'0101'  IF SCREEN JUST LOADED                     
         BNE   SYS30                                                            
         GOTO1 RECVAL,DMCB,TLSYCDQ,(X'20',0)  GET THE RECORD                    
         BAS   RE,DISPLAY                     DISPLAY RECORD FIRST              
         MVC   TWAKEYSV,KEY        NEED BECAUSE DON'T GET VALKEY MODE           
*                                  WHEN DO ACTION CHANGE FROM OTHER REC         
         B     DISPLYD             (NEED THIS WHEN DON'T HAVE KEY FLDS)         
         SPACE 1                                                                
SYS30    BAS   RE,BLDREC                                                        
         B     XIT                                                              
         EJECT                                                                  
*              DISPLAY THE RECORD                                               
         SPACE 1                                                                
DISPLAY  NTR1                                                                   
         TWAXC SSCUSBKH            CLEAR THE SCREEN                             
         SPACE 1                                                                
         MVI   ELCODE,TASYELQ      GET SYSTEM CONTROL ELEMENT                   
         L     R4,AIO                                                           
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                MUST BE THERE                                
         USING TASYD,R4                                                         
         MVC   SSCUSBK,TASYUSBK    USA BANK A/C                                 
         MVC   SSCUSST,TASYUSST    NEXT CHECK                                   
         MVC   SSCCNBK,TASYCNBK    CANADA BANK A/C                              
         MVC   SSCCNST,TASYCNST    NEXT CHECK                                   
         MVC   SSCEUBK,TASYEUBK    EURO BANK A/C                                
         MVC   SSCEUST,TASYEUST    NEXT CHECK                                   
         MVC   SSCPCBK,TASYPCBK    PRINT BANK A/C                               
         MVC   SSCPCST,TASYPCST    NEXT CHECK                                   
         MVC   SSCLCBK,TASYLCBK    P+ BANK A/C                                  
         MVC   SSCLCST,TASYLCST    NEXT CHECK                                   
         SPACE 1                                                                
         GOTO1 ACTVOUT,DMCB,(X'80',SSCLCHGH)  LAST CHANGED                      
         B     XIT                                                              
         EJECT                                                                  
*              BUILD THE RECORD                                                 
         SPACE 1                                                                
         USING TASYD,R4                                                         
BLDREC   NTR1                                                                   
         MVI   ELCODE,TASYELQ      GET SYSTEM CONTROL ELEMENT                   
         L     R4,AIO                                                           
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                MUST BE THERE                                
         USING TASYD,R4                                                         
         SPACE                                                                  
         CLI   SSCUSBKH+5,0        IF THERE IS INPUT                            
         BE    *+16                                                             
         MVC   TASYUSBK,SSCUSBK    MOVE INTO ELEMENT                            
         OC    TASYUSBK,SPACES     PAD WITH SPACES                              
         SPACE                                                                  
         CLI   SSCUSSTH+5,0                                                     
         BE    *+16                                                             
         MVC   TASYUSST,SSCUSST                                                 
         OC    TASYUSST,SPACES                                                  
         SPACE                                                                  
         CLI   SSCCNBKH+5,0                                                     
         BE    *+16                                                             
         MVC   TASYCNBK,SSCCNBK                                                 
         OC    TASYCNBK,SPACES                                                  
         SPACE                                                                  
         CLI   SSCCNSTH+5,0                                                     
         BE    *+16                                                             
         MVC   TASYCNST,SSCCNST                                                 
         OC    TASYCNST,SPACES                                                  
         SPACE                                                                  
         CLI   SSCEUBKH+5,0                                                     
         BE    *+16                                                             
         MVC   TASYEUBK,SSCEUBK                                                 
         OC    TASYEUBK,SPACES                                                  
         SPACE                                                                  
         CLI   SSCEUSTH+5,0                                                     
         BE    *+16                                                             
         MVC   TASYEUST,SSCEUST                                                 
         OC    TASYEUST,SPACES                                                  
         SPACE                                                                  
         CLI   SSCPCBKH+5,0                                                     
         BE    *+16                                                             
         MVC   TASYPCBK,SSCPCBK                                                 
         OC    TASYPCBK,SPACES                                                  
         SPACE                                                                  
         CLI   SSCPCSTH+5,0                                                     
         BE    *+16                                                             
         MVC   TASYPCST,SSCPCST                                                 
         OC    TASYPCST,SPACES                                                  
         SPACE                                                                  
         CLI   SSCLCSTH+5,0                                                     
         BE    *+16                                                             
         MVC   TASYLCST,SSCLCST                                                 
         OC    TASYLCST,SPACES                                                  
         SPACE                                                                  
         GOTO1 ACTVIN,DMCB,(X'80',0)  LAST CHANGED                              
         B     XIT                                                              
         EJECT                                                                  
*              LOCAL ERROR/EXIT ROUTINES                                        
         SPACE                                                                  
FLDINV   MVI   ERROR,INVALID       INVALID INPUT FIELD                          
         B     THEEND                                                           
         SPACE                                                                  
FLDMISS  MVI   ERROR,MISSING       MISSING INPUT FIELD                          
         B     THEEND                                                           
         SPACE                                                                  
DISPLYD  MVI   MYMSGNO1,4          RECORD DISPLAYED - ENTER CHANGES             
         MVI   MYMSYS,X'FF'                                                     
         OI    GENSTAT2,USGETTXT                                                
         L     R2,AFRSTREC                                                      
         B     THEEND                                                           
         SPACE                                                                  
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
       ++INCLUDE TASCR89D                                                       
         EJECT                                                                  
* DDGENTWA                                                                      
* TAGENWORKD                                                                    
* TAGENFILE                                                                     
* DDSPOOLD                                                                      
* DDSPLWORKD                                                                    
         PRINT OFF                                                              
       ++INCLUDE DDGENTWA                                                       
       ++INCLUDE TAGENWORKD                                                     
       ++INCLUDE TAGENFILE                                                      
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'007TAGEN89   01/16/13'                                      
         END                                                                    
