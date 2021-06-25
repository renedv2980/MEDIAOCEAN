*          DATA SET TAREP59    AT LEVEL 002 AS OF 01/03/13                      
*PHASE T70359A                                                                  
         TITLE 'T70359 - SEASONAL COMMERCIAL UPDATE'                            
T70359   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 TMPLNQ,T70356                                                    
         LR    RE,RC                                                            
         L     RC,0(R1)                                                         
         USING GEND,RC             RC=A(CONTROLLER W/S)                         
         L     RA,ATWA                                                          
         USING T703FFD,RA          RA=A(SCREEN)                                 
         L     R9,ASUBSYSD                                                      
         USING SUBSYSD,R9          R9=A(SYSTEM W/S)                             
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8           R8=A(SPOOL DSECT)                            
         LA    R7,BUFF                                                          
         LA    R7,8(R7)                                                         
         USING RECD,R7             R7=A(LOCAL W/S)                              
         ST    RE,ASVPTRS          SAVE A(SAVED POINTER BLOCK)                  
         AHI   RE,L'SVPTRBLK                                                    
         ST    RE,AUPPTRS          SAVE A(UPDATED POINTER BLOCK)                
         EJECT                                                                  
*              MODE CONTROLLED ROUTINES                                         
         SPACE 1                                                                
         GOTO1 INITIAL,DMCB,0      INITIALIZE                                   
         SPACE 1                                                                
         CLI   MODE,VALKEY         IF CALLED WITH VALIDATE KEY                  
         BNE   *+12                                                             
         BAS   RE,VK               VALIDATE THE KEY                             
         B     XIT                                                              
         SPACE 1                                                                
         CLI   MODE,PRINTREP       IF CALLED WITH PRINTREP                      
         BNE   *+8                                                              
         BAS   RE,PREP             PROCESS THE REPORT                           
XIT      XIT1                                                                   
         EJECT                                                                  
*              ROUTINE TO VALIDATE KEY                                          
         SPACE 1                                                                
VK       NTR1                                                                   
         BAS   RE,VALOPT           MERELY VALIDATE THE OPTIONS                  
         B     XIT                                                              
         SPACE 2                                                                
*              ROUTINE TO VALIDATE OPTIONS                                      
VALOPT   NTR1                                                                   
         LA    R2,SPLOPTH          VALIDATE OPTIONS                             
         CLI   5(R2),0                                                          
         BE    VOPTX                                                            
         SPACE 1                                                                
         LA    R3,BLOCK            R3=A(SCAN BLOCK)                             
         USING SCAND,R3                                                         
         GOTO1 SCANNER,DMCB,(R2),(X'80',(R3))                                   
         CLI   4(R1),0                                                          
         BE    FLDINV                                                           
         ZIC   R0,4(R1)            R0 = NUM OF SCAN BLOCK ENTRIES               
         SPACE 1                                                                
VOPT4    CLC   =C'TRACE',SCDATA1                                                
         BNE   FLDINV                                                           
         CLI   SCDATA2,C'Y'        IF TRACING REQUESTED                         
         BNE   VOPT8                                                            
         OI    OPTION,OPTTRACE     SAVE INDICATOR                               
         SPACE 1                                                                
VOPT8    LA    R3,SCANNEXT         BUMP TO NEXT ELEMENT                         
         BCT   R0,VOPT4                                                         
VOPTX    B     XIT                                                              
         DROP  R3                  DROP THE SCAND DSECT                         
         EJECT                                                                  
         SPACE 1                                                                
*              ROUTINE CONTROLS REPORT GENERATION                               
         SPACE 1                                                                
PREP     NTR1                                                                   
         LA    R1,MYSPECS          SET A(SPECS) FOR PRINTING                    
         ST    R1,SPECS                                                         
         LA    R1,HOOK             SET A(HEADLINE HOOK)                         
         ST    R1,HEADHOOK                                                      
         SPACE 1                                                                
         USING PRNTD,R2                                                         
         LA    R2,P                                                             
         SPACE 1                                                                
         MVC   AIO,AIO1            WILL GET COMMERCIALS INTO AIO1               
         SPACE 1                                                                
         USING TLCOD,R4                                                         
         LA    R4,KEY              READ ALL COMMERCIAL KEYS                     
         XC    KEY,KEY                                                          
         MVI   TLCOCD,TLCOCDQ                                                   
         GOTO1 HIGH                                                             
         B     PREP20                                                           
PREP10   GOTO1 SEQ                                                              
PREP20   LA    R4,KEY                                                           
         CLI   KEY,TLCOCDQ                                                      
         BNE   XIT                                                              
         SPACE 1                                                                
         MVC   TGAGY,TLCOAGY       SAVE AGENCY                                  
         MVC   TGCOM,TLCOCOM       AND INTERNAL COMMERCIAL NUMBER               
         DROP  R4                                                               
         SPACE 1                                                                
         GOTO1 GETREC              GET COMMERCIAL RECORD                        
         SPACE 1                                                                
         USING TACOD,R4                                                         
         L     R4,AIO1             GET COMMERCIAL DETAILS ELEMENT               
         MVI   ELCODE,TACOELQ                                                   
         BAS   RE,GETEL                                                         
         BNE   PREP10                                                           
         SPACE 1                                                                
         CLI   TACOTYPE,CTYSEAS2   ONLY CONSIDER SEASONAL (TYPE H)              
         BNE   PREP10              COMMERCIALS                                  
         OC    TACOAIR,TACOAIR     WITH FIRST SEASON FIRST AIR DATE SET         
         BZ    PREP10                                                           
         OC    TACOSAIR,TACOSAIR   AND SECOND SEASON FIRST AIR DATE             
         BNZ   PREP10              NOT SET                                      
         SPACE 1                                                                
         MVC   TGCID,TACOCID       SAVE COMMERCIAL ID                           
         SPACE 1                                                                
         GOTO1 DATCON,DMCB,(1,TACOAIR),(0,FSTAIR0) SAVE TWO FORMATS OF          
         MVC   FSTAIR1,TACOAIR                     FIRST AIR DATE               
         SPACE 1                                                                
         GOTO1 ADDAY,DMCB,FSTAIR0,DUB,413          CALCULATE FIRST AIR          
         GOTO1 DATCON,DMCB,(0,DUB),(1,WORK)        PLUS 59 WEEKS                
         SPACE 1                                                                
         CLC   TGTODAY1,WORK       FIRST AIR DATE MUST HAVE BEEN AT             
         BL    PREP10              LEAST 59 WEEKS AGO                           
         SPACE 1                                                                
         GOTO1 ADDAY,DMCB,(C'Y',FSTAIR0),DUB,1  CALCULATE 2ND SEASON            
         GOTO1 DATCON,DMCB,(0,DUB),(1,TACOSAIR) 1ST AIR DATE AND SAVE           
         GOTO1 DATCON,DMCB,(0,DUB),(8,SNDAIR8)                                  
         DROP  R4                                                               
         SPACE 1                                                                
         MVC   SVKEY,KEY           SAVE COMMERCIAL KEY                          
         SPACE 1                                                                
         MVC   AIO,AIO2            WILL GET INVOICES INTO AIO2                  
         SPACE 1                                                                
         USING TLINPD,R4                                                        
         LA    R4,KEY              READ ALL PAYMENT HISTORY KEYS                
         XC    KEY,KEY             FOR INVOICES FOR THIS COMMERCIAL             
         MVI   TLINPCD,TLINHCDQ                                                 
         MVC   TLINHCOM,TGCOM                                                   
         GOTO1 HIGH                                                             
         B     PREP40                                                           
PREP30   GOTO1 SEQ                                                              
PREP40   CLC   KEY(TLINHINV-TLINPD),KEYSAVE                                     
         BNE   PREP50                                                           
         SPACE 1                                                                
         GOTO1 GETREC              GET INVOICE RECORD                           
         SPACE 1                                                                
         USING TAPDD,R4                                                         
         L     R4,AIO2             GET PAYMENT DETAILS ELEMENT                  
         MVI   ELCODE,TAPDELQ                                                   
         BAS   RE,GETEL                                                         
         BNE   PREP30                                                           
         SPACE 1                                                                
         CLC   TAPDCYCS,FSTAIR1    ONLY CONSIDER IF CYCLE START IS              
         BL    PREP30              NOT EARLIER THAN FIRST AIR DATE              
         SPACE 1                                                                
         GOTO1 USEVAL,DMCB,TAPDUSE,TAPDTYPE                                     
         TM    TGUSSTA2,HLDTYPE    IF THIS IS A HOLDING FEE INVOICE             
         BZ    PREP30              THEN WE WILL UPDATE COMMERCIAL               
         DROP  R4                                                               
         SPACE 1                                                                
         MVC   KEY,SVKEY           RESTORE COMMERCIAL KEY                       
         GOTO1 HIGH                                                             
         CLC   KEY,SVKEY                                                        
         BNE   PREP50                                                           
         GOTO1 GETREC               RE-GET RECORD INTO AIO2 AND                 
         GOTO1 ASAVPTRS,DMCB,ASVPTRS   SAVE INTIAL PASSIVE POINTERS             
         SPACE 1                                                                
         MVC   PAGY,TGAGY          PRINT OUT INFORMATION                        
         MVC   PCID,TGCID                                                       
         GOTO1 DATCON,DMCB,(1,FSTAIR1),(8,PFAIR)                                
         MVC   PSAIR,SNDAIR8                                                    
         BAS   RE,PRINTIT                                                       
         SPACE 1                                                                
         MVI   TGBYTE,GETCOM       TRACE INITIAL RECORD                         
         BAS   RE,PTRACE                                                        
         SPACE 1                                                                
         MVC   AIO,AIO1            PUT BACK COMMERCIAL WITH UPDATED             
         GOTO1 PUTREC                       2ND SEASON 1ST AIR DATE             
         SPACE 1                                                                
         MVI   TGBYTE,PUTCOM       TRACE UPDATED RECORD                         
         BAS   RE,PTRACE                                                        
         SPACE 1                                                                
         MVI   BYTE,8                                                           
         TM    OPTION,OPTTRACE                                                  
         BZ    *+8                                                              
         OI    BYTE,X'10'                                                       
         GOTO1 AADDPTRS,DMCB,(BYTE,ASVPTRS),AUPPTRS AND UPDATE POINTERS         
         SPACE 1                                                                
PREP50   MVC   AIO,AIO1                                                         
         MVC   KEY,SVKEY                                                        
         GOTO1 HIGH                                                             
         B     PREP10                                                           
         DROP  R2                                                               
         EJECT                                                                  
*              HEADLINE HOOK (HEADHOOK)                                         
         SPACE 1                                                                
HOOK     NTR1                                                                   
         B     XIT                                                              
         SPACE 2                                                                
*              ROUTINE TO PRINT LINE OF OUTPUT                                  
         SPACE 1                                                                
PRINTIT  NTR1                                                                   
         GOTO1 SPOOL,DMCB,(R8)                                                  
         B     XIT                                                              
         SPACE 2                                                                
*              ROUTINE TO PRINT OUT TRACED RECORD                               
         SPACE 1                                                                
PTRACE   NTR1                                                                   
         TM    OPTION,OPTTRACE                                                  
         BZ    XIT                                                              
         SPACE 1                                                                
         MVI   FORCEHED,C'N'                                                    
         SPACE 1                                                                
         TM    TGBYTE,GETCOM                                                    
         BZ    PT10                                                             
         GOTO1 TRACE,DMCB,AIO,0,=C'GET COM',(0,7)                               
         SPACE 1                                                                
PT10     TM    TGBYTE,PUTCOM                                                    
         BZ    PTX                                                              
         GOTO1 TRACE,DMCB,AIO,0,=C'PUT COM',(0,7)                               
PTX      B     XIT                                                              
         EJECT                                                                  
*              ERROR ROUTINES                                                   
         SPACE 1                                                                
FLDINV   MVI   ERROR,INVALID       INVALID INPUT FIELD                          
         B     THEEND                                                           
         SPACE 1                                                                
THEEND   GOTO1 ERREX                                                            
         SPACE 2                                                                
*              CONDITION CODE & EXIT ROUTINE                                    
         SPACE 1                                                                
YES      SR    RC,RC                                                            
NO       LTR   RC,RC                                                            
         SPACE 1                                                                
         B     XIT                                                              
         EJECT                                                                  
*              STANDARD ROUTINES                                                
         SPACE 1                                                                
         GETEL  R4,DATADISP,ELCODE                                              
         SPACE 2                                                                
*              CONSTANTS, ETC.                                                  
         SPACE 1                                                                
OPTTRACE EQU   X'80'                                                            
         SPACE 1                                                                
GETCOM   EQU   X'80'                                                            
PUTCOM   EQU   X'40'                                                            
         LTORG                                                                  
         EJECT                                                                  
*              REPORT SPECIFICATIONS                                            
         SPACE 1                                                                
MYSPECS  SSPEC H1,2,RUN                                                         
         SSPEC H1,100,REPORT                                                    
         SSPEC H1,117,PAGE                                                      
         SSPEC H2,100,REQUESTOR                                                 
*                                                                               
         SSPEC H1,46,C'UPDATED SEASONAL COMMERCIALS WITH SECOND'                
         SSPEC H1,87,C'SEASON FIRST AIR DATE'                                   
*                                                                               
         SSPEC H7,02,C'AGENCY'                                                  
         SSPEC H8,02,C'------'                                                  
         SSPEC H7,10,C'COMMERCIAL'                                              
         SSPEC H8,10,C'------------'                                            
         SSPEC H7,24,C'FIRST SEASON FIRST AIR DATE'                             
         SSPEC H8,24,C'---------------------------'                             
         SSPEC H7,53,C'SECOND SEASON FIRST AIR DATE'                            
         SSPEC H8,53,C'----------------------------'                            
         DC    X'00'                                                            
         EJECT                                                                  
*              DSECT TO COVER LOCAL WORKING STORAGE                             
         SPACE 1                                                                
RECD     DSECT                                                                  
SVKEY    DS    XL(L'KEY)                                                        
         SPACE 1                                                                
FSTAIR0  DS    CL6                 FIRST AIR DATE - EBCDIC                      
FSTAIR1  DS    PL3                 FIRST AIR DATE - PWOS                        
         SPACE 1                                                                
SNDAIR8  DS    CL8                 SECOND SEASON FIRST AIR DATE                 
         SPACE 1                                                                
ASVPTRS  DS    A                   A(SAVED POINTER BLOCK)                       
AUPPTRS  DS    A                   A(UPDATED POINTER BLOCK)                     
         SPACE 2                                                                
*              DSECT TO COVER PRINT LINE                                        
         SPACE 1                                                                
PRNTD    DSECT                                                                  
         DS    CL1                                                              
PAGY     DS    CL6        AGENCY                                                
         DS    CL2                                                              
PCID     DS    CL12       COMMERCIAL                                            
         DS    CL2                                                              
PFAIR    DS    CL8        FIRST AIR DATE                                        
         DS    CL22                                                             
PSAIR    DS    CL8        NEW SECOND SEASON FIRST AIR DATE                      
         ORG                                                                    
         EJECT                                                                  
       ++INCLUDE TAREPFFD                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE TAREPE2D                                                       
         EJECT                                                                  
* DDGENTWA   (MUST FOLLOW LAST SCREEN)                                          
* DDPERVAL                                                                      
* DDSPOOLD                                                                      
* DDSPLWORKD                                                                    
* DDBIGBOX                                                                      
* TAGENFILE                                                                     
* TASYSDSECT                                                                    
* TASYSEQUS                                                                     
* TAREPWORKD                                                                    
         PRINT OFF                                                              
       ++INCLUDE DDGENTWA                                                       
       ++INCLUDE DDPERVALD                                                      
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE DDBIGBOX                                                       
       ++INCLUDE TAGENFILE                                                      
       ++INCLUDE TASYSDSECT                                                     
       ++INCLUDE TASYSEQUS                                                      
       ++INCLUDE TAREPWORKD                                                     
         PRINT ON                                                               
**********************************************************************          
*        DSECT FOR NMOD STORAGE GRAB AREA                            *          
**********************************************************************          
                                                                                
TMPD     DSECT                                                                  
SVPTRBLK DS    CL((320*L'TLDRREC)+1) PASSIVE + ACTIVE PTRS                      
UPPTRBLK DS    CL((320*L'TLDRREC)+1) PASSIVE + ACTIVE PTRS FOR ADDPTRS          
TMPLNQ   EQU   *-TMPD                                                           
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'002TAREP59   01/03/13'                                      
         END                                                                    
