*          DATA SET CTSFM1D    AT LEVEL 033 AS OF 05/01/02                      
*PHASE TA0A1DA                                                                  
*                                                                               
***********************************************************************         
*                                                                     *         
*  TITLE: TA0A1D - ZENITH CLIENT/REP RECORD MAINT/LIST                *         
*                                                                     *         
*  COMMENTS: MAINTAINS ADDS CLIENT /REP RECORDS FOR ZENITH            *         
*                                                                     *         
*                                                                     *         
***********************************************************************         
         TITLE 'TA0A1D ZENITH CLIENT/REP MAINTENANCE/LIST'                      
TA0A1D   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,TA0A1D**,R7,RR=R3                                              
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     R8,ASPOOLD          GENERAL PRINT AREAS                          
         USING SPOOLD,R8                                                        
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA      BASE SCREEN FOR SYSTEM + THIS PROG           
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         ST    R3,RELO                                                          
         MVI   ACTELOPT,C'Y'       CREATE ACTIVITY ELEMENT ON RECORDS           
*                                                                               
         CLI   MODE,VALKEY         VALIDATE KEY                                 
         BE    VK                                                               
         CLI   MODE,VALREC         VALIDATE RECORD                              
         BE    VR                                                               
         CLI   MODE,DISPKEY        DISPLAY KEY                                  
         BE    DK                                                               
         CLI   MODE,DISPREC        DISPLAY RECORD                               
         BE    DR                                                               
         CLI   MODE,LISTRECS       ONLINE LIST RECORDS                          
         BE    LR                                                               
         CLI   MODE,PRINTREP       PRINT REPORT                                 
         BE    LR                                                               
*                                                                               
EXIT     XIT1                                                                   
         EJECT                                                                  
* ********************************************************************          
* VK -   VALIDATE KEY ROUTINE                                                   
* ********************************************************************          
*                                                                               
VK       DS    0H                                                               
         XC    KEY,KEY                                                          
         LA    R3,KEY                                                           
         USING ZENRECD,R3                                                       
         MVI   ZENKCODE,ZENKCODQ    KEY SYSTEM X'05'                            
         MVI   ZENKTYP,ZENCLTQ     TYPE X'09' FOR CLIENT REC                    
         MVC   ZENKAGY,AGENCY      AGENCY                                       
*                                                                               
         CLI   ACTNUM,ACTLIST      IF ACTION LIST OR REPORT                     
         BE    VKX                                                              
         CLI   ACTNUM,ACTREP                                                    
         BE    VKX                 THAT'S ALL                                   
*                                                                               
         LA    R2,ZENCLTH          CLIENT                                       
         CLI   5(R2),0                                                          
         BE    MISSFLD                                                          
         MVC   ZENKCLT,8(R2)                                                    
         OC    ZENKCLT,=X'404040'                                               
*                                                                               
*                                                                               
VKX      DS    0H                                                               
         B     EXIT                                                             
         DROP  R3                                                               
         EJECT                                                                  
* ********************************************************************          
* VR -   VALIDATE RECORD ROUTINE                                                
* ********************************************************************          
*                                                                               
VR       DS    0H                                                               
*                                                                               
         LA    R2,ZENCLTNH         CLIENT NAME                                  
         CLI   5(R2),0                                                          
         BE    MISSFLD                                                          
*                                                                               
         CLI   ACTNUM,ACTADD                                                    
         BE    VR10                                                             
*                                                                               
         MVI   ELCODE,ZENELCDQ                                                  
         GOTO1 REMELEM                                                          
*                                                                               
VR10     DS    0H                  ADD DESCRIPTION ELEMENT                      
         LA    R6,ELEM                                                          
         USING ZENELEM,R6                                                       
         XC    ELEM,ELEM                                                        
         MVI   ZENELCD,ZENELCDQ                                                 
         MVI   ZENELLN,ZENELENE                                                 
         MVC   ZENCNAME,ZENCLTN                                                 
*                                                                               
***      B     VR20                TEMPORARY                                    
*                                                                               
         LA    R2,ZENACCOH                                                      
         CLI   ZENACCOH+5,0                                                     
         BE    VR20                                                             
         CLI   ZENACCOH+5,2                                                     
         BNE   INVLFLD                                                          
         MVC   ZENCAGOF,ZENACCO                                                 
*                                                                               
* - CODE BELOW ADD OFFICENUM/ACC AGY OVERRIDE                                   
**       XC    SCANOUT,SCANOUT                                                  
**       GOTO1 SCANNER,DMCB,ZENACCOH,(2,SCANOUT),C',=/-'                        
**       LA    R1,SCANOUT                                                       
**       CLI   0(R1),2             RIGHT JUSTIFY                                
**       BE    *+14                                                             
**       MVC   ZENCOFF+1(1),12(R1)                                              
**       B     *+10                                                             
**       MVC   ZENCOFF,12(R1)                                                   
**       MVC   ZENCAGOF,44(R1)                                                  
         DROP  R6                                                               
*                                                                               
VR20     DS    0H                                                               
         GOTO1 ADDELEM                                                          
         CLI   DMCB+12,0                                                        
         BE    VRX                                                              
         DC    H'0'                                                             
*                                                                               
*                                                                               
VRX      DS    0H                                                               
         B     EXIT                                                             
         EJECT                                                                  
* *******************************************************************           
* DR -   DISPLAY RECORD                                                         
* *******************************************************************           
*                                                                               
DR       DS    0H                                                               
*                                                                               
         LA    R4,ZENCLTNH                                                      
         TWAXC (R4)                CLEAR SCRN FIELDS                            
*                                                                               
         L     R6,AIO                                                           
         USING ZENRECD,R6                                                       
         MVI   ELCODE,ZENELCDQ                                                  
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING ZENELEM,R6                                                       
*                                                                               
         LA    R4,ZENCLTNH                                                      
         MVC   ZENCLTN,ZENCNAME                                                 
         FOUT  (R4)                                                             
*                                                                               
***      B     DRX                                                              
*                                                                               
         LA    R4,ZENACCO                                                       
         LA    R1,ZENCOFF                                                       
         OC    ZENCOFF(4),ZENCOFF                                               
         BZ    DRX                                                              
**       MVC   0(2,R4),0(R1)                                                    
**       LA    R4,2(R4)                                                         
**       MVI   0(R4),C'/'                                                       
         MVC   0(2,R4),ZENCAGOF                                                 
         LA    R4,ZENACCOH                                                      
         FOUT  (R4)                                                             
*                                                                               
DRX      DS    0H                                                               
         B     EXIT                                                             
         DROP  R6                                                               
         EJECT                                                                  
* ******************************************************************            
* DK -   DISPLAY KEY                                                            
* ******************************************************************            
*                                                                               
DK       DS    0H                                                               
         LA    R6,KEY                                                           
         USING ZENRECD,R6                                                       
*                                                                               
         LA    R4,ZENCLTH                                                       
         MVC   ZENCLT,ZENKCLT                                                   
         FOUT  (R4)                                                             
         B     EXIT                                                             
         DROP  R6                                                               
         EJECT                                                                  
* *********************************************************************         
* ONLINE LIST ROUTINE                                                           
* *********************************************************************         
*                                                                               
LR       DS    0H                                                               
         LA    R5,LISTAR                                                        
         USING PLINED,R5                                                        
         CLI   MODE,PRINTREP                                                    
         BNE   LR02                                                             
*                                                                               
         LA    R3,HEADING          SET UP REPORT HEADINGS                       
         ST    R3,SPECS                                                         
         LA    R3,HDRTN                                                         
         ST    R3,HEADHOOK                                                      
         LA    R5,P                                                             
*                                                                               
LR02     DS    0H                                                               
         LA    R4,KEY                                                           
         USING ZENRECD,R4                                                       
*                                                                               
*                                                                               
LR05     OC    KEY(L'ZENRKEY),KEY   FIRST TIME THROUGH?                         
         BNZ   LR10                                                             
         MVI   ZENKCODE,ZENKCODQ    X'05' REC ID                                
         MVI   ZENKTYP,ZENCLTQ     X'09' TYPE                                   
         MVC   ZENKAGY,AGENCY      AGENCY                                       
         LA    R2,LSTCLIH          CLIENT                                       
         CLI   5(R2),0                                                          
         BE    LR10                                                             
         MVC   ZENKCLT,ZENCLT                                                   
*                                                                               
LR10     GOTO1 HIGH                FIRST RECORD                                 
         B     LR40                                                             
*                                                                               
LRSEQ    GOTO1 SEQ                                                              
*                                                                               
LR40     CLI   ZENKCODE,ZENKCODQ    X'05'    RECORD                             
         BNE   LRX                                                              
         CLI   ZENKTYP,ZENCLTQ     X'09'    CLIENT                              
         BNE   LRX                                                              
         CLC   ZENKAGY,AGENCY               AGENCY                              
         BNE   LRX                                                              
*                                                                               
         L     R6,AIO                                                           
         USING ZENRECD,R6                                                       
         MVC   LISTAR,SPACES           CLEAR LIST LINE                          
         MVC   LISTAR+4(3),ZENKCLT     PASS CLIENT CODE                         
*                                                                               
         MVI   ELCODE,ZENELCDQ                                                  
         BAS   RE,GETEL                                                         
         BNE   LRSEQ                                                            
         USING ZENELEM,R6                                                       
*                                                                               
         MVC   LISTAR+8(20),ZENCNAME                                            
         MVC   LISTAR+30(2),ZENCAGOF                                            
*                                                                               
         DROP  R6                                                               
*                                                                               
LR65     CLI   MODE,PRINTREP                                                    
         BE    LR70                GO PRINT REPORT                              
         GOTO1 LISTMON             SEND LINE TO SCREEN                          
         B     LRSEQ                                                            
*                                                                               
* PRINT REPORT ROUTINE  * * * * * * * * * * * * * * * * * * * * * * *           
*                                                                               
LR70     DS    0H                                                               
         L     R6,AIO                                                           
         USING ZENRECD,R6                                                       
         MVC   P+2(L'ZENKCLT),ZENKCLT                                           
         MVI   ELCODE,ZENELCDQ                                                  
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING ZENELEM,R6                                                       
         MVC   P+7(L'ZENCNAME),ZENCNAME                                         
         MVC   P+30(2),ZENCAGOF                                                 
*                                                                               
LR80     GOTO1 SPOOL,DMCB,(R8)                                                  
         B     LRSEQ                                                            
*                                                                               
LRX      B     EXIT                                                             
         DROP  R4,R5                                                            
         EJECT                                                                  
* HEADER ROUTINE    * * * * * * * * * * * * * * * * * * * * * * * * *           
*                                                                               
HEADING  DS    0H                                                               
         SSPEC H1,1,REQUESTOR                                                   
         SSPEC H1,25,C'ZENITH RECORDS'                                          
         SSPEC H2,25,C'--------------'                                          
         SSPEC H1,55,AGYNAME                                                    
         SSPEC H2,55,AGYADD                                                     
         SSPEC H3,55,REPORT                                                     
         SSPEC H4,55,RUN                                                        
         SSPEC H5,55,PAGE                                                       
         DC    X'0'                                                             
         EJECT                                                                  
*                                                                               
HDRTN    NTR1                                                                   
         LA    R4,H8                                                            
         USING PLINED,R4                                                        
**       MVC   PLCLT(35),=C'CLT  CLIENT NAME            OFFICE'                 
**       LA    R4,H9                                                            
**       MVC   PLCLT(35),=C'---  -------------------    -------'                
         MVC   PLCLT(36),=C'CLT  CLIENT NAME           ACC AGY'                 
         LA    R4,H9                                                            
         MVC   PLCLT(36),=C'---  -------------------   --------'                
         B     EXIT                                                             
         DROP  R4                                                               
*                                                                               
RELO     DS    A                   RELOCATION FACTOR                            
*                                                                               
INVLFLD  MVC   GERROR,=AL2(INVALID)                                             
         B     VSFMERR                                                          
*                                                                               
MISSFLD  MVC   GERROR,=AL2(MISSING)                                             
         B     VSFMERR                                                          
*                                                                               
INVLMED  MVC   GERROR,=AL2(INVMED)                                              
         B     VSFMERR                                                          
*                                                                               
INVLSTA  MVC   GERROR,=AL2(INVSTA)                                              
         B     VSFMERR                                                          
*                                                                               
NOAUTHC  MVC   GERROR,=AL2(NOAUTHCH)                                            
         B     VSFMERR                                                          
*                                                                               
NOAUTHS  MVC   GERROR,=AL2(NOAUTHSH)                                            
         B     VSFMERR                                                          
*                                                                               
BADSWTC  MVC   GERROR,=AL2(BADSWTCH)                                            
         B     VSFMERR                                                          
*                                                                               
VSFMERR  GOTO1 SFMERR                                                           
*                                                                               
         SPACE 3                                                                
         GETEL R6,DATADISP,ELCODE                                               
         SPACE 3                                                                
***                                                                             
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
         PRINT OFF                                                              
       ++INCLUDE CTGENFILE                                                      
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDFLDIND                                                       
       ++INCLUDE DDCOMFACS                                                      
       ++INCLUDE CTSFMFFD                                                       
         PRINT ON                                                               
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE CTSFMACD                                                       
         ORG   CONTAGH                                                          
       ++INCLUDE CTSFMBCD                                                       
         EJECT                                                                  
       ++INCLUDE CTSFMWORKD                                                     
         EJECT                                                                  
*                                                                               
* TA0AAC STORAGE DSECT    * * * * * * * * * * * * * * * * * * * * * *           
*                                                                               
         ORG   SYSSPARE                                                         
SCANOUT  DS    XL200                                                            
FLAG     DS    X                                                                
*                                                                               
*                                                                               
PLINED   DSECT                                                                  
         DS    CL2                                                              
PLCLT    DS    CL3                                                              
         DS    CL2                                                              
PLCLTNM  DS    CL20                                                             
         DS    CL2                                                              
PLACCOFF DS    CL6                                                              
         DS    CL2                                                              
PEND     EQU   *                                                                
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'033CTSFM1D   05/01/02'                                      
         END                                                                    
