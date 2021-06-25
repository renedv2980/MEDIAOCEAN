*          DATA SET CTSFM1F    AT LEVEL 022 AS OF 05/01/02                      
*PHASE TA0A1FA                                                                  
*                                                                               
***********************************************************************         
*                                                                     *         
*  TITLE: TA0A1F - ZENITH REP RECORD MAINT/LIST                *                
*                                                                     *         
*  COMMENTS: MAINTAINS ADDS REP RECORDS FOR ZENITH            *                 
*                                                                     *         
*                                                                     *         
***********************************************************************         
         TITLE 'TA0A1F ZENITH REP MAINTENANCE/LIST'                             
TA0A1F   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,TA0A1F**,R7,RR=R3                                              
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
         MVI   ZENKTYP,ZENREPQ     TYPE X'0A' FOR REP REC                       
         MVC   ZENKAGY,AGENCY      ZENITH                                       
*                                                                               
*        CLI   ACTNUM,ACTLIST      IF ACTION LIST OR REPORT                     
*        BE    VKX                                                              
*        CLI   ACTNUM,ACTREP                                                    
*        BE    VKX                 THAT'S ALL                                   
*                                                                               
         LA    R2,ZENSYSH          SYSTEM                                       
         CLI   5(R2),0                                                          
         BE    MISSFLD                                                          
         CLI   8(R2),C'S'          SPOT                                         
         BE    VK10                                                             
         CLI   8(R2),C'N'          NETWORK                                      
         BE    VK10                                                             
         CLI   8(R2),C'P'          PRINT                                        
         BNE   MISSFLD                                                          
VK10     MVC   ZENKSYS,8(R2)                                                    
*                                                                               
         LA    R2,ZENMEDH          MEDIA                                        
         CLI   5(R2),0                                                          
         BE    MISSFLD                                                          
         CLI   ZENKSYS,C'S'        IF SPOT                                      
         BNE   VK20                                                             
         CLI   8(R2),C'T'                                                       
         BE    VK30                                                             
         CLI   8(R2),C'R'                                                       
         BE    VK30                                                             
         CLI   8(R2),C'X'                                                       
         BE    VK30                                                             
         B     INVLMED                                                          
*                                                                               
VK20     CLI   ZENKSYS,C'N'        IF NET                                       
         BNE   VK22                                                             
         CLI   8(R2),C'N'                                                       
         BE    VK30                                                             
         B     INVLMED                                                          
*                                                                               
VK22     CLI   ZENKSYS,C'P'        IF PRINT                                     
         BE    *+6                                                              
         DC    H'0'                SHOULD NEVER GET HERE                        
         CLI   8(R2),C'N'          NEWSPAPERS                                   
         BE    VK30                                                             
         CLI   8(R2),C'T'          TRADE                                        
         BE    VK30                                                             
         CLI   8(R2),C'S'          SUPPLEMENT                                   
         BE    VK30                                                             
         CLI   8(R2),C'M'          MAGS                                         
         BE    VK30                                                             
         CLI   8(R2),C'O'          OUTDOOR                                      
         BE    VK30                                                             
         B     INVLMED                                                          
VK30     MVC   ZENKMED,8(R2)                                                    
*                                                                               
         CLI   ACTNUM,ACTLIST      IF ACTION LIST OR REPORT                     
         BE    VKX                                                              
         CLI   ACTNUM,ACTREP                                                    
         BE    VKX                 THAT'S ALL                                   
                                                                                
*                                                                               
         LA    R2,ZENREPH          REP                                          
         CLI   5(R2),0                                                          
         BE    MISSFLD                                                          
         CLI   ZENKSYS,C'P'             ,,IF PRINT                              
         BNE   VK50                                                             
         MVC   ZENKPREP,8(R2)           ,,USE ZENKPREP                          
         OC    ZENKPREP,=X'40404040'                                            
         TM    4(R2),X'08'         PRINT REPS MUST BE NUMERIC                   
         BZ    INVLFLD                                                          
         ZIC   R1,5(R2)                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         PACK  WORK(4),8(0,R2)     FORCE INPUT=9 TO OUTPUT=009                  
         UNPK  ZENKPREP,WORK(4)                                                 
         B     VKX                                                              
                                                                                
VK50     MVC   ZENKREP,8(R2)             ,,ELSE USE ZENKREP                     
         OC    ZENKREP,=X'40404040'                                             
         CLI   5(R2),3             IF ALL 3 POSITION USED                       
         BE    VKX                 THAT'S ALL                                   
         TM    4(R2),X'08'         ELSE IF IT'S NUMERIC                         
         BZ    VKX                                                              
         ZIC   R1,5(R2)                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         PACK  WORK(4),8(0,R2)     FORCE INPUT=9 TO OUTPUT=009                  
         UNPK  ZENKREP,WORK(4)                                                  
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
         LA    R2,ZENRNAMH         REP NAME                                     
         CLI   5(R2),0                                                          
         BE    MISSFLD                                                          
*                                                                               
         LA    R2,ZENSTRTH         REP ADDRESS                                  
         CLI   5(R2),0                                                          
         BE    MISSFLD                                                          
*                                                                               
         LA    R2,ZENCITYH         CITY                                         
         CLI   5(R2),0                                                          
         BE    MISSFLD                                                          
*                                                                               
         LA    R2,ZENSTATH         STATE                                        
         CLI   5(R2),0                                                          
         BE    MISSFLD                                                          
*                                                                               
         LA    R2,ZENZIPH          ZIP                                          
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
         MVC   ZENREPNM,ZENRNAM                                                 
         MVC   ZENREPAD,ZENSTRT                                                 
         MVC   ZENRADR2,ZENCITY                                                 
         MVC   ZENRADR3,ZENSTAT                                                 
         MVC   ZENRADR4,ZENZIP                                                  
         DROP  R6                                                               
*                                                                               
         DS    0H                                                               
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
         LA    R4,ZENRNAMH                                                      
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
         LA    R4,ZENRNAMH                                                      
         MVC   ZENRNAM,ZENREPNM                                                 
         FOUT  (R4)                                                             
         LA    R4,ZENSTRTH                                                      
         MVC   ZENSTRT,ZENREPAD                                                 
         FOUT  (R4)                                                             
         LA    R4,ZENCITYH                                                      
         MVC   ZENCITY,ZENRADR2                                                 
         FOUT  (R4)                                                             
         LA    R4,ZENSTATH                                                      
         MVC   ZENSTAT,ZENRADR3                                                 
         FOUT  (R4)                                                             
         LA    R4,ZENZIPH                                                       
         MVC   ZENZIP,ZENRADR4                                                  
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
         LA    R4,ZENSYSH                                                       
         MVC   ZENSYS,ZENKSYS                                                   
         FOUT  (R4)                                                             
         LA    R4,ZENMEDH                                                       
         MVC   ZENMED,ZENKMED                                                   
         FOUT  (R4)                                                             
         LA    R4,ZENREPH                                                       
         XC    ZENREP,ZENREP                                                    
         MVC   ZENREP(3),ZENKREP                                                
         CLI   ZENKSYS,C'P'        IF PRINT                                     
         BNE   *+10                                                             
         MVC   ZENREP(4),ZENKPREP  USE PRINT FIELD                              
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
         MVI   ZENKTYP,ZENREPQ     X'0A' TYPE                                   
         MVC   ZENKAGY,AGENCY      ZENITH AGENCY                                
**       LA    R2,LSTREPH          CLIENT                                       
**       CLI   5(R2),0                                                          
**       BE    LR10                                                             
         MVC   ZENKSYS,ZENSYS                                                   
         MVC   ZENKMED,ZENMED                                                   
         MVC   ZENKREP,ZENREP                                                   
         CLI   ZENKSYS,C'P'                                                     
         BNE   *+10                                                             
         MVC   ZENKPREP,ZENREP                                                  
*                                                                               
LR10     GOTO1 HIGH                FIRST RECORD                                 
         B     LR40                                                             
*                                                                               
LRSEQ    GOTO1 SEQ                                                              
*                                                                               
LR40     CLI   ZENKCODE,ZENKCODQ    X'05'    RECORD                             
         BNE   LRX                                                              
         CLI   ZENKTYP,ZENREPQ     X'0A'    REP                                 
         BNE   LRX                                                              
         CLC   ZENKAGY,AGENCY               AGENCY                              
         BNE   LRX                                                              
         CLC   ZENKSYS,ZENSYS               SYSTEM                              
         BNE   LRX                                                              
         CLC   ZENKMED,ZENMED               MEDIA                               
         BNE   LRX                                                              
*                                                                               
         L     R6,AIO                                                           
         USING ZENRECD,R6                                                       
         MVC   LISTAR,SPACES           CLEAR LIST LINE                          
         MVC   LISTAR(3),ZENKREP       PASS REP CODE                            
         CLI   ZENKSYS,C'P'                                                     
         BNE   *+10                                                             
         MVC   LISTAR(4),ZENKPREP                                               
*                                                                               
         MVI   ELCODE,ZENELCDQ                                                  
         BAS   RE,GETEL                                                         
         BNE   LRSEQ                                                            
         USING ZENELEM,R6                                                       
*                                                                               
         MVC   LISTAR+8(22),ZENREPNM               NAME                         
         MVC   LISTAR+32(24),ZENREPAD              STREET ADDRESS               
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
         MVC   P+2(L'ZENKREP),ZENKREP                                           
         CLI   ZENKSYS,C'P'                                                     
         BNE   *+10                                                             
         MVC   P+2(4),ZENKPREP                                                  
         MVI   ELCODE,ZENELCDQ                                                  
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING ZENELEM,R6                                                       
         MVC   P+7(L'ZENREPNM),ZENREPNM                                         
         MVC   P+30(L'ZENREPAD),ZENREPAD                                        
         GOTO1 SPOOL,DMCB,(R8)                                                  
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
         MVC   PLCLT(33),=C'REP  REP NAME                ADDR'                  
         LA    R4,H9                                                            
         MVC   PLCLT(33),=C'---  ----------------------------'                  
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
       ++INCLUDE CTSFMADD                                                       
         ORG   CONTAGH                                                          
       ++INCLUDE CTSFMBDD                                                       
         EJECT                                                                  
       ++INCLUDE CTSFMWORKD                                                     
         EJECT                                                                  
*                                                                               
* TA0AAC STORAGE DSECT    * * * * * * * * * * * * * * * * * * * * * *           
*                                                                               
         ORG   SYSSPARE                                                         
MYWORK   DS    XL96                                                             
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
**PAN#1  DC    CL21'022CTSFM1F   05/01/02'                                      
         END                                                                    
