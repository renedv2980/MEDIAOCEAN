*          DATA SET REBAS02    AT LEVEL 073 AS OF 05/27/03                      
*PHASE T82402A,*                                                                
*INCLUDE NUMVAL                                                                 
*                                                                               
***********************************************************************         
*                                                                     *         
*  TITLE: T82402 - STATION DEFINITION MAINTENANCE                     *         
*                                                                     *         
*  CALLED FROM: REP CONTROLLER (T82400)                               *         
*                                                                     *         
*  INPUTS: SCREENS REBASAF  (T824AF) -- MAINTENANCE                   *         
*          SCREENS SPSFMD0  (T824D0) -- LIST                          *         
*                                                                     *         
*  LOCALS: REGISTER USAGE                                             *         
*          R0 - WORK                                                  *         
*          R1 - WORK                                                  *         
*          R2 - POINTER TO SCREEN FLDH                                *         
*          R3 - WORK                                                  *         
*          R4 - STATION RECORD                                        *         
*          R5 - WORK                                                  *         
*          R6 - GETEL REGISTER                                        *         
*          R7 - SECOND BASE REGISTER                                  *         
*          R8 - SPOOLD                                                *         
*          R9 - SYSD                                                  *         
*          RA - TWA                                                   *         
*          RB - FIRST BASE                                            *         
*          RC - GEND                                                  *         
*          RD - SYSTEM                                                *         
*          RE - SYSTEM                                                *         
*          RF - SYSTEM                                                *         
*                                                                     *         
***********************************************************************         
*                                                                               
         TITLE 'T82402 STATION MAINTENANCE'                                     
T82402   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,*T82402*,R7,RR=R3                                              
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA      BASE SCREEN FOR SYSTEM + THIS PROG           
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         L     R8,ASPOOLD          GENERAL PRINT AREAS                          
         USING SPOOLD,R8                                                        
         ST    R3,RELO                                                          
*                                                                               
         LA    R3,AFFLLIST                                                      
         ST    R3,ADDRLIST                                                      
*                                                                               
         LA    R3,TVBLST                                                        
         ST    R3,ATVBLST                                                       
*                                                                               
INVALREP EQU   102                                                              
NOGRPERR EQU   121                                                              
NOCHGERR EQU   123                                                              
COMNFND  EQU   277                 FILE COMMENT REC NOT FOUND                   
COMMIS   EQU   278                 COMMENT CODE MISSING                         
COMFENT  EQU   279                 CMT CDE MUST BE FIRST AND ONLY ENTRY         
LRECFND  EQU   283                 LIABILITY RECORD NOT FOUND                   
NOREPWDT EQU   300                 CAN'T ENTER NEW REP W/O LEAVE DATE           
OWNREQ   EQU   680                 OWNER REQUIRED                               
LCOMERR  EQU   681                 LIABILITY COMMENT                            
NOECTYP  EQU   683                 ELECTRONIC CONTRACT FORMAT                   
FAXMBERR EQU   684                 FAX VALUES MUST BE NUMERIC                   
MBOXERR  EQU   685                 NUMBER MUST BE 8 DIGITS                      
DESTERR  EQU   686                 INVALI ID, MAILBOX, OR FAX NUMBER            
GRPHERR  EQU   687                 GRAPH NOT ALLOWED IN THIS FIELD              
*                                                                               
         CLI   MODE,VALKEY         VALIDATE KEY                                 
         BNE   MAIN10                                                           
         GOTO1 =A(VK),DMCB,(RC),(RA),RR=RELO                                    
         BNZ   TRAPERR                                                          
         B     EXIT                                                             
MAIN10   EQU   *                                                                
         CLI   MODE,VALREC         VALIDATE RECORD                              
         BNE   MAIN20                                                           
         GOTO1 =A(VR),DMCB,(RC),(RA),RR=RELO                                    
         BNZ   TRAPERR                                                          
         B     EXIT                                                             
MAIN20   EQU   *                                                                
         CLI   MODE,DISPREC        DISPLAY RECORD                               
         BNE   MAIN30                                                           
         GOTO1 =A(DR),DMCB,(RC),(RA),RR=RELO                                    
         BNZ   TRAPERR                                                          
         B     EXIT                                                             
MAIN30   EQU   *                                                                
         CLI   MODE,DISPKEY        DISPLAY KEY                                  
         BNE   MAIN40                                                           
         GOTO1 =A(DK),DMCB,(RC),(RA),RR=RELO                                    
         BNZ   TRAPERR                                                          
         B     EXIT                                                             
MAIN40   EQU   *                                                                
         CLI   MODE,LISTRECS       ONLINE LIST RECORDS                          
         BNE   EXIT                                                             
         GOTO1 =A(LR),DMCB,(RC),(RA),RR=RELO                                    
         BNZ   TRAPERR                                                          
*                                                                               
EXIT     XIT1                                                                   
         SPACE                                                                  
TRAPERR  GOTO1 MYERROR                                                          
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
AFFLLIST DC    CL3'ABC'                                                         
         DC    CL3'NBC'                                                         
         DC    CL3'CBS'                                                         
         DC    CL3'IND'                                                         
         DC    CL3'UNI'                                                         
         DC    CL3'TEL'                                                         
         DC    CL3'FOX'                                                         
         DC    CL3'GAL'                                                         
         DC    CL3'UPN'                                                         
         DC    CL3'WBT'                                                         
         DC    X'00'                                                            
         DS    0H                                                               
*                                                                               
       ++INCLUDE RETVBTAB          TVB HARD CODED REGION TABLE                  
         EJECT                                                                  
**********************************************************************          
*                  VALIDATE KEY ROUTINE                              *          
**********************************************************************          
VK       NMOD1 0,*T82402*                                                       
         L     RC,0(R1)                                                         
         L     RA,4(R1)            TWA                                          
*                                                                               
         CLI   ACTNUM,ACTLIST      ONLINE LIST RECORDS                          
         BE    VKGOOD                                                           
*                                                                               
         LA    R2,FMSSTAH                                                       
         GOTO1 ANY                 REQUIRED                                     
*                                                                               
* VALIDATE STATION CALL LETTERS                                                 
*                                                                               
         XC    BLOCK(256),BLOCK                                                 
         MVC   WORK(50),SPACES                                                  
         GOTO1 SCANNER,DMCB,(R2),(2,BLOCK),C',=,-'                              
         MVC   RERROR,=AL2(INVSTA)                                              
         LA    R5,BLOCK                                                         
*                                                                               
         CLI   0(R5),3                                                          
         BL    VKBAD                                                            
         CLI   0(R5),4                                                          
         BH    VKBAD                                                            
         TM    2(R5),X'40'         TEST ALPHA                                   
         BZ    VKBAD                                                            
         MVC   WORK(4),12(R5)      SAVE CALL LETTERS                            
*                                                                               
         SR    RE,RE                                                            
         ICM   RE,1,1(R5)          DEFAULT = TV                                 
         BZ    VK100               YES                                          
         BCTR  RE,0                                                             
         EX    RE,STATV            TV LEAVE BLANK                               
         BE    VK100                                                            
         MVI   WORK+4,C'L'         L = LO POWER                                 
         EX    RE,STALP                                                         
         BE    VK100                                                            
         MVI   WORK+4,C'A'         AM = A                                       
         EX    RE,STAAM                                                         
         BE    VK100                                                            
         MVI   WORK+4,C'F'         FM = F                                       
         EX    RE,STAFM                                                         
         BE    VK100                                                            
         MVI   WORK+4,C'C'         CM = C                                       
         EX    RE,STACM                                                         
         BE    VK100                                                            
         MVI   WORK+4,C' '         MAY BE SATELLITE STATION                     
         EX    RE,STA1                                                          
         BNE   VK50                                                             
         MVI   WORK+4,C'1'                                                      
         B     VK100                                                            
VK50     EX    RE,STA2                                                          
         BNE   VKBAD                                                            
         MVI   WORK+4,C'2'                                                      
*                                                                               
VK100    EQU   *                                                                
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING RSTAKEY,R4                                                       
         MVI   RSTAKTYP,X'02'                                                   
         MVC   RSTAKREP,AGENCY     REP                                          
         MVC   RSTAKSTA(4),WORK    STATION                                      
         OC    RSTAKSTA,SPACES                                                  
         CLI   WORK+4,C'1'         DON'T FILL FOR SATELLITES                    
         BE    VKGOOD                                                           
         CLI   WORK+4,C'2'                                                      
         BE    VKGOOD                                                           
         MVC   RSTAKSTA+4(1),WORK+4                                             
VKGOOD   EQU   *                                                                
         SR    R0,R0                                                            
         B     VKEX                                                             
VKBAD    EQU   *                                                                
         LA    R0,1                                                             
VKEX     EQU   *                                                                
         LTR   R0,R0               SET THE CONDITION CODE                       
         XIT1                                                                   
         DROP  R4                                                               
         EJECT                                                                  
STATV    CLC   22(0,R5),=C'TV'                                                  
STAAM    CLC   22(0,R5),=C'AM'                                                  
STAFM    CLC   22(0,R5),=C'FM'                                                  
STACM    CLC   22(0,R5),=C'CM'                                                  
STALP    CLC   22(0,R5),=C'L'                                                   
STA1     CLC   22(0,R5),=C'1'                                                   
STA2     CLC   22(0,R5),=C'2'                                                   
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
**********************************************************************          
*                  DISPLAY RECORD                                    *          
**********************************************************************          
DR       NMOD1 0,*DISREC*                                                       
         L     RC,0(R1)                                                         
         L     RA,4(R1)                                                         
*                                                                               
         BAS   RE,CLEAR            CLEAR FIELDS                                 
*                                                                               
         L     R4,AIO                                                           
         USING RSTAREC,R4                                                       
*                                                                               
         MVC   SVCLDT,RSTACLDT                                                  
*                                                                               
         LA    R2,FMSMKH                                                        
         MVC   8(L'FMSMK,R2),RSTAMKT                                            
         FOUT  (R2)                                                             
*                                                                               
         EJECT                                                                  
* CONTRACT TO STATION?                                                          
         LA    R2,FMSCSTH                                                       
         MVC   8(3,R2),=C'YES'                                                  
         TM    RSTASTAT,X'08'                                                   
         BZ    *+10                                                             
         MVC   8(3,R2),=C'NO '                                                  
         FOUT  (R2)                                                             
*                                                                               
         EJECT                                                                  
* RECAP?                                                                        
         LA    R2,FMSCAPH                                                       
         MVC   8(3,R2),=C'YES'                                                  
         TM    RSTASTAT,X'04'                                                   
         BZ    *+10                                                             
         MVC   8(3,R2),=C'NO '                                                  
         FOUT  (R2)                                                             
*                                                                               
         EJECT                                                                  
* CHANNEL                                                                       
         LA    R2,FMSCHH                                                        
         EDIT  RSTACHAN,(4,8(R2)),ALIGN=LEFT                                    
         FOUT  (R2)                                                             
*                                                                               
         EJECT                                                                  
* GROUP/SUBGROUP                                                                
         LA    R2,FMSGSGH                                                       
         MVC   8(L'FMSGSG,R2),RSTAGRUP                                          
         MVC   SVGRUP,RSTAGRUP                                                  
         FOUT  (R2)                                                             
*                                                                               
         EJECT                                                                  
* EXPAND GROUP/SUBGROUP                                                         
         ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
*                                                                               
         XC    KEY,KEY                                                          
         MVI   KEY,7                                                            
         MVC   KEY+23(2),AGENCY                                                 
         MVC   KEY+25(2),RSTAGRUP                                               
         GOTO1 HIGH                                                             
         CLC   KEYSAVE(27),KEY                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         DROP  R4                                                               
         MVC   AIO,AIO2                                                         
         GOTO1 GETREC                                                           
         L     R4,AIO                                                           
         USING RGRPREC,R4                                                       
         MVC   8(20,R2),RGRPNAME                                                
         FOUT  (R2)                                                             
         DROP  R4                                                               
*                                                                               
         EJECT                                                                  
* TRAFFIC FORMAT                                                                
         MVC   AIO,AIO1            RESTORE                                      
         L     R4,AIO                                                           
         USING RSTAREC,R4                                                       
*                                                                               
         LA    R2,FMSTRFH                                                       
         MVC   8(L'FMSTRF,R2),SPACES                                            
         CLI   RSTATRAF,0                                                       
         BE    *+10                                                             
         MVC   8(L'RSTATRAF,R2),RSTATRAF                                        
         FOUT  (R2)                                                             
*                                                                               
         EJECT                                                                  
* TVB REGION                                                                    
         LA    R2,FMSTVBH                                                       
         MVC   8(L'FMSTVB,R2),SPACES                                            
         FOUT  (R2)                                                             
         CLI   RSTATVB,0                                                        
         BE    DISP0040                                                         
         L     R3,ATVBLST                                                       
*                                                                               
         CLC   0(2,R3),RSTATVB                                                  
         BE    *+18                                                             
         LA    R3,20(R3)                                                        
         CLI   0(R3),X'FF'                                                      
         BNE   *-18                                                             
         DC    H'0'                                                             
                                                                                
         MVC   8(2,R2),0(R3)                                                    
         FOUT  (R2)                                                             
*                                                                               
         EJECT                                                                  
* EXPAND TVB NAME                                                               
DISP0040 ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         MVC   8(20,R2),SPACES                                                  
         CLI   RSTATVB,0                                                        
         BE    *+10                                                             
         MVC   8(18,R2),2(R3)                                                   
         FOUT  (R2)                                                             
*                                                                               
         EJECT                                                                  
* JOIN DATE                                                                     
         LA    R2,FMSJDH                                                        
         GOTO1 DATCON,DMCB,(3,RSTASTRT),(8,8(R2))                               
         FOUT  (R2)                                                             
*                                                                               
         EJECT                                                                  
* LEAVE DATE                                                                    
         LA    R2,FMSLDH                                                        
         MVC   8(L'FMSLD,R2),SPACES                                             
         OC    RSTAEND,RSTAEND                                                  
         BZ    DISP0060                                                         
         GOTO1 DATCON,DMCB,(3,RSTAEND),(8,8(R2))                                
DISP0060 FOUT  (R2)                                                             
*                                                                               
         EJECT                                                                  
* OWNER                                                                         
         LA    R2,FMSOWNH                                                       
         MVC   8(L'FMSOWN,R2),SPACES                                            
         FOUT  (R2)                                                             
         OC    RSTAOWN,RSTAOWN                                                  
         BZ    *+10                                                             
         MVC   8(3,R2),RSTAOWN                                                  
         FOUT  (R2)                                                             
*                                                                               
         EJECT                                                                  
* EXPAND OWNER NAME                                                             
         ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         SPACE 1                                                                
         MVC   8(20,R2),SPACES                                                  
         OC    RSTAOWN,RSTAOWN                                                  
         BZ    DISP0080                                                         
* READ OWNERSHIP RECORD FOR NAME                                                
         MVC   AIO,AIO2                                                         
         SPACE 1                                                                
         XC    KEY,KEY                                                          
         MVI   KEY,X'2A'                                                        
         MVC   KEY+22(2),AGENCY                                                 
         MVC   KEY+24(3),RSTAOWN                                                
         GOTO1 HIGH                                                             
         CLC   KEYSAVE(27),KEY                                                  
         BE    DISP0070            FOUND                                        
         MVC   8(17,R2),=C'OWNER NOT ON FILE'                                   
         B     DISP0075                                                         
DISP0070 EQU   *                                                                
         GOTO1 GETREC                                                           
         DROP  R4                                                               
         L     R4,AIO              AIO=AIO2                                     
         USING ROWNREC,R4                                                       
         MVC   8(20,R2),ROWNNAME                                                
         DROP  R4                                                               
DISP0075 EQU   *                                                                
         MVC   AIO,AIO1            RESTORE                                      
DISP0080 FOUT  (R2)                                                             
*                                                                               
         EJECT                                                                  
* RANK                                                                          
         L     R4,AIO                                                           
         USING RSTAREC,R4                                                       
*                                                                               
         LA    R2,FMSRNKH                                                       
         MVC   8(1,R2),SPACES                                                   
         FOUT  (R2)                                                             
         CLI   RSTARANK,0                                                       
         BE    *+10                                                             
         MVC   8(1,R2),RSTARANK                                                 
         FOUT  (R2)                                                             
*                                                                               
         EJECT                                                                  
* STATUS                                                                        
         LA    R2,FMSSTH                                                        
         TM    RSTASTAT,X'F1'      ANY STATUS BITS ON (OTHER THAN               
*                        08-CON TO STATION, 04-RECAP, 02-DON'T SEND)            
         BZ    *+8                 NO                                           
         BAS   RE,SUNSCAN          DISPLAY STATUS FIELD                         
*                                                                               
         EJECT                                                                  
* FORMER REP/NEW REP                                                            
         BAS   RE,FNFMT                                                         
*                                                                               
         EJECT                                                                  
* RECEIVING ID                                                                  
         LA    R2,FMSRIDH                                                       
         BAS   RE,RIDFMT                                                        
*                                                                               
         EJECT                                                                  
* SIGN ON ID'S                                                                  
         LA    R2,FMSSIDH                                                       
         BAS   RE,SOFMT                                                         
*                                                                               
         EJECT                                                                  
* PRIMARY AFFILIATE                                                             
         LA    R2,FMSAFH                                                        
         MVC   8(L'RSTAAFFL,R2),RSTAAFFL                                        
         FOUT  (R2)                                                             
*                                                                               
         EJECT                                                                  
* DISPLAY ALL DATA FROM X'08' ELEM                                              
         BAS   RE,OPTDIS                                                        
*                                                                               
         BAS   RE,DISPCOMS                                                      
*                                                                               
         EJECT                                                                  
* RADIO COMBOS                                                                  
         OI    FMSCMH+6,X'80'                                                   
         OI    FMSCM1H+6,X'80'                                                  
         OI    FMSCM2H+6,X'80'                                                  
         OI    FMSCM3H+6,X'80'                                                  
         OI    FMSCM4H+6,X'80'                                                  
         FOUT  FMSDC1H,SPACES,8    CLEAR/XMIT COMBO STATION DATES               
         FOUT  FMSDC2H,SPACES,8                                                 
         FOUT  FMSDC3H,SPACES,8                                                 
         FOUT  FMSDC4H,SPACES,8                                                 
         L     R6,AIO                                                           
         MVI   ELCODE,X'0A'                                                     
         BAS   RE,GETEL                                                         
         BNE   DISP0200                                                         
         USING RSTACSEL,R6                                                      
*                                                                               
         CLI   RSTAKSTA+4,C'C'     IS THIS A COMBO STA?                         
         BE    DISP0100            YES - DISPLAY UP TO 4 CHILDREN               
*                                                                               
         MVC   FMSCM(4),RSTACS     NO  - DISPLAY 1 PARENT                       
         MVC   FMSCM+4(2),=C'-C'                                                
         B     DISP0200                                                         
DISP0100 DS    0H                                                               
         LA    R2,FMSCM1H          FIRST COMBO FIELD                            
         LA    R3,FMSDC1H          FIRST COMBO DATE                             
DISP0120 MVC   8(4,R2),RSTACS                                                   
         MVI   8+4(R2),C'-'                                                     
         MVC   8+5(1,R2),RSTACS+4                                               
         CLI   RSTACPRF,C'*'       PREFERRED STATION?                           
         BNE   DISP0140            NO                                           
         MVI   8+6(R2),C'*'        YES - SET FLAG                               
DISP0140 EQU   *                                                                
         CLI   RSTACPRF,C'-'       MINUSED STATION?                             
         BNE   DISP0160            NO                                           
         MVI   8+6(R2),C'-'        YES - SET FLAG                               
DISP0160 EQU   *                                                                
         LR    RF,RA               CHECK FOR DDS TERMINAL                       
         USING TWAD,RF                                                          
         CLI   TWAOFFC,C'*'        DDS TERMINAL?                                
*                                                                               
         DROP  RF                                                               
*                                                                               
         BNE   DISP0180            NO                                           
         CLI   1(R6),8             CHECK L(COMBO STATION ELEMENT)               
         BNH   DISP0180            NO DATE IN ELEMENT                           
         OC    8(2,R6),8(R6)       ANY DATE IN ELEMENT?                         
         BZ    DISP0180            NO                                           
         GOTO1 DATCON,DMCB,(2,RSTACDTE),(5,8(R3))                               
DISP0180 EQU   *                                                                
         FOUT  (R2)                                                             
         FOUT  (R3)                                                             
*                                                                               
         ZIC   R0,1(R6)            BUMP TO NEXT ELEMENT                         
         AR    R6,R0                                                            
         CLI   0(R6),X'0A'         COMBO STATION ELEMENT?                       
         BNE   DISP0200            NO  -                                        
         GOTO1 =A(NEXTUF),DMCB,(R2),RR=RELO  GET NEXT UNPR. FIELD               
         ZIC   R0,0(R3)            BUMP TO NEXT DATE FIELD                      
         AR    R3,R0                  ADD LENGTH OF SCREEN FIELD!               
         B     DISP0120            GO BACK FOR NEXT                             
*                                                                               
         DROP  R6                                                               
DISP0200 DS    0H                                                               
*                                                                               
         EJECT                                                                  
* TWX FIELD                                                                     
         LA    R2,FMSTWXH                                                       
         OI    6(R2),X'80'                                                      
         L     R6,AIO                                                           
         MVI   ELCODE,X'07'                                                     
         BAS   RE,GETEL                                                         
         BNE   DISP0220                                                         
*                                                                               
         USING RSTATWXL,R6                                                      
         MVC   8(20,R2),RSTATWX                                                 
         DROP  R6                                                               
*                                                                               
DISP0220 DS    0H                                                               
*                                                                               
         EJECT                                                                  
* LIABILITY FIELD                                                               
         LA    R2,FMSLIBH                                                       
         OI    6(R2),X'80'                                                      
         MVC   FMSLIC(L'FMSLIC),SPACES                                          
         FOUT  FMSLICH                                                          
         OC    RSTALIAB,RSTALIAB                                                
         BZ    DPGOOD                                                           
         EDIT  (B1,RSTALIAB),(2,8(R2)),FILL=0                                   
*                                                                               
         XC    DUB(L'RCMTKCDE),DUB                                              
         MVC   DUB(4),=C'LIAB'                                                  
         MVC   DUB+4(2),8(R2)                                                   
         GOTO1 =A(DISPFC),DMCB,(RC),(RA),DUB,(L'FMSLIC,FMSLIC),        +        
               RR=RELO                                                          
         FOUT  FMSLICH             XMIT                                         
*                                                                               
DPGOOD   DS    0H                                                               
*                                                                               
         SR    R0,R0                                                            
         B     DPEX                                                             
*                                                                               
DPBAD    LA    R0,1                                                             
*                                                                               
DPEX     EQU   *                                                                
         LTR   R0,R0               SETS THE CONDITION CODE                      
         XIT1                                                                   
         EJECT                                                                  
         SPACE 2                                                                
********************************************************************            
* SUB-ROUTINE TO DISPLAY STATUS FIELD ON STATION SCREEN *                       
********************************************************************            
SUNSCAN  NTR1                                                                   
         MVC   WORK(60),SPACES    PRECLEAR FIELD FOR UNSCAN                     
         LA    R5,WORK                                                          
         SR    R3,R3               COUNT OF FIELDS                              
         SPACE                                                                  
         TM    RSTASTAT,X'80'                                                   
         BZ    US10                                                             
         MVC   0(3,R5),=C'BOP'                                                  
         MVI   10(R5),C'N'                                                      
         LA    R5,20(R5)                                                        
         LA    R3,1(R3)            INCREMENT FIELD COUNT                        
         SPACE                                                                  
US10     EQU   *                                                                
         TM    RSTASTAT,X'40'                                                   
         BZ    US20                                                             
         MVC   0(8,R5),=C'CONTRACT'                                             
         MVI   10(R5),C'N'                                                      
         LA    R5,20(R5)                                                        
         LA    R3,1(R3)                                                         
         SPACE                                                                  
US20     DS    0H                                                               
         TM    RSTASTAT,X'20'                                                   
         B     US30                AVAILS=NEW IS SKIPPED!!                      
***>>>   BZ    US30                                                             
         MVC   0(8,R5),=CL8'AVAILS'                                             
         MVC   10(3,R5),=C'NEW'                                                 
         LA    R5,20(R5)                                                        
         LA    R3,1(R3)                                                         
         SPACE                                                                  
US30     DS    0H                                                               
         TM    RSTASTAT,X'10'                                                   
         BZ    USX                                                              
         MVC   0(8,R5),=CL8'BUDGET'                                             
         MVC   10(3,R5),=C'YES'                                                 
         LA    R5,20(R5)                                                        
         LA    R3,1(R3)                                                         
         SPACE 1                                                                
USX      EQU   *                                                                
         GOTO1 UNSCAN,DMCB,((R3),WORK),(R2),0                                   
         OI    6(R2),X'80'                                                      
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
*******************************************************************             
*   SUBROUTINE TO FORMAT RECEIVING ID FIELD                       *             
*******************************************************************             
RIDFMT   NTR1                                                                   
         L     R6,AIO                                                           
         MVI   ELCODE,X'05'                                                     
         BAS   RE,GETEL                                                         
         BNE   RFMTEX                                                           
         USING RSTAXEL,R6                                                       
         MVC   8(L'RSTARSO,R2),RSTARSO                                          
         FOUT  (R2)                                                             
RFMTEX   EQU   *                                                                
         XIT1                                                                   
         DROP  R6                                                               
         EJECT                                                                  
********************************************************************            
*  SUBROUTING TO FORMAT SIGN-ON ID FIELD                                        
* MOVE ALL SIGN-ON IDS INTO DUMMY FIELD AND THEN MOVE DATA INTO                 
* THE SCREEN FIELD FOR THE LENGTH OF THE FIELD                                  
********************************************************************            
SOFMT    NTR1                                                                   
         XC    TEMPIDS(40),TEMPIDS       DUMMY FIELD                            
         LA    R3,TEMPIDS                                                       
         L     R6,AIO                                                           
         MVI   ELCODE,X'06'                                                     
         BAS   RE,GETEL                                                         
         BNE   SOF40                                                            
         USING RSTASOEL,R6                                                      
         B     SOF15                                                            
*                                                                               
SOF10    BAS   RE,NEXTEL                                                        
         BNE   SOF40                                                            
*                                                                               
SOF15    EQU   *                                                                
         MVC   0(L'RSTASO,R3),RSTASO                                            
         LA    R3,7(R3)                                                         
SOF20    CLI   0(R3),C' '          GET RID OF EXTRA SPACES AT THE END           
         BNE   SOF30               OF SIGN ON ID                                
         BCTR  R3,0                                                             
         B     SOF20                                                            
         SPACE 1                                                                
SOF30    EQU   *                                                                
         LA    R3,1(R3)                                                         
         MVI   0(R3),C','                                                       
         LA    R3,1(R3)                                                         
         B     SOF10                                                            
         SPACE 1                                                                
SOF40    BCTR  R3,0                                                             
         CLI   0(R3),C','                                                       
         BNE   *+8                                                              
         MVI   0(R3),C' '                                                       
         MVC   8(L'FMSSID,R2),TEMPIDS                                           
         FOUT  (R2)                                                             
         XIT1                                                                   
         DROP  R6                                                               
         EJECT                                                                  
******************************************************************              
*        OPTDIS - DISPLAY ALL FIELDS ON THE STATION RECORD                      
*                 EXTENDED DESCRIPTION ELEMENT                                  
******************************************************************              
OPTDIS   NTR1                                                                   
         FOUT  FMSOPTH,=C'NNNNNNNNNNNNNNNNN',17  OPTIONS                        
         FOUT  FMSECH,=C'NO ',3               ELECTRONIC CON                    
         FOUT  FMSINVH,=C'NO ',3              INVOICE                           
         OI    FMSRDSH+6,X'80'                                                  
         OI    FMSFAXH+6,X'80'                                                  
         OI    FMSINTH+6,X'80'                                                  
         OI    FMSRWSH+6,X'80'                                                  
         OI    FMSMCDH+6,X'80'                                                  
*                                                                               
         ZIC   RF,FMSMCDH                                                       
         LA    R2,FMSMCDH(RF)                                                   
         OI    6(R2),X'80'                                                      
*                                                                               
         OI    FMSAF2H+6,X'80'                                                  
         OI    FMSTZH+6,X'80'                                                   
         OI    FMSLUH+6,X'80'                                                   
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,X'08'                                                     
         BAS   RE,GETEL                                                         
         BNE   OPTDGOOD                                                         
         USING RSTAXXEL,R6                                                      
*                                                                               
         MVC   FMSOPT(9),RSTAOPTS     OPTIONS                                   
         LA    R3,FMSOPT+9                                                      
         LA    R5,RSTAOPTA                                                      
         BAS   RE,DOPROF                                                        
*                                                                               
* IF THERE IS A 2ND FAX #, WE WILL OVERWIRTE THE DESTINATION FIELD              
* WITH THE 2ND FAX NUMBER                                                       
         MVC   FMSRDS(L'RSTAORDS),RSTAORDS  DESTINATION ID                      
         OC    RSTAOFX2,RSTAOFX2   DO WE HAVE A SECOND FAX #?                   
         BZ    OPTD0008            NO, SKIP                                     
         CLI   RSTAOFX2,X'00'      FIRST BYTE EMPTY?                            
         BNE   OPTD0006            NO  - JUST PUT OUT WHAT'S THERE              
         CLI   RSTAOFX2+1,X'00'    ANYTHING IN SECOND BYTE?                     
         BE    OPTD0006            NO  - JUST PUT OUT WHAT'S THERE              
         ZIC   R3,RSTAOFX2+1       INTERNATIONAL CODE                           
         EDIT  (R3),(3,FMSRDS),FILL=0                                           
         UNPK  WORK(16),RSTAOFX2+3(8)                                           
         ZIC   RF,RSTAOFX2+2       LENGTH OF SIGNIFICANT DIGITS                 
         LA    RE,16               MAXIMUM LENGTH OF FIELD                      
         SR    RE,RF               GET SIGNIFICANT OFFSET                       
         LA    RF,WORK             A(UNPACKED NUMBER)                           
         AR    RF,RE               ADD OFFSET                                   
         ZIC   RE,RSTAOFX2+2       GET LENGTH OF FAX# FIELD AGAIN               
         BCTR  RE,0                DECREMENT FOR EX                             
         EX    RE,OPTD0004         MOVE BY LENGTH                               
         B     OPTD0008                                                         
OPTD0004 MVC   FMSRDS+3(0),0(RF)                                                
*                                                                               
OPTD0006 EQU   *                                                                
         MVC   FMSRDS(L'RSTAOFX2),RSTAOFX2     FAX NUMBER                       
OPTD0008 DS    0H                                                               
         CLI   RSTAOFAX,X'00'      FIRST BYTE EMPTY?                            
         BNE   OPTD0030            NO  - JUST PUT OUT WHAT'S THERE              
         CLI   RSTAOFAX+1,X'00'    ANYTHING IN SECOND BYTE?                     
         BE    OPTD0030            NO  - JUST PUT OUT WHAT'S THERE              
         ZIC   R3,RSTAOFAX+1       INTERNATIONAL CODE                           
         EDIT  (R3),(3,FMSFAX),FILL=0                                           
         UNPK  WORK(16),RSTAOFAX+3(8)                                           
         ZIC   RF,RSTAOFAX+2       LENGTH OF SIGNIFICANT DIGITS                 
         LA    RE,16               MAXIMUM LENGTH OF FIELD                      
         SR    RE,RF               GET SIGNIFICANT OFFSET                       
         LA    RF,WORK             A(UNPACKED NUMBER)                           
         AR    RF,RE               ADD OFFSET                                   
         ZIC   RE,RSTAOFAX+2       GET LENGTH OF FAX# FIELD AGAIN               
         BCTR  RE,0                DECREMENT FOR EX                             
         EX    RE,OPTD0010         MOVE BY LENGTH                               
         B     OPTD0035                                                         
OPTD0010 MVC   FMSFAX+3(0),0(RF)                                                
*                                                                               
OPTD0030 EQU   *                                                                
         MVC   FMSFAX(L'RSTAOFAX),RSTAOFAX     FAX NUMBER                       
OPTD0035 EQU   *                                                                
         MVC   FMSINT(L'RSTAOSI),RSTAOSI       A/R INTERFACE CODE               
         MVC   FMSRWS,RSTARWS                  FMT AND CONTRACT FILTER          
*                                                                               
* MARKET CODE                                                                   
         ZIC   RF,FMSMCDH                                                       
         LA    R3,FMSMCDH(RF)                                                   
         CLC   RSTAMKTC(4),SPACES                                               
         BE    OPTD0040                                                         
         OC    RSTAMKTC(4),RSTAMKTC                                             
         BZ    OPTD0040                                                         
         MVC   FMSMCD,RSTAMKTC                                                  
         GOTO1 =A(DISPMKT),DMCB,(RC),(RA),RR=RELO                               
OPTD0040 EQU   *                                                                
         FOUT  (R3)                                                             
*                                                                               
         MVC   FMSAF2,RSTAAFL2                 SECONDARY AFFILIATE              
         MVC   FMSTZ,RSTATZ                    TIME ZONE                        
*                                                                               
         TM    RSTAXOPT,X'80'                  ELECTRONIC CON                   
         BZ    *+10                                                             
         MVC   FMSEC,=C'YES'                                                    
*                                                                               
         TM    RSTAXOPT,X'40'                  INVOICE                          
         BZ    *+10                                                             
         MVC   FMSINV,=C'YES'                                                   
*                                                                               
         MVC   FMSLU,RSTALUID                  LUID                             
*                                                                               
OPTDGOOD EQU   *                                                                
         XIT1                                                                   
*                                                                               
         EJECT                                                                  
*********************************************************************           
* ROUTINE TO DISPLAY PROFILE BITS                                               
* NOTE: R4 MUST ADDRESS BYTE TO BE DISPLAYED (8 OPTION BITS/BYTE)               
*       R3 MUST ADDRESS 8-BYTE DISPLAY AREA ON SCREEN                           
*********************************************************************           
DOPROF   NTR1                                                                   
         MVC   0(8,R3),=C'NNNNNNNN'                                             
         TM    0(R5),X'80'                                                      
         BZ    *+8                                                              
         MVI   0(R3),C'Y'                                                       
         TM    0(R5),X'40'                                                      
         BZ    *+8                                                              
         MVI   1(R3),C'Y'                                                       
         TM    0(R5),X'20'                                                      
         BZ    *+8                                                              
         MVI   2(R3),C'Y'                                                       
         TM    0(R5),X'10'                                                      
         BZ    *+8                                                              
         MVI   3(R3),C'Y'                                                       
         TM    0(R5),X'08'                                                      
         BZ    *+8                                                              
         MVI   4(R3),C'Y'                                                       
         TM    0(R5),X'04'                                                      
         BZ    *+8                                                              
         MVI   5(R3),C'Y'                                                       
         TM    0(R5),X'02'                                                      
         BZ    *+8                                                              
         MVI   6(R3),C'Y'                                                       
         TM    0(R5),X'01'                                                      
         BZ    *+8                                                              
         MVI   7(R3),C'Y'                                                       
DOPROFX  XIT1                                                                   
         EJECT                                                                  
**********************************************************************          
*   SUBROUTINE TO FORMAT FORMER REP/NEW REP FIELDS                              
**********************************************************************          
FNFMT    NTR1                                                                   
         L     R6,AIO                                                           
         MVI   ELCODE,X'0C'                                                     
         BAS   RE,GETEL                                                         
         BNE   FNFMEXIT            NO, NOTHING TO PUT TO SCREEN                 
*                                                                               
         USING RSTAFNEL,R6                                                      
         MVC   FMSFOR,RSTAFNFO     GET THE FORMER REP                           
         FOUT  FMSFORH             PUT TO SCREEN                                
         MVC   FMSNEW,RSTAFNNE     GET THE NEW REP                              
         FOUT  FMSNEWH                                                          
         DROP  R6                                                               
*                                                                               
FNFMEXIT XIT1                                                                   
         EJECT                                                                  
*********************************************************************           
*  DISPCOMS:  DISPLAYS CONTRACT AND STATION COMMENTS.                           
*********************************************************************           
DISPCOMS NTR1                                                                   
         LA    R2,FMSCCMH                                                       
         OI    6(R2),X'80'                                                      
         BAS   RE,CMTFMT           DISPLAY CONTRACT COMMENT IF ANY              
*                                                                               
         LA    R2,FMSSC1H          A(STATION COMMENT FIELD)                     
         OI    6(R2),X'80'                                                      
         LA    R2,FMSSC2H                                                       
         OI    6(R2),X'80'                                                      
         LA    R2,FMSSC1H                                                       
         L     R6,AIO                                                           
         MVI   ELCODE,X'0B'                                                     
         BAS   RE,GETEL                                                         
         BNE   DCOM0098                                                         
         ZIC   RF,1(R6)            GET ELEMENT LENGTH                           
         LA    RE,3                PREPARE FOR MOVE BY LENGTH                   
         SR    RF,RE                                                            
         EX    RF,DCOM0095                                                      
         ZIC   RF,1(R6)            BUMP TO NEXT ELEMENT                         
         AR    R6,RF                                                            
         CLI   0(R6),X'0B'         ANOTHER STATION COMMENT ELT?                 
         BNE   DCOM0098            NO                                           
         LA    R2,FMSSC2H          YES - NEXT COMMENT FIELD ON SCREEN           
         ZIC   RF,1(R6)            GET ELEMENT LENGTH                           
         LA    RE,3                PREPARE FOR MOVE BY LENGTH                   
         SR    RF,RE                                                            
         EX    RF,DCOM0095                                                      
         B     DCOM0098                                                         
*                                                                               
DCOM0095 EQU   *                                                                
         MVC   8(0,R2),2(R6)       MOVE BY LENGTH                               
*                                                                               
DCOM0098 EQU   *                                                                
         XIT1                                                                   
         EJECT                                                                  
********************************************************************            
* SUB-ROUTINE FOR FORMAT OF CONTRACT COMMENT                                    
********************************************************************            
CMTFMT   NTR1                                                                   
         L     R6,AIO                                                           
         MVI   ELCODE,X'03'                                                     
         BAS   RE,GETEL                                                         
         BNE   CF200                                                            
         USING RSTACEL,R6                                                       
         CLI   RSTACTYP,C'M'                                                    
         BE    CF100                                                            
         MVC   8(1,R2),RSTACTYP                                                 
         MVI   9(R2),C'='                                                       
         MVC   HALF,RSTACNUM                                                    
         LH    R6,HALF                                                          
         EDIT  (R6),(4,10(R2)),ALIGN=LEFT                                       
         B     CF200                                                            
         SPACE                                                                  
CF100    ZIC   R1,RSTACLEN                                                      
         SH    R1,=H'5'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   8(0,R2),RSTACCMT                                                 
*                                                                               
         CLC   =C'C=',8(R2)        DISPLAY STORED FILE COMMENT                  
         BNE   CF200                                                            
         DROP  R6                                                               
         GOTO1 =A(DISPFC),DMCB,(RC),(RA),10(R2),(46,22(R2)),RR=RELO             
*                                                                               
CF200    XIT1                                                                   
         EJECT                                                                  
*                                                                               
**********************************************************************          
*                  CLEAR FIELDS ROUTINE                              *          
**********************************************************************          
CLEAR    NTR1                                                                   
         LA    R2,FMSMKH           FIRST FIELD                                  
         LA    R5,FMSLST           LAST FIELD                                   
*                                                                               
CR20     EQU   *                                                                
         CR    R2,R5               ALL FIELDS HAVE BEEN CLEARED?                
         BNL   CREX                YES                                          
         TM    1(R2),X'20'         PROTECTED FIELD?                             
         BO    CR50                YES - NEXT FIELD                             
         ZIC   R3,5(R2)            LENGTH                                       
         LTR   R3,R3                                                            
         BZ    CR50                NEXT FIELD                                   
         BCTR  R3,0                                                             
         EX    R3,*+8                                                           
         B     *+10                                                             
         XC    8(0,R2),8(R2)                                                    
         OI    6(R2),X'80'                                                      
CR50     EQU   *                                                                
         ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         B     CR20                                                             
CREX     EQU   *                                                                
         XIT1                                                                   
         DROP  R4                                                               
         EJECT                                                                  
**********************************************************************          
         GETEL R6,DATADISP,ELCODE                                               
**********************************************************************          
TEMPIDS  DS    CL40                DUMMY FIELD                                  
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
**********************************************************************          
*                  VALIDATE RECORD                                   *          
**********************************************************************          
VR       NMOD1 0,*VALREC*                                                       
         L     RC,0(R1)                                                         
         L     RA,4(R1)            TWA                                          
         L     R4,AIO                                                           
*                                                                               
         MVC   MYKEY(L'MYKEY),0(R4)   SAVE THE KEY(AVOID PUTREC DRAMA)          
*                                                                               
         OI    GENSTAT2,RETEQSEL                                                
         USING STATD,R4                                                         
*                                                                               
         CLI   ACTNUM,ACTADD       IF ACTION ADD                                
         BE    VR0040                                                           
*                                                                               
         MVI   ELCODE,X'03'                                                     
         GOTO1 REMELEM                  COMMENT ELEMENTS                        
         MVI   ELCODE,X'05'                                                     
         GOTO1 REMELEM                  EXTENDED DESCRIP. ELEMENT               
         MVI   ELCODE,X'06'                                                     
         GOTO1 REMELEM                  SIGN ON ID'S ELEMENT                    
         MVI   ELCODE,X'07'                                                     
         GOTO1 REMELEM                  TWX ELEMENT                             
         MVI   ELCODE,X'08'                                                     
         GOTO1 REMELEM                  EXTRA DESCRIP. ELEMENT                  
         MVI   ELCODE,X'09'                                                     
         GOTO1 REMELEM                  COMBINED STATION ELEMENT                
*                                                                               
         BAS   RE,SAV0AELS                                                      
*                                  SAVE ANY COMBO ELEMENTS                      
         MVI   ELCODE,X'0A'                                                     
         GOTO1 REMELEM                  NEW COMBINED STATION ELEMENT            
         MVI   ELCODE,X'0B'                                                     
         GOTO1 REMELEM                  STATION COMMENT ELEMENT                 
         MVI   ELCODE,X'0C'                                                     
         GOTO1 REMELEM                  FORMER REP/NEW REP ELEMENT              
*                                                                               
         LA    R6,RSTAELEM         POINT R6 AT FIRST ELEMENT                    
         ZIC   R1,1(R6)                                                         
         SH    R1,=H'3'                                                         
         EX    R1,*+8              ZERO OUT '01' ELEMENT DATA                   
         B     VR0080                                                           
         XC    2(0,R6),2(R6)                                                    
         SPACE 1                                                                
VR0040   EQU   *                                                                
         MVC   RSTAELEM(2),=X'0153'                                             
         MVC   RSTALEN,=Y(117)                                                  
         SPACE 1                                                                
VR0080   EQU   *                                                                
         MVC   RSTAPROF,ZEROS                                                   
         CLI   ACTNUM,ACTADD                                                    
         BE    *+10                                                             
         MVC   RSTACLDT,SVCLDT     RESTORE SAVED CLOSE DATE                     
         EJECT                                                                  
*                                                                               
* MARKET                                                                        
         LA    R2,FMSMKH                                                        
         CLI   5(R2),0                                                          
         BE    VRERR1                                                           
         MVC   RSTAMKT,8(R2)                                                    
         EJECT                                                                  
*                                                                               
* CONTRACT TO STATION                                                           
         LA    R2,FMSCSTH                                                       
         MVI   RSTASTAT,0                                                       
         CLI   5(R2),0                                                          
         BE    VR0160                                                           
         ZIC   R1,5(R2)                                                         
         BCTR  R1,0                                                             
         EX    R1,CLCNO                                                         
         BNE   VR0120                                                           
         OI    RSTASTAT,X'08'                                                   
         B     VR0160                                                           
VR0120   EQU   *                                                                
         EX    R1,CLCYES                                                        
         BNE   VRERR2                                                           
VR0160   EQU   *                                                                
         EJECT                                                                  
*                                                                               
* RECAP                                                                         
*                                                                               
         LA    R2,FMSCAPH                                                       
         CLI   5(R2),0                                                          
         BE    VR0280                                                           
         ZIC   R1,5(R2)                                                         
         BCTR  R1,0                                                             
         EX    R1,CLCNO                                                         
         BNE   VR0240                                                           
         OI    RSTASTAT,X'04'                                                   
         B     VR0280                                                           
VR0240   EX    R1,CLCYES                                                        
         BNE   VRERR2                                                           
VR0280   DS    0H                                                               
         EJECT                                                                  
*                                                                               
* CHANNEL                                                                       
         LA    R2,FMSCHH                                                        
         CLI   5(R2),0                                                          
         BE    VRERR1                                                           
         TM    4(R2),X'08'         VALID NUM                                    
         BZ    VRERR3                                                           
         CLI   RSTAKSTA+4,C' '     CHECK TV                                     
         BE    VR0300              YES                                          
         CLI   RSTAKSTA+4,C'L'     CHECK TV                                     
         BNE   VR0320              NO                                           
VR0300   EQU     *                                                              
         CLI   5(R2),2                                                          
         BH    VRERR2                                                           
         B     VR0360                                                           
VR0320   CLI   5(R2),3                                                          
         BL    VRERR2                                                           
VR0360   BAS   RE,PACK                                                          
         LTR   R0,R0                                                            
         BZ    VRERR3                                                           
         STH   R0,RSTACHAN                                                      
         EJECT                                                                  
*                                                                               
* GROUP/SUBGROUP                                                                
         LA    R2,FMSGSGH                                                       
         CLI   5(R2),0                                                          
         BE    VRERR1                                                           
         BAS   RE,MOVE             MOVE DATA FROM SCREEN TO WORK FIELD          
*                                                                               
*   FOLLOWING TEST PERMITS A CHANGE TO GROUP/SUBGROUP IF THE FINAL              
*        PROFILE BIT FOR 'FILE' IS ON.  THIS IS A TEMPORARY TEST                
*        TO FACILITATE CONVERSION OF KATZ STATION RECORDS, AND                  
*        SHOULD BE REMOVED WHEN THAT CHORE IS COMPLETED.                        
* !!!!!!!!!!!!!!!!!!!!!!!!!!!                                                   
         TM    SVPGPBIT+7,X'01'    TEST VERY LAST BIT OF PROFILE                
         BO    VR0380              ON  - SKIP GROUP/SUBGRP TEST                 
         CLI   ACTNUM,ACTADD                                                    
         BE    *+14                                                             
         CLC   SVGRUP,WORK         TEST SAME GROUP                              
         BNE   VRERR4              NO - ERROR                                   
VR0380   EQU   *                                                                
         MVC   RSTAGRUP,WORK                                                    
         MVC   SVGRUP,RSTAGRUP                                                  
         SPACE 1                                                                
         XC    KEY,KEY             VALIDATE GROUP ON FILE                       
         MVI   KEY,7                                                            
         MVC   KEY+23(2),AGENCY                                                 
         MVC   KEY+25(2),RSTAGRUP                                               
         GOTO1 HIGH                                                             
         CLC   KEYSAVE(27),KEY                                                  
         BNE   VRERR5                                                           
VR0400   DS    0H                                                               
         EJECT                                                                  
*                                                                               
* TRAFFIC FORMAT                                                                
         LA    R2,FMSTRFH                                                       
         ST    R2,FULL             SAVE A(R2) FOR RECEIVING ID EDIT             
         CLI   5(R2),0                                                          
         BE    VR0520                                                           
         GOTO1 =A(EDITTRAF),DMCB,RR=RELO                                        
         BNZ   VRERR2                                                           
* CROSS CHECK THAT ONLY FORMAT 'A' USED FOR RADIO (BAND A/F/C)                  
         CLI   RSTAKSTA+4,C' '    DEFAULT TELEVISION                            
         BE    VR0440                                                           
         CLI   RSTAKSTA+4,C'L'    LOW POWER TELEVISION                          
         BE    VR0440                                                           
         CLI   RSTAKSTA+4,C'T'    EXPLICIT TELEVISION                           
         BE    VR0440                                                           
         CLI   8(R2),C'A'         ALLOW ONLY 'A' OR ' ' FOR RADIO               
         BE    VR0480                                                           
         CLI   8(R2),C' '                                                       
         BE    VR0480                                                           
         BNE   VRERR2                                                           
*                                                                               
VR0440   CLI   8(R2),C'A'          DON'T ALLOW 'A' FOR TV                       
         BE    VRERR2                                                           
*                                                                               
VR0480   MVC   RSTATRAF,8(R2)                                                   
VR0520   DS    0H                                                               
         EJECT                                                                  
*                                                                               
* TVB REGION                                                                    
         LA    R2,FMSTVBH                                                       
         CLI   5(R2),0                                                          
         BE    VR0600                                                           
         L     R5,ATVBLST                                                       
VR0540   EQU   *                                                                
         CLC   0(2,R5),8(R2)                                                    
         BE    VR0560                                                           
         LA    R5,20(R5)                                                        
         CLI   0(R5),X'FF'                                                      
         BNE   VR0540                                                           
*                                                                               
         B     VRERR2                                                           
VR0560   EQU   *                                                                
         MVC   RSTATVB,8(R2)                                                    
VR0600   DS    0H                                                               
         EJECT                                                                  
*                                                                               
* JOIN DATE                                                                     
         LA    R2,FMSJDH                                                        
         XC    WORK,WORK                                                        
         GOTO1 DATVAL,DMCB,(0,8(R2)),WORK                                       
         OC    0(4,R1),0(R1)                                                    
         BZ    VRERR2                                                           
*                                                                               
* LEAVE DATE                                                                    
         LA    R2,FMSLDH                                                        
         CLI   5(R2),0                                                          
         BE    VR0640                                                           
         GOTO1 (RF),(R1),(0,8(R2)),WORK+6                                       
         OC    0(4,R1),0(R1)                                                    
         BZ    VRERR2                                                           
VR0640   GOTO1 DATCON,DMCB,WORK,(3,RSTASTRT)                                    
         OC    WORK+6(6),WORK+6                                                 
         BZ    VR0680                                                           
         GOTO1 (RF),(R1),WORK+6,(3,RSTAEND)                                     
VR0680   DS    0H                                                               
         EJECT                                                                  
*                                                                               
* OWNER                                                                         
         LA    R2,FMSOWNH                                                       
         CLI   5(R2),0             ANYTHING IN OWNER FIELD?                     
         BNE   VR0690              YES                                          
         TM    SVPGPBIT+1,X'10'    NO  - OWNER REQUIRED?                        
         BO    VRERR6              YES - ERROR                                  
         B     VR0720              NO  - OWNER NOT REQUIRED                     
VR0690   EQU   *                                                                
         CLI   5(R2),3             MUST INPUT 3 CHARS                           
         BNE   VRERR2                                                           
*                                  READ OWNERSHIP RECORD FOR NAME               
         XC    KEY,KEY                                                          
         MVI   KEY,X'2A'                                                        
         MVC   KEY+22(2),AGENCY                                                 
         MVC   KEY+24(3),8(R2)                                                  
         GOTO1 HIGH                                                             
         CLC   KEYSAVE(27),KEY                                                  
         BNE   VRERR7                                                           
*                                                                               
         MVC   RSTAOWN,8(R2)                                                    
VR0720   DS    0H                                                               
         EJECT                                                                  
*                                                                               
* RANK                                                                          
         LA    R2,FMSRNKH                                                       
         CLI   5(R2),0                                                          
         BNE   VR0760                                                           
         LA    R1,SVPGPBIT         PROGRAM PROFILE BITS                         
         TM    0(R1),X'08'         4TH BIT ON = RANK REQUIRED                   
         BZ    VR0800              NOT REQUIRED                                 
         LR    R1,RA                CHECK FOR DDS TERMINAL                      
         USING TWAD,R1                                                          
         CLI   TWAOFFC,C'*'        DDS TERMINAL?                                
         BE    VR0800              YES - IGNORE TEST                            
         DROP  R1                                                               
         B     VRERR1              REQUIRED = ERROR                             
VR0760   TM    4(R2),X'08'         VALID NUMERIC                                
         BZ    VRERR3                                                           
         CLI   8(R2),C'7'          MUST BE FROM 1-7                             
         BH    VRERR2                                                           
         MVC   RSTARANK,8(R2)                                                   
VR0800   DS    0H                                                               
         EJECT                                                                  
*                                                                               
* STATUS                                                                        
         LA    R2,FMSSTH                                                        
         CLI   5(R2),0                                                          
         BE    VR0840                                                           
         BAS   RE,SCANSTAT         EDIT STATUS FIELD                            
         BNZ   VRERR2                                                           
VR0840   DS    0H                                                               
         EJECT                                                                  
*                                                                               
* FORMER REP/NEW REP                                                            
         CLI   FMSNEWH+5,0         ANYTHING ENTERED IN NEW REP FIELD?           
         BE    VR0870              NO                                           
         CLI   FMSLDH+5,0          YES, IS LEAVE DATE ENTERED?                  
         BNE   VR0875              YES, EDIT THE FIELDS                         
         B     VRERR8              NO REP WITHOUT LEAVE DATE                    
*                                                                               
VR0870   CLI   FMSFORH+5,0         ANYTHING ENTERED IN FORMER REP FLD?          
         BE    VR0880              NO                                           
*                                                                               
VR0875   GOTO1 =A(FNEDT),DMCB,(RC),(RA),RR=RELO EDT FRM/NEW REP FLDS            
         BNZ   VRERR9              ERRORS                                       
VR0880   EQU   *                                                                
         EJECT                                                                  
*                                                                               
* RECEIVING ID                                                                  
         LA    R2,FMSRIDH                                                       
         SPACE 1                                                                
         CLI   RSTATRAF,C'A'       RADIO MUST HAVE 'GRAPH'                      
         BE    *+12                RECEIVING ID                                 
         CLI   RSTATRAF,C'G'       GRAPHNET FORMAT MUST HAVE                    
         BNE   VR0920              'GRAPH' RECEIVING ID                         
         CLC   8(5,R2),=C'GRAPH'                                                
         BNE   VRERR2                                                           
         CLI   5(R2),5                                                          
         BNE   VRERR2                                                           
         B     VR0960                                                           
*                                                                               
VR0920   CLI   5(R2),0                                                          
         BE    VR1000                                                           
         CLC   8(5,R2),=C'GRAPH'   NON-GRAPHNET FORMAT CAN'T                    
         BE    VRERR2              HAVE 'GRAPH' RECEIVING ID                    
*                                                                               
VR0960   BAS   RE,MOVE             MOVE DATA FROM SCREEN TO WORK FIELD          
         BAS   RE,RIDEDT           EDIT RECEIVING ID FIELD                      
         BNZ   VRERR2                                                           
*                                                                               
         OC    RSTATRAF,RSTATRAF   IF THERE'S RECEIVING ID,                     
         BNZ   VR1000              THERE MUST BE A TRAFFIC TYPE                 
         LA    R2,FMSTRFH          POINT TO TRAFFIC FIELD                       
         B     VRERR1              MISSING INPUT                                
VR1000   DS    0H                                                               
         EJECT                                                                  
*                                                                               
* SIGN ON ID'S                                                                  
         LA    R2,FMSSIDH                                                       
         CLI   5(R2),0                                                          
         BE    VR1040                                                           
*                                                                               
         BAS   RE,SOEDT            EDIT SIGN ON ID FIELD                        
         BNZ   VRERR2                                                           
VR1040   DS    0H                                                               
         EJECT                                                                  
*                                                                               
* PRIMARY AFFILIATE                                                             
         LA    R2,FMSAFH                                                        
         L     R1,ADDRLIST                                                      
VR1080   DS    0H                                                               
         CLI   0(R1),0                                                          
         BE    VR1120                                                           
         CLC   0(3,R1),8(R2)                                                    
         BE    VR1160                                                           
         LA    R1,3(R1)                                                         
         B     VR1080                                                           
VR1120   DS    0H                                                               
         CLI   RSTAKSTA+4,C' '     TV MUST HAVE AFFIL                           
         BE    VRERR2                                                           
         CLI   5(R2),0             RADIO MAY BE BLANK                           
         BNE   VRERR2                                                           
         OC    8(3,R2),=CL3' '                                                  
VR1160   MVC   RSTAAFFL,8(R2)                                                   
         EJECT                                                                  
*                                                                               
* TWX NUMBER                                                                    
         LA    R2,FMSTWXH                                                       
         CLI   5(R2),0                                                          
         BE    VR1200                                                           
         XC    ELEM,ELEM                                                        
         MVC   ELEM(2),=X'0740'                                                 
         MVC   ELEM+2(20),8(R2)                                                 
         GOTO1 ADDELEM                                                          
VR1200   DS    0H                                                               
         EJECT                                                                  
*                                                                               
* LIABILITY COMMENT NUMBER                                                      
         XC    RSTALIAB,RSTALIAB                                                
         LA    R2,FMSLIBH                                                       
         CLI   5(R2),0                                                          
         BNE   VR1240                                                           
         MVC   FMSLIC,SPACES                                                    
         FOUT  FMSLICH                                                          
         B     VR1280                                                           
*                                                                               
VR1240   CLI   5(R2),2                                                          
         BNE   VRERR10             LIAB. COMMENT MUST BE 1 THRU 99              
         LA    RE,8(R2)                                                         
         ZIC   RF,5(R2)                                                         
         STM   RE,RF,DMCB                                                       
         GOTO1 =V(NUMVAL),DMCB,RR=RELO                                          
         CLI   DMCB,0                                                           
         BNE   VRERR10                                                          
         CLC   DMCB+4(4),=F'0'                                                  
         BNH   VRERR10                                                          
         CLC   DMCB+4(4),=F'100'                                                
         BNL   VRERR10                                                          
         MVC   RSTALIAB,DMCB+4+3                                                
*                                                                               
         XC    KEY,KEY                                                          
         LA    R3,KEY                                                           
         USING RCMTD,R3                                                         
         MVI   RCMTKTYP,X'2E'      RECORD TYPE                                  
         MVC   RCMTKREP,AGENCY     REP CODE                                     
         MVC   RCMTKOFF,=X'FFFF'   OFFICE CODE                                  
         MVC   RCMTKCDE(4),=C'LIAB'  LIABILITY COMMENT CODE                     
         MVC   RCMTKCDE+4(2),8(R2)                                              
         OC    RCMTKCDE,SPACES     BLANK PADDED                                 
         DROP  R3                                                               
         GOTO1 HIGH                                                             
         CLC   KEY(L'RCMTKEY),KEYSAVE                                           
         BE    VR1280                                                           
         XC    RSTALIAB,RSTALIAB                                                
         B     VRERR11             LIABILITY REC NOT FOUND                      
VR1280   DS    0H                                                               
         EJECT                                                                  
*                                                                               
* CONTRACT COMMENT                                                              
         LA    R2,FMSCCMH                                                       
         CLI   5(R2),0                                                          
         BE    VR1320                                                           
         BAS   RE,CMTEDT           EDIT CONTRACT COMMENT                        
         BNZ   VRBAD                                                            
VR1320   DS    0H                                                               
         EJECT                                                                  
*                                                                               
* STATION COMMENTS:                                                             
         LA    R0,2                                                             
         LA    R2,FMSSC1H          A(1ST STATION COMMENT HEADER)                
VR1440   EQU   *                                                                
         CLI   5(R2),0             ANY VALUE IN FIELD?                          
         BE    VR1560              NO  - LOOP EACH FIELD                        
         XC    ELEM,ELEM           YES - SET UP ELEMENT                         
         MVI   ELEM,X'0B'          INSERT ELEMENT ID                            
         ZIC   RF,5(R2)            INSERT LENGTH OF ELEMENT                     
         LA    RF,2(RF)            ADD TWO FOR ELEMENT INFORMATION              
         STC   RF,ELEM+1           INSERT INTO ELEMENT                          
         LA    RE,3                SET UP TO MOVE COMMENT BY LENGTH             
         SR    RF,RE                                                            
         EX    RF,VR1480           MOVE BY LENGTH                               
         B     VR1520                                                           
*                                                                               
VR1480   MVC   ELEM+2(0),8(R2)     MOVE COMMENT TO ELEMENT                      
*                                                                               
VR1520   EQU   *                                                                
         GOTO1 ADDELEM                                                          
*                                  ADD COMMENT ELEMENT TO RECORD                
VR1560 EQU     *                                                                
         ZIC   RF,0(R2)            BUMP TO NEXT FIELD                           
         AR    R2,RF                                                            
         BCT   R0,VR1440           GO BACK FOR NEXT FIELD                       
         EJECT                                                                  
*                                                                               
* RADIO COMBOS                                                                  
         LA    R2,FMSCMH           COMBO FIELD                                  
         CLI   5(R2),0                                                          
         BE    VR1640                                                           
         CLI   RSTAKSTA+4,C'C'     COMBINED STA CAN'T HAVE COMBO FLD            
         BE    VRERR2                                                           
         CLI   RSTAKSTA+4,C' '     TV CAN'T HAVE COMBO FLD                      
         BE    VRERR2                                                           
         CLI   RSTAKSTA+4,C'T'     TV CAN'T HAVE COMBO FLD                      
         BE    VRERR2                                                           
         CLI   RSTAKSTA+4,C'L'     TV CAN'T HAVE COMBO FLD                      
         BE    VRERR2                                                           
         CLI   5(R2),5             INPUT LEN MUST BE 5 OR 6 CHARS               
         BE    *+16                                                             
         CLI   5(R2),6                                                          
         BE    *+20                                                             
         B     VRERR2                                                           
         CLI   8+4(R2),C'C'        IF INPUT LEN=5, MUST BE                      
         BNE   VRERR2                                                           
         B     *+14                                                             
         CLC   =C'-C',8+4(R2)      IF INPUT LEN=6, MUST BE                      
         BNE   VRERR2                                                           
* BUILD STATION RECORD (MAKE SURE COMBO EXISTS)                                 
*                                                                               
         XC    KEY,KEY             VERIFY STATION IS VALID                      
         MVI   KEY,2                                                            
         MVC   KEY+20(2),AGENCY                                                 
         MVC   KEY+22(4),8(R2)                                                  
         OC    KEY+22(4),SPACES                                                 
         MVI   KEY+26,C'C'                                                      
         GOTO1 HIGH                                                             
         CLC   KEYSAVE(27),KEY                                                  
         BNE   VRERR12                                                          
* BUILD X'0A' COMBINED STATION ELEM                                             
         XC    ELEM,ELEM                                                        
         MVC   ELEM(2),=X'0A0A'    NEW LENGTH: 10 CHARS                         
         MVC   ELEM+2(5),KEY+22                                                 
         GOTO1 ADDELEM                                                          
* EDIT CM1-4                                                                    
VR1640 DS      0H                                                               
         MVI   PREFSTAT,C'N'       SET PREFERRED STATION FLAG                   
         LA    R2,FMSCM1H          COMBO 1                                      
*                                                                               
* PROGRAMMERS NOTE:                                                             
*     VALUE OF NEXT INSTRUCTION DETERMINES NUMBER OF 'CM' FIELDS                
*      (COMBO STATIONS) THAT WILL BE USED.  THIS NUMBER SHOULD                  
*      CORRESPOND TO THE NUMBER OF UNPROTECTED FIELDS WITHIN                    
*      THE SCREEN.                                                              
*                                                                               
         LA    R0,4                4 COMBO FIELDS                               
VR1680   CLI   5(R2),0                                                          
         BE    VR2000                                                           
         CLI   RSTAKSTA+4,C'C'     CHECK FOR COMBINED STATION                   
         BNE   VRERR2              ONLY COMBINED STA CAN HAVE 1-4               
*                                                                               
         XC    ELEM,ELEM           INITIALIZE COMBINED STA ELT                  
         XC    KEY,KEY             VERIFY STATION IS VALID                      
         MVI   KEY,2                                                            
         MVC   KEY+20(2),AGENCY                                                 
         MVC   KEY+22(4),8(R2)                                                  
         OC    KEY+22(4),SPACES                                                 
*                                                                               
         CLI   5(R2),5             INPUT LENGTH MUST BE 5 OR 6                  
         BE    VR1880                                                           
         BL    VRERR2              LESS THAN 5:  ERROR                          
         CLI   8+5(R2),C'*'        6TH AN ASTERISK?                             
         BNE   VR1720              NO                                           
         CLI   5(R2),6             YES - ONLY ALLOW 6 CHARS                     
         BNE   VRERR2              ERROR                                        
         MVI   ELEM+7,C'*'         SET PREFERRED STATION                        
         CLI   PREFSTAT,C'N'       FIRST PREFERRED STAT?                        
         BNE   VRERR2              NO  - CAN'T HAVE TWO                         
*                                                                               
         MVI   PREFSTAT,C'Y'       YES - SET TO 'ENTERED'                       
         B     VR1880              PROCESS AS IF 5 CHARACTERS                   
VR1720   EQU   *                                                                
         CLI   8+5(R2),C'-'        6TH A MINUS?                                 
         BNE   VR1760              NO                                           
         CLI   5(R2),6             YES - ONLY ALLOW 6 CHARS                     
         BNE   VRERR2              ERROR                                        
         MVI   ELEM+7,C'-'         SET MINUSED STATION                          
         B     VR1880              PROCESS AS IF 5 CHARACTERS                   
VR1760   EQU   *                                                                
         CLI   8+6(R2),C'*'        7TH AN ASTERISK?                             
         BNE   VR1800              NO                                           
         CLI   5(R2),7             YES - ONLY ALLOW 7 CHARS                     
         BNE   VRERR2              ERROR                                        
         MVI   ELEM+7,C'*'         SET PREFERRED STATION                        
         CLI   PREFSTAT,C'N'       FIRST PREFERRED STAT?                        
         BNE   VRERR2              NO  - CAN'T HAVE TWO                         
         MVI   PREFSTAT,C'Y'       YES - SET TO 'ENTERED'                       
         B     VR1920              PROCESS AS IF 6 CHARACTERS                   
VR1800   EQU   *                                                                
         CLI   8+6(R2),C'-'        7TH A MINUS?                                 
         BNE   VR1840              NO                                           
         CLI   5(R2),7             YES - ONLY ALLOW 7 CHARS                     
         BNE   VRERR2              ERROR                                        
         MVI   ELEM+7,C'-'         SET MINUSED STATION                          
         B     VR1920              PROCESS AS IF 6 CHARACTERS                   
VR1840   EQU   *                                                                
         CLI   5(R2),6                                                          
         BE    VR1920                                                           
         B     VRERR2                                                           
*                                                                               
VR1880   CLI   8+4(R2),C'A'                                                     
         BNE   *+12                                                             
         MVI   KEY+26,C'A'                                                      
         B     VR1960                                                           
         CLI   8+4(R2),C'F'                                                     
         BNE   VRERR2                                                           
         MVI   KEY+26,C'F'                                                      
         B     VR1960                                                           
VR1920 CLC     =C'-A',8+4(R2)                                                   
         BNE   *+12                                                             
         MVI   KEY+26,C'A'                                                      
         B     VR1960                                                           
         CLC   =C'-F',8+4(R2)                                                   
         BNE   VRERR2                                                           
         MVI   KEY+26,C'F'                                                      
VR1960   GOTO1 HIGH                                                             
         CLC   KEYSAVE(27),KEY                                                  
         BNE   VRERR12                                                          
*                                                                               
         MVC   ELEM(2),=X'0A0A'    NEW LENGTH                                   
         MVC   ELEM+2(5),KEY+22                                                 
*                                                                               
         GOTO1 DATCON,DMCB,(5,WORK),(2,ELEM+8)                                  
*                                  INSERT TODAY'S DATE EFFECTIVE                
         GOTO1 ADDELEM                                                          
VR2000   DS    0H                                                               
         GOTO1 =A(NEXTUF),DMCB,(R2),RR=RELO                                     
         BCT   R0,VR1680                                                        
         OC    COMBAREA,COMBAREA   ANY OLD X'0A' ELTS?                          
         BZ    VR2080              NO  - ACCEPT NEW ONES                        
         GOTO1 =A(CHK0AELS),DMCB,(RC),(RA),RR=RELO                              
*                                  YES - INSERT OLD DATES                       
****>>>  BNZ   FLERR6                                                           
*                                                                               
*   ABOVE BRANCH TO BE REMOVED WHEN STATIONS CAN BE DELETED.                    
*                                                                               
VR2080   EQU   *                                                                
         EJECT                                                                  
*                                                                               
* EDIT ALL FIELDS ON EXTENDED DESC ELEM                                         
* (DEST FMT, OPTIONS, MKT CODE, DEST ID, INV, A/R INTERFACE CODE,               
*  FAX #, SECONDARY AFFL, TIME ZONE, ELEC CON, LUID, INVOICE)                   
         GOTO1 =A(OPTEDT),DMCB,(RC),(RA),RR=RELO                                
         BNZ   VRBAD                                                            
*                                                                               
VR2120   DS    0H                                                               
*                                                                               
*    THIS BYPASS ELIMINATES THE CHECK TO ENSURE ALL STATIONS THAT               
*      BELONG TO THE COMBO ARE OF THE SAME FORMAT/RECEIVING ID.                 
*      THIS IS NECESSARY BECAUSE ALL STATIONS CANNOT BE UPDATED                 
*      AT ONE TIME, AND THIS IS A 'YOU CANNOT GET THERE FROM HERE'              
*      SITUATION.  ANOTHER APPROACH MUST BE DEVISED.                            
*                                                                               
*****    BAS   RE,CHECKFMT         CHECK COMBO STATION FORMATS                  
*****    BZ    FLFILE              CC = ZERO = OKAY                             
*****    B     FLERR7              CC NOT = ZERO = ERROR                        
*                                                                               
VRGOOD   EQU   *                                                                
         GOTO1 =A(DR),DMCB,(RC),(RA),RR=RELO                                    
         MVC   KEY(L'MYKEY),MYKEY    REASTABLISH THE SEQUENCE                   
         GOTO1 HIGH                                                             
         MVC   AIO,AIO2                                                         
         GOTO1 GETREC                                                           
         MVC   AIO,AIO1                                                         
         SR    R0,R0                                                            
         B     VREX                                                             
*                                                                               
VRBAD    LA    R0,1                                                             
*                                                                               
VREX     EQU   *                                                                
         LTR   R0,R0               SET THE CONDITION CODE                       
         XIT1  REGS=(R2)           IN CASE OF ERROR MESSAGE                     
         EJECT                                                                  
********************************************************************            
*  SAV0AELS:  SAVES COMBO STATION ELEMENTS FOR COMPARISON TO ENSURE             
*    ORDER IS NOT CHANGED.                                                      
********************************************************************            
SAV0AELS NTR1                                                                   
         XC    COMBAREA,COMBAREA                                                
         LA    R2,COMBAREA         SET A(COMBO ELT AREA)                        
         LA    R1,RSTAELEM         STATION REC DESCRIPTIVE ELEMENT              
SAVA0010 EQU   *                                                                
         CLI   0(R1),X'00'         END OF RECORD?                               
         BE    SAVA0040            YES - FINISHED                               
         ZIC   R3,1(R1)            YES - TAKE LENGTH                            
         CLI   0(R1),X'0A'         COMBO ELEMENT?                               
         BNE   SAVA0020            NO  - SKIP AND LOOK AT NEXT                  
         LR    RF,R3                                                            
         BCTR  RF,0                                                             
         EX    RF,SAVA0030         MOVE FIELD BY LENGTH                         
         AR    R2,R3               BUMP COMPAREA BY LENGTH                      
SAVA0020 EQU   *                                                                
         AR    R1,R3               ADD L(ELT) AND A(ELT)                        
         B     SAVA0010            GO BACK FOR NEXT ELEMENT                     
SAVA0030 MVC   0(0,R2),0(R1)       MOVE X'0A' ELT BY LENGTH                     
SAVA0040 EQU   *                                                                
         XIT1                                                                   
         EJECT                                                                  
******************************************************************              
* SUB-ROUTINE TO EDIT STATUS FIELD ON STATION SCREEN                            
******************************************************************              
SCANSTAT NTR1                                                                   
         XC    WORK,WORK                                                        
         GOTO1 SCANNER,DMCB,(R2),(4,WORK)                                       
         CLI   DMCB+4,0                                                         
         BE    SSBAD                                                            
         ZIC   R5,DMCB+4           NUMBER OF LINES                              
         LA    R6,WORK                                                          
         SPACE                                                                  
SS10     EQU   *                                                                
         CLI   0(R6),3             KEYWORD MUST BE AT LEAST 3 BYTES             
         BL    SSBAD                                                            
         SPACE                                                                  
         CLI   0(R6),3             VALDATE BOP CHECK OVERRIDE                   
         BH    SS20                                                             
         CLC   12(3,R6),=C'BOP'                                                 
         BNE   SS20                                                             
         ZIC   R1,1(R6)                                                         
         LTR   R1,R1                                                            
         BZ    SSBAD                                                            
         CH    R1,=H'2'                                                         
         BH    SSBAD                                                            
         BCTR  R1,0                                                             
         EX    R1,NOCOMP                                                        
         BNE   SSBAD                                                            
         OI    RSTASTAT,X'80'                                                   
         B     SSX                                                              
         SPACE                                                                  
SS20     EQU   *                   VALIDATE CONTRACT LOCKOUT                    
         CLI   0(R6),8                                                          
         BH    SSBAD                                                            
         ZIC   R1,0(R6)                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   12(0,R6),=C'CONTRACT'                                            
         BNE   SS30                                                             
         ZIC   R1,1(R6)                                                         
         LTR   R1,R1                                                            
         BZ    SSBAD                                                            
         CH    R1,=H'2'                                                         
         BH    SSBAD                                                            
         BCTR  R1,0                                                             
         EX    R1,NOCOMP                                                        
         BNE   SSBAD                                                            
         OI    RSTASTAT,X'40'                                                   
         B     SSX                                                              
         SPACE                                                                  
SS30     DS    0H                  VALIDATE ACCESS TO NEW AVAILS                
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   12(0,R6),=CL8'AVAILS'                                            
         B     SS40                AVAILS=NEW IS SKIPPED!!                      
***>>>   BNE   SS40                DISCONTINUED!!                               
         CLI   1(R6),3                                                          
         BNE   SSBAD                                                            
         CLC   22(3,R6),=C'NEW'                                                 
         BNE   SSBAD                                                            
         OI    RSTASTAT,X'20'                                                   
         B     SSX                                                              
         SPACE                                                                  
SS40     DS    0H              VALIDATE MANDATORY SCHED GRPS FOR SAR            
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   12(0,R6),=CL8'BUDGET'                                            
         BNE   SSBAD                                                            
         ZIC   R1,1(R6)                                                         
         LTR   R1,R1                                                            
         BZ    SSBAD                                                            
         CH    R1,=H'3'                                                         
         BH    SSBAD                                                            
         BCTR  R1,0                                                             
         EX    R1,YESCOMP                                                       
         BNE   SSBAD                                                            
         OI    RSTASTAT,X'10'                                                   
         B     SSX                                                              
         SPACE                                                                  
SSX      EQU   *                                                                
         LA    R6,32(R6)                                                        
         BCT   R5,SS10                                                          
SSGOOD   SR    R0,R0                                                            
*                                                                               
SSEX     LTR   R0,R0                                                            
         XIT1                                                                   
*                                                                               
SSBAD    LA    R0,1                                                             
         B     SSEX                                                             
         EJECT                                                                  
*********************************************************************           
*   SUBROUTINE TO EDIT RECEIVING ID FIELD                                       
*********************************************************************           
RIDEDT   NTR1                                                                   
         XC    ELEM,ELEM                                                        
         LA    R5,ELEM                                                          
         USING RSTAXEL,R5                                                       
         MVC   RSTAXEL(2),=X'0514'                                              
         MVC   RSTARSO,WORK                                                     
*                                                                               
         GOTO1 =A(GETID),DMCB,(RC),RR=RELO                                      
         CLC   HALF,=H'0'          INVALID ID                                   
         BE    RIDBAD                                                           
         MVC   RSTARID,HALF                                                     
         MVC   GRAPHLAG,HALF       SAVE ID FOR COMBO TESTING                    
         DROP  R5                                                               
         GOTO1 ADDELEM                                                          
RIDGOOD  EQU   *                                                                
         SR    R0,R0                                                            
RIDEX    EQU   *                                                                
         LTR   R0,R0                                                            
         XIT1                                                                   
RIDBAD   EQU   *                                                                
         LA    R0,1                                                             
         B     RIDEX                                                            
         EJECT                                                                  
**********************************************************************          
*   SUBROUTINE TO EDIT SIGN-ON ID FIELD                                         
**********************************************************************          
SOEDT    NTR1                                                                   
         L     R6,AIO3             R6 POINTS TO BLOCK                           
         LA    R5,ELEM                                                          
         XC    ELEM,ELEM           CLEAR ELEMENT AREA                           
         GOTO1 SCANNER,DMCB,(R2),(5,(R6)),0                                     
*                                                                               
         CLI   DMCB+4,0                                                         
         BE    SEDTBAD                                                          
         ZIC   R3,DMCB+4                                                        
         SPACE 1                                                                
         USING RSTASOEL,R5                                                      
         MVC   RSTASOEL(2),=X'0611'                                             
SOE10    MVC   WORK,SPACES                                                      
         ZIC   RF,0(R6)                                                         
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   WORK(0),12(R6)                                                   
         SPACE 1                                                                
         GOTO1 =A(GETID),DMCB,(RC),RR=RELO                                      
         CLC   HALF,=H'0'                                                       
         BE    SEDTBAD                                                          
         MVC   RSTASO,12(R6)                                                    
         MVC   RSTASID,HALF                                                     
         DROP  R5                                                               
         SPACE 1                                                                
         GOTO1 ADDELEM                                                          
         SPACE 1                                                                
         LA    R6,32(R6)                                                        
         BCT   R3,SOE10                                                         
SEDTGOOD EQU   *                                                                
         SR    R0,R0                                                            
SEDTEX   EQU   *                                                                
         LTR   R0,R0                                                            
         XIT1                                                                   
SEDTBAD  LA    R0,1                                                             
         B     SEDTEX                                                           
         EJECT                                                                  
**********************************************************************          
* SUB-ROUTINE TO EDIT CONTRACT COMMENT FIELD ON STATION SCREEN                  
**********************************************************************          
CMTEDT   NTR1                                                                   
         CLC   =C'C=',8(R2)        SUPPORT FOR FILE COMMENT                     
         BNE   CE30                                                             
         CLI   5(R2),2                                                          
         BH    *+14                                                             
         MVC   RERROR,=AL2(COMMIS) COMMENT CODE MISSING                         
         B     GEBAD                                                            
*                                                                               
         OC    8(14,R2),SPACES     BLANK PADDED                                 
         CLC   10(8,R2),SPACES                                                  
         BNE   *+14                                                             
         MVC   RERROR,=AL2(COMMIS) COMMENT CODE MISSING                         
         B     GEBAD                                                            
*                                                                               
         CLC   18(4,R2),SPACES                                                  
         BE    *+14                                                             
         MVC   RERROR,=AL2(COMFENT)                                             
         B     GEBAD               CMT CDE MUST BE FIRST AND ONLY ENTRY         
*                                                                               
         MVI   5(R2),10            KEEP ONLY COMMENT CODE                       
         XC    KEY,KEY                                                          
         LA    R3,KEY                                                           
         USING RCMTD,R3                                                         
         MVI   RCMTKTYP,X'2E'      RECORD TYPE                                  
         MVC   RCMTKREP,AGENCY     REP CODE                                     
         MVC   RCMTKOFF,=X'FFFF'   OFFICE CODE                                  
         MVC   RCMTKCDE,10(R2)     COMMENT CODE                                 
         DROP  R3                                                               
         GOTO1 HIGH                                                             
         CLC   KEY(L'RCMTKEY),KEYSAVE                                           
         BE    *+14                                                             
         MVC   RERROR,=AL2(COMNFND)                                             
         B     GEBAD               FILE COMMENT REC NOT FOUND                   
*                                                                               
         LA    R5,ELEM                                                          
         XC    ELEM,ELEM                                                        
         USING RSTACEL,R5                                                       
         MVI   RSTACCOD,3                                                       
         MVI   RSTACTYP,C'M'       FILE COMMENT = MANUAL CMT                    
         ZIC   R1,5(R2)                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   RSTACCMT(0),8(R2)                                                
         LA    R1,5(R1)            COMPUTE EL LEN                               
         STC   R1,RSTACLEN                                                      
         B     CE200                                                            
         DROP  R5                                                               
*                                                                               
CE30     EQU   *                                                                
         L     R6,AIO2             R6 POINTS TO BLOCK                           
         LA    R5,ELEM                                                          
         XC    ELEM,ELEM           CLEAR ELEMENT AREA                           
         GOTO1 SCANNER,DMCB,(60,(R2)),(1,(R6)),0                                
         CLI   DMCB+4,0                                                         
         BE    CEINVERR                                                         
         SPACE                                                                  
         USING RSTACEL,R5                                                       
         MVI   RSTACCOD,3                                                       
         CLI   1(R6),0                                                          
         BNE   CE100                                                            
         MVI   RSTACTYP,C'M'       UNDIVIDED FIELD IMPLIES MANUAL CMT           
         ZIC   R1,0(R6)                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   RSTACCMT(0),12(R6)                                               
         LA    R1,5(R1)            COMPUTE EL LEN                               
         STC   R1,RSTACLEN                                                      
         B     CE200                                                            
         SPACE                                                                  
CE100    EQU   *                                                                
         CLI   0(R6),1                                                          
         BNE   CEINVERR                                                         
         CLI   12(R6),C'L'         LIBRARY REFERENCE                            
         BNE   CEINVERR                                                         
         CLI   1(R6),4             VALUE CANNOT BE G.T. 9999                    
         BH    CEINVERR                                                         
         TM    3(R6),X'80'                                                      
         BNO   CEINVERR                                                         
         OC    8(4,R6),8(R6)                                                    
         BZ    CEINVERR                                                         
         MVI   RSTACTYP,C'L'                                                    
         MVC   RSTACNUM,10(R6)                                                  
         MVI   RSTACLEN,6                                                       
         DROP  R5                                                               
         SPACE                                                                  
CE200    GOTO1 ADDELEM                                                          
*                                                                               
CEGOOD   EQU   *                                                                
         SR    R0,R0                                                            
GEEX     EQU   *                                                                
         LTR   R0,R0                                                            
         XIT1                                                                   
GEBAD    LA    R0,1                                                             
         B     GEEX                                                             
*                                                                               
CEINVERR EQU   *                                                                
         MVC   RERROR,=AL2(INVERR)                                              
         B     GEBAD                                                            
         EJECT                                                                  
********************************************************************            
PACK     SR    R1,R1                                                            
         SR    R0,R0                                                            
         ZAP   DUB,=P'0'                                                        
         IC    R1,5(R2)                                                         
         LTR   R1,R1                                                            
         BCR   8,RE                EXIT ON ZERO LENGTH                          
         TM    4(R2),X'08'                                                      
         BCR   8,RE                        OR NON NUMERIC                       
         BCTR  R1,R0                                                            
         EX    R1,VARPACK                                                       
         CVB   R0,DUB                                                           
         BR    RE                                                               
         SPACE 2                                                                
VARPACK  PACK  DUB,8(0,R2)                                                      
         EJECT                                                                  
********************************************************************            
MOVE     MVI   WORK,C' '                                                        
         MVC   WORK+1(L'WORK-1),WORK                                            
         SR    R1,R1                                                            
         IC    R1,5(R2)                                                         
         LTR   R1,R1                                                            
         BCR   8,RE                EXIT ON ZERO LENGTH                          
         BCTR  R1,R0                                                            
         EX    R1,VARMOVE                                                       
         BR    RE                                                               
         SPACE 2                                                                
VARMOVE  MVC   WORK(0),8(R2)                                                    
         EJECT                                                                  
********************************************************************            
CLCNO    CLC   8(0,R2),=C'NO '                                                  
CLCYES   CLC   8(0,R2),=C'YES'                                                  
NOCOMP   CLC   22(0,R6),=C'NO '                                                 
YESCOMP  CLC   22(0,R6),=C'YES'                                                 
         SPACE 3                                                                
********************************************************************            
VRERR1   EQU   *                                                                
         MVC   RERROR,=AL2(MSSNGERR)                                            
         B     VRBAD                                                            
*                                                                               
VRERR2   EQU   *                                                                
         MVC   RERROR,=AL2(INVERR)                                              
         B     VRBAD                                                            
*                                                                               
VRERR3   EQU   *                                                                
         MVC   RERROR,=AL2(NUMERR)                                              
         B     VRBAD                                                            
*                                                                               
VRERR4   EQU   *                                                                
         MVC   RERROR,=AL2(NOCHGERR)                                            
         B     VRBAD                                                            
*                                                                               
VRERR5   EQU   *                                                                
         MVC   RERROR,=AL2(NOGRPERR)                                            
         B     VRBAD                                                            
*                                                                               
VRERR6   EQU   *                                                                
         MVC   RERROR,=AL2(OWNREQ)                                              
         B     VRBAD                                                            
*                                                                               
VRERR7   EQU   *                                                                
         MVC   RERROR,=AL2(ERRNF)  RECORD NOT FOUND                             
         B     VRBAD                                                            
*                                                                               
VRERR8   EQU   *                                                                
         LA    R2,FMSNEWH                                                       
         MVC   RERROR,=AL2(NOREPWDT)                                            
         B     VRBAD                                                            
*                                                                               
VRERR9   EQU   *                                                                
         MVC   RERROR,=AL2(INVALREP)                                            
         B     VRBAD                                                            
*                                                                               
VRERR10  EQU   *                                                                
         MVC   RERROR,=AL2(LCOMERR)                                             
         B     VRBAD                                                            
*                                                                               
VRERR11  EQU   *                                                                
         MVC   RERROR,=AL2(LRECFND)                                             
         B     VRBAD                                                            
*                                                                               
VRERR12  EQU   *                                                                
         MVC   RERROR,=AL2(STAERR)                                              
         B     VRBAD                                                            
*                                                                               
VRERR13  EQU   *                                                                
         MVC   RERROR,=AL2(NOECTYP)                                             
         B     VRBAD                                                            
*                                                                               
VRERR14  EQU   *                                                                
         MVC   RERROR,=AL2(FAXMBERR)                                            
         B     VRBAD                                                            
*                                                                               
VRERR15  EQU   *                                                                
         MVC   RERROR,=AL2(MBOXERR)                                             
         B     VRBAD                                                            
*                                                                               
VRERR16  EQU   *                                                                
         MVC   RERROR,=AL2(DESTERR)                                             
         B     VRBAD                                                            
*                                                                               
VRERR17  EQU   *                                                                
         MVC   RERROR,=AL2(GRPHERR)                                             
         B     VRBAD                                                            
*                                                                               
         EJECT                                                                  
********************************************************************            
*ETEL1   AH    R6,DATADISP                                                      
*IRSTEL1 CLI   0(R6),0                                                          
*        BNE   *+10                                                             
*        CLI   0(R6),1                                                          
*        BR    RE                                                               
*        CLI   ELCODE,0                                                         
*        BCR   8,RE                                                             
*        CLC   ELCODE,0(R6)                                                     
*        BCR   8,RE                                                             
*EXTEL1  SR    RF,RF                                                            
*        IC    RF,1(R6)                                                         
*        LTR   RF,RF                                                            
*        BNZ   *+6                                                              
*        DC    H'0'                                                             
*        AR    R6,RF                                                            
*        B     FIRSTEL1                                                         
*        EJECT                                                                  
*                                                                               
         DROP  R4                                                               
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
RELO     DS    A                                                                
PREFSTAT DC    C'N'                                                             
MYKEY    DS    CL27                                                             
         EJECT                                                                  
**********************************************************************          
*                  LIST RECORDS                                      *          
**********************************************************************          
LR       NMOD1 0,*LSTREC*                                                       
         L     RC,0(R1)                                                         
         L     RA,4(R1)            TWA                                          
*                                                                               
         OC    KEY,KEY             FIRST TIME THROUGH?                          
         BNZ   LR15                NO                                           
*                                                                               
         LA    R4,KEY                                                           
         USING STATD,R4                                                         
         MVI   RSTAKTYP,X'02'      STATION RECORD TYPE                          
         MVC   RSTAKREP(2),AGENCY  REP CODE                                     
*                                                                               
         LA    R2,LSTSTATH                                                      
         CLI   5(R2),0                                                          
         BE    LR10                                                             
*                                                                               
         XC    BLOCK(256),BLOCK                                                 
         MVC   WORK(50),SPACES                                                  
         GOTO1 SCANNER,DMCB,(R2),(2,BLOCK),C',=,-'                              
         MVC   RERROR,=AL2(INVSTA)                                              
         LA    R5,BLOCK                                                         
*                                                                               
         CLI   0(R5),4                                                          
         BH    LRBAD                                                            
         TM    2(R5),X'40'         TEST ALPHA                                   
         BZ    LRBAD                                                            
         MVC   RSTAKSTA(4),12(R5)  MOVE CALL LETTERS INTO THE KEY               
*                                                                               
         SR    RE,RE                                                            
         ICM   RE,1,1(R5)          DEFAULT = TV                                 
         BZ    LR10                YES                                          
         BCTR  RE,0                                                             
         EX    RE,STATV2           TV LEAVE BLANK                               
         BE    LR10                                                             
         MVI   RSTAKSTA+4,C'L'         L = LO POWER                             
         EX    RE,STALP2                                                        
         BE    LR10                                                             
         MVI   RSTAKSTA+4,C'A'         AM = A                                   
         EX    RE,STAAM2                                                        
         BE    LR10                                                             
         MVI   RSTAKSTA+4,C'F'         FM = F                                   
         EX    RE,STAFM2                                                        
         BE    LR10                                                             
         MVI   RSTAKSTA+4,C'C'         CM = C                                   
         EX    RE,STACM2                                                        
         BE    LR10                                                             
         MVI   RSTAKSTA+4,C' '         MAY BE SATELLITE STATION                 
         EX    RE,STA1V2                                                        
         BNE   LR05                                                             
         MVI   RSTAKSTA+4,C'1'                                                  
         B     LR10                                                             
LR05     EX    RE,STA2V2                                                        
         BNE   LRBAD                                                            
         MVI   RSTAKSTA+4,C'2'                                                  
*                                                                               
LR10     EQU   *                                                                
         OC    RSTAKSTA,SPACES                                                  
         DROP  R4                                                               
LR15     EQU   *                                                                
         GOTO1 HIGH                                                             
*                                                                               
LR20     EQU   *                                                                
         CLC   KEY(22),KEYSAVE     ALL RECORDS HAVE BEEN DISPLAYED?             
         BNE   LRGOOD              YES                                          
*                                                                               
         GOTO1 GETREC                                                           
*                                                                               
         XC    LISTAR,LISTAR                                                    
*                                                                               
         L     R4,AIO                                                           
         USING STATD,R4                                                         
         MVC   LISTSTA(4),RSTAKSTA                                              
         CLI   RSTAKSTA+4,C' '                                                  
         BE    LR25                                                             
         MVI   LISTSTA+4,C'-'                                                   
         MVC   LISTSTA+5(1),RSTAKSTA+4                                          
         DROP  R4                                                               
*                                                                               
LR25     EQU   *                                                                
         L     R6,AIO                                                           
         MVI   ELCODE,X'01'                                                     
         BAS   RE,GETEL2                                                        
         BNE   LR30                                                             
*                                                                               
         USING RSTAELEM,R6                                                      
         MVC   LISTMKT(L'RSTAMKT),RSTAMKT                                       
         MVC   LISTGRP(L'RSTAGRUP),RSTAGRUP                                     
         MVC   LISTAFF(L'RSTAAFFL),RSTAAFFL                                     
         GOTO1 DATCON,DMCB,(3,RSTASTRT),(8,LISTJDT)                             
         GOTO1 DATCON,DMCB,(3,RSTAEND),(8,LISTLDT)                              
         DROP  R6                                                               
         GOTO1 LISTMON                                                          
*                                                                               
LR30     EQU   *                                                                
         MVC   KEYSAVE(27),KEY                                                  
         GOTO1 SEQ                                                              
         B     LR20                                                             
*                                                                               
LRGOOD   EQU   *                                                                
         SR    R0,R0                                                            
         B     LREX                                                             
*                                                                               
LRBAD    LA    R0,1                                                             
*                                                                               
LREX     EQU   *                                                                
         LTR   R0,R0               SET THE CONDITION CODE                       
         XIT1  REGS=(R2)                                                        
         EJECT                                                                  
********************************************************************            
GETEL2   AH    R6,DATADISP                                                      
FIRSTEL2 CLI   0(R6),0                                                          
         BNE   *+10                                                             
         CLI   0(R6),1                                                          
         BR    RE                                                               
         CLI   ELCODE,0                                                         
         BCR   8,RE                                                             
         CLC   ELCODE,0(R6)                                                     
         BCR   8,RE                                                             
NEXTEL2  SR    RF,RF                                                            
         IC    RF,1(R6)                                                         
         LTR   RF,RF                                                            
         BNZ   *+6                                                              
         DC    H'0'                                                             
         AR    R6,RF                                                            
         B     FIRSTEL2                                                         
         EJECT                                                                  
*                                                                               
STATV2   CLC   22(0,R5),=C'TV'                                                  
STAAM2   CLC   22(0,R5),=C'AM'                                                  
STAFM2   CLC   22(0,R5),=C'FM'                                                  
STACM2   CLC   22(0,R5),=C'CM'                                                  
STALP2   CLC   22(0,R5),=C'L'                                                   
STA1V2   CLC   22(0,R5),=C'1'                                                   
STA2V2   CLC   22(0,R5),=C'2'                                                   
*                                                                               
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
                                                                                
**********************************************************************          
*                  DISPLAY KEY                                       *          
**********************************************************************          
                                                                                
DK       NMOD1 0,*DISKEY*                                                       
         L     RC,0(R1)                                                         
         L     RA,4(R1)            TWA                                          
*                                                                               
         L     R4,AIO                                                           
         USING STATD,R4                                                         
         MVC   FMSSTA(L'FMSSTA),SPACES                                          
         MVC   FMSSTA(4),RSTAKSTA                                               
         CLI   RSTAKSTA+4,C' '                                                  
         BE    DKGOOD                                                           
         CLI   FMSSTA+3,C' '                                                    
         BNE   DK10                                                             
         MVI   FMSSTA+3,C'-'                                                    
         MVC   FMSSTA+4(1),RSTAKSTA+4                                           
         B     *+14                                                             
DK10     EQU   *                                                                
         MVI   FMSSTA+4,C'-'                                                    
         MVC   FMSSTA+5(1),RSTAKSTA+4                                           
*                                                                               
DKGOOD   EQU   *                                                                
         OI    FMSSTAH+6,X'80'                                                  
         SR    R0,R0                                                            
         B     DKEX                                                             
*                                                                               
DKBAD    LA    R0,1                                                             
*                                                                               
DKEX     EQU   *                                                                
         LTR   R0,R0               SET THE CONDITION CODE                       
         XIT1                                                                   
         DROP  R4                                                               
         EJECT                                                                  
**********************************************************************          
       ++INCLUDE DDFLDIND          EQUATED INDICATORS FOR FOUT                  
       ++INCLUDE REBASINC                                                       
       ++INCLUDE REBASTWA                                                       
         SPACE 2                                                                
*                                                                               
         ORG   CONTAGH                                                          
       ++INCLUDE REBASAFD          MAINTENANCE SCREEN                           
         ORG   CONTAGH                                                          
       ++INCLUDE REBASD0D          LIST SCREEN                                  
         EJECT                                                                  
**********************************************************************          
*   SUBROUTINE TO EDIT FORMER REP/NEW REP FIELDS -- X'0C' ELEMENT               
**********************************************************************          
         CSECT                                                                  
FNEDT    NMOD1 0,**FNEDT*,RR=R4                                                 
         L     RC,0(R1)                                                         
         L     RA,4(R1)                                                         
         L     R4,AIO                                                           
         USING STATD,R4                                                         
*                                                                               
         LA    R5,ELEM                                                          
         USING RSTAFNEL,R5                                                      
         MVI   RSTAFNEC,X'0C'      BUILD X'0C' ELEMENT                          
         MVI   RSTAFNLN,8                                                       
*                                                                               
         CLI   RSTAKSTA+4,C' '     IS THIS TV STATION (BAND IS EMPTY)?          
         BE    FN20                YES                                          
         CLI   RSTAKSTA+4,C'L'     IS THIS TV STATION (LOW POWER)?              
         BE    FN20                YES                                          
*                                                                               
         LA    R3,RAREPTAB                                                      
         LA    R6,L'RAREPTAB                                                    
         B     FN30                                                             
*                                                                               
FN20     EQU   *                                                                
         LA    R3,REPIDS                                                        
         LA    R6,L'REPIDS                                                      
*                                                                               
FN30     EQU   *                                                                
         LA    R2,FMSFORH          FORMER REP FIELD                             
         CLI   5(R2),0             IS ANYTHING THERE?                           
         BNE   *+14                YES                                          
         MVC   RSTAFNFO,SPACES     NO, MOVE IN SPACES                           
         B     FN50                EDIT NEW REP FIELD                           
         OC    FMSFOR,SPACES                                                    
         BAS   RE,VERIREP          VERIFY THE REP                               
         BNE   FNNO                INVALID REP                                  
         MVC   RSTAFNFO,FMSFOR     MOVE IN THE REP                              
*                                                                               
FN50     LA    R2,FMSNEWH          NEW REP FIELD                                
         CLI   5(R2),0             ANYTHING THERE?                              
         BNE   *+14                YES                                          
         MVC   RSTAFNNE,SPACES                                                  
         B     FN100               ADD THE ELEMENT                              
         OC    FMSNEW,SPACES                                                    
         BAS   RE,VERIREP          VERIFY THE REP                               
         BNE   FNNO                INVALID REP                                  
         MVC   RSTAFNNE,FMSNEW     MOVE IN THE REP                              
*                                                                               
         DROP  R5                                                               
FN100    GOTO1 ADDELEM                                                          
         B     FNYES               NO ERROR                                     
         SPACE 2                                                                
*                                                                               
* R6 HAS LENGTH OF EACH ENTRY IN TABLE                                          
*                                                                               
VERIREP  NTR1                                                                   
VERI10   CLI   0(R3),X'FF'         END OF TABLE?                                
         BE    FNNO                YES, INVALID REP                             
         CLC   8(3,R2),0(R3)       CORRECT REP?                                 
         BE    FNYES                                                            
         LA    R3,0(R3,R6)         GO TO NEXT REP                               
         B     VERI10                                                           
         SPACE 1                                                                
*                                                                               
FNYES    SR    RC,RC                                                            
FNNO     LTR   RC,RC                                                            
         XIT1  REGS=(R2)           SAVE THE CURSOR POSITION                     
         DROP  R4                                                               
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
       ++INCLUDE DDDARETAB         REP IDS                                      
       ++INCLUDE REPREPS           REP INITIALS                                 
         EJECT                                                                  
********************************************************************            
*  CHK0AELS:  THE ORDER OF THE STATIONS MAY NOT BE CHANGED.  BECAUSE            
*    THE 'PREFERRED STATION' FOR PRINT REPORTS MAY BE CHANGED, THE              
*    PREVIOUSLY VALID BIT CANNOT BE RELIED UPON TO INDICATE THAT THE            
*    STATIONS, ONCE ENTERED, ARE IN THE SAME SEQUENCE.                          
********************************************************************            
         CSECT                                                                  
CHK0AELS NMOD1 0,**CK0A**                                                       
         L     RC,0(R1)                                                         
         L     RA,4(R1)                                                         
         L     R4,AIO                                                           
         USING STATD,R4                                                         
         LA    R1,RSTAELEM         A(DESCRIPTIVE ELT OF STATION REC)            
CHKA0010 EQU   *                                                                
         CLI   0(R1),X'00'         END OF RECORD?                               
         BE    CHKA0050            YES - EXIT OKAY                              
         CLI   0(R1),X'0A'         COMBO STA ELEMENT?                           
         BE    CHKA0020            YES -                                        
CHKA0015 EQU   *                                                                
         ZIC   RF,1(R1)            NO  - BUMP TO NEXT NEW X'0A'                 
         AR    R1,RF                                                            
         B     CHKA0010            GO BACK FOR NEXT                             
CHKA0020 EQU   *                                                                
         LA    R2,COMBAREA         A(OLD X'0A' ELEMENTS)                        
CHKA0021 EQU   *                                                                
         OC    0(4,R2),0(R2)       ANY OLD X'0A' AT LOC?                        
         BZ    CHKA0015            NO  - LOOK FOR NEXT NEW X'0A'                
         CLC   2(5,R1),2(R2)       YES - NEW VS OLD COMBO STATION               
         BE    CHKA0030            FOUND - SET DATES FROM OLD                   
         ZIC   RF,1(R2)            N.F.  - BUMP TO NEXT OLD X'0A'               
         AR    R2,RF                                                            
         B     CHKA0021            GO BACK FOR NEXT OLD                         
CHKA0030 EQU   *                                                                
         CLI   1(R2),8             OLD:  OLD FORMAT? (LEN < 9)                  
         BH    CHKA0040            NO  - NEW FORMAT                             
         XC    8(2,R1),8(R1)       YES - STATION PREVIOUSLY ENTERED,            
*                                     NO DATE IN ELT                            
         B     CHKA0015            GO BACK FOR NEXT NEW                         
CHKA0040 EQU   *                                                                
         MVC   8(2,R1),8(R2)       NEW FORMAT:  USE DATE                        
*                                     FROM OLD ELEMENT                          
         B     CHKA0015            GO BACK FOR NEXT NEW                         
CHKA0050 EQU   *                                                                
         BAS   RE,CHK0AORD         CHECK ORDER                                  
*                                                                               
*   ABOVE TO BE REMOVED WHEN STATIONS CAN BE DELETED                            
*                                                                               
         BNZ   CHKA0060            ERROR FOUND                                  
         SR    R0,R0               NO ERROR = SET CC ZERO                       
         LTR   R0,R0                                                            
CHKA0060 EQU   *                                                                
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
*  CHK0AORD:  THE ORDER OF THE STATIONS MAY NOT BE CHANGED.  BECAUSE            
*    THE 'PREFERRED STATION' FOR PRINT REPORTS MAY BE CHANGED, THE              
*    PREVIOUSLY VALID BIT CANNOT BE RELIED UPON TO INDICATE THAT THE            
*    STATIONS, ONCE ENTERED, ARE IN THE SAME SEQUENCE.                          
*                                                                               
CHK0AORD NTR1                                                                   
         LA    R1,RSTAELEM         A(DESCRIPTIVE ELT OF STATION REC)            
         LA    R2,COMBAREA         A(OLD X'0A' ELEMENTS)                        
CHKO0010 EQU   *                                                                
         CLI   0(R1),X'00'         END OF RECORD?                               
         BE    CHKO0040            YES - EXIT OKAY                              
         CLI   0(R1),X'0A'         COMBO STA ELEMENT?                           
         BNE   CHKO0020            NO  - GET NEXT                               
         OC    0(8,R2),0(R2)       ANY OLD X'0A' AT LOC?                        
         BZ    CHKO0040            NO  - FINISHED - EXIT OKAY                   
         CLC   2(5,R1),2(R2)       NEW VS OLD COMBO STATION                     
         BNE   CHKO0030            NOT SAME - EXIT N.G.                         
         ZIC   RF,1(R2)            BUMP TO NEXT OLD X'0A'                       
         AR    R2,RF                                                            
CHKO0020 EQU   *                                                                
         ZIC   RF,1(R1)            BUMP TO NEXT NEW X'0A'                       
         AR    R1,RF                                                            
         B     CHKO0010            GO BACK FOR NEXT                             
CHKO0030 EQU   *                                                                
         LA    RF,1                SET CC = NO GOOD                             
         LTR   RF,RF                                                            
         B     CHKO0050            EXIT                                         
CHKO0040 EQU   *                                                                
         OC    0(7,R2),0(R2)       LAST CHECK: ANY OLD X'0A' LEFT?              
         BNZ   CHKO0030            YES - TOO FEW NEW X'0A'S:  ERROR             
         SR    RF,RF               OKAY - SET CC = ZERO                         
         LTR   RF,RF                                                            
CHKO0050 EQU   *                                                                
         XIT1                                                                   
         DROP  R4                                                               
         EJECT                                                                  
********************************************************************            
*        OPTEDT -  EDIT AND LOAD ALL FIELDS ON THE STATION RECORD               
*                   EXTENDED DESCRIPTION ELEMENT                                
********************************************************************            
OPTEDT   NMOD1 0,*OPTEDT*                                                       
         L     RC,0(R1)                                                         
         L     RA,4(R1)                                                         
*                                                                               
         L     R4,AIO                                                           
         USING STATD,R4                                                         
*                                                                               
         LA    R5,ELEM        BUILD X'08' (EXTRA DESCRIPTION ELEMENT)           
         XC    ELEM,ELEM                                                        
         USING RSTAXXEL,R5                                                      
         MVC   RSTAXXEL(2),=X'0850'                                             
         MVI   RSTAOPTS,C'N'                                                    
         MVC   RSTAOPTS+1(8),RSTAOPTS                                           
         MVI   RSTAOPTA,0                                                       
*                                                                               
         LA    R2,FMSOPTH                                                       
*                                                                               
OE06     DS    0H                                                               
         OC    FMSOPT,SPACES                                                    
         LA    R1,17               LOOP THRU 17 OPTIONS                         
         LA    R3,FMSOPT                                                        
OE07     CLI   0(R3),C' '                                                       
         BNE   OE08                                                             
         MVI   0(R3),C'N'                                                       
OE08     CLI   0(R3),C'N'                                                       
         BE    OE09                                                             
         CLI   0(R3),C'Y'                                                       
         BNE   OEERR1                                                           
OE09     LA    R3,1(R3)                                                         
         BCT   R1,OE07                                                          
*                                                                               
         MVC   RSTAOPTS(9),FMSOPT  STORE FIRST 9 OPTIONS AS CHARACTER           
         LA    R2,FMSOPT+9                                                      
         LA    R6,RSTAOPTA                                                      
         BAS   RE,PUTPROF          STORE LAST 8 PROFILES                        
*                                                                               
OE30     CLI   RSTAOPTS+2,C'Y'                                                  
         BNE   OE31                                                             
         OI    RSTASTAT,X'02'      INDICATE "DON'T SEND" IS ALLOWED             
*                                                                               
*- UNCOUPLE DESTINATION ID FIELD FROM TRAFFIC SYSTEM.                           
OE31     EQU   *                                                                
         LA    R2,FMSRDSH          REP OFFICE DESTINATION ID                    
         CLI   5(R2),0             ANY I/P                                      
         BZ    OE40                                                             
*                                                                               
         BAS   RE,MOVE2                                                         
         GOTO1 =A(GETID),DMCB,(RC),RR=RELO                                      
         CLC   HALF,=H'0'          VALID ID?                                    
         BE    OE32                NO - CHECK IF FAX/MAILBOX NUMBER             
         CLC   =C'GRAPH',FMSRDS                                                 
         BE    OEERR16                                                          
         MVC   RSTAORDS(8),WORK    YES - SAVE OFF ID                            
         MVC   RSTAORID(2),HALF                                                 
         B     OE40                                                             
OE32     EQU   *                                                                
         CLI   FMSTRF,C'G'                                                      
         BE    *+12                                                             
         CLI   FMSTRF,C'A'                                                      
         BNE   OEERR1                                                           
         ZIC   RF,5(R2)            DERIVE LENGTH                                
         LA    RE,8(R2)            SET A(FIELD)                                 
         CLC   =C'MB=',0(RE)       MAIL BOX NUMBER?                             
         BNE   OE34                NO                                           
         CH    RF,=H'11'           MAILBOX LENGTH MUST BE 8 (+3 FOR             
*                                     KEYWORD MB=)                              
         BNE   OEERR14                                                          
         LA    R1,3                YES                                          
         SR    RF,R1               SUBTRACT 3 FROM LENGTH FOR 'MB='             
         LA    RE,3(RE)            BUMP PAST 'MB='                              
OE34     EQU   *                                                                
         CLI   0(RE),C'0'          ZERO OR GREATER?                             
         BL    OEERR15             NO                                           
         CLI   0(RE),C'9'          NINE OR LESS?                                
         BH    OEERR15             NO                                           
         LA    RE,1(RE)                                                         
         BCT   RF,OE34             GO BACK FOR NEXT                             
         CLC   =C'011',8(R2)       INTERNATIONAL FAX NUMBER?                    
         BE    OE35                YES - SPECIAL TREATMENT FOR NUMBER           
         MVC   RSTAOFX2(13),FMSRDS NO  - SAVE NUMBER AS ENTERED                 
         B     OE38                                                             
OE35     EQU   *                                                                
         MVI   RSTAOFX2,0          SET 1ST BYTE TO BINARY ZERO                  
*                                     TO SERVE AS FLAG                          
         MVI   RSTAOFX2+1,11       SET INTERNATIONAL CODE                       
*                                                                               
*   THIS IS REDUNDANT AT THIS TIME, AS '011' IS THE ONLY INTERNAT-              
*        IONAL CODE WE ARE USING.  LATER, IF MORE ARE USED, THIS                
*        BECOMES MORE MEANINGFUL.                                               
*                                                                               
         ZIC   RF,5(R2)            RETRIEVE LENGTH AGAIN                        
         LA    RE,4                SUBTRACT FOR 011 + EX                        
         SR    RF,RE                                                            
         EX    RF,OE36             PACK BY LENGTH                               
         B     OE37                                                             
OE36     PACK  DUB(8),11(0,R2)     PACK NUMBER AFTER 011                        
*                                                                               
OE37     EQU   *                                                                
         LA    RF,1(RF)            ADD 1 BACK FROM EX                           
         STC   RF,RSTAOFX2+2       SAVE LENGTH OF DATA FOR EDITING              
*                                     SIGNIFICANT DIGITS                        
         MVC   RSTAOFX2+3(8),DUB   SAVE INTERNATIONAL NUMBER                    
*                                                                               
* IN THE CASE THAT WE HAVE A FAX,MB,ETC. NUMBER IN THE DEST FIELD, WE           
* FORCE THE DEST ID IN THE RSTAREC TO BE 'GRAPH'                                
*                                                                               
OE38     EQU   *                                                                
         XC    WORK,WORK                                                        
         MVC   WORK(8),=C'GRAPH   '                                             
         GOTO1 =A(GETID),DMCB,(RC),RR=RELO                                      
         CLC   HALF,=H'0'                                                       
         BNE   *+6                                                              
         DC    H'0'                'GRAPH' HAD BETTER BE VALID                  
         MVC   RSTAORDS(8),WORK                                                 
         MVC   RSTAORID(2),HALF                                                 
OE40     EQU   *                                                                
         LA    R2,FMSFAXH          STATION FAX NUMBER                           
         CLI   5(R2),0                                                          
         BZ    OE50                                                             
         ZIC   RF,5(R2)            DERIVE LENGTH                                
         LA    RE,8(R2)            SET A(FIELD)                                 
         CLC   =C'MB=',0(RE)       MAIL BOX NUMBER?                             
         BNE   OE42                NO                                           
         CH    RF,=H'11'           MAILBOX LENGTH MUST BE 8 (+3 FOR             
*                                     KEYWORD MB=)                              
         BNE   OEERR14                                                          
         LA    R1,3                YES                                          
         SR    RF,R1               SUBTRACT 3 FROM LENGTH FOR 'MB='             
         LA    RE,3(RE)            BUMP PAST 'MB='                              
OE42     EQU   *                                                                
         CLI   0(RE),C'0'          ZERO OR GREATER?                             
         BL    OEERR13             NO                                           
         CLI   0(RE),C'9'          NINE OR LESS?                                
         BH    OEERR13             NO                                           
         LA    RE,1(RE)                                                         
         BCT   RF,OE42             GO BACK FOR NEXT                             
         CLC   =C'011',8(R2)       INTERNATIONAL FAX NUMBER?                    
         BE    OE45                YES - SPECIAL TREATMENT FOR NUMBER           
         MVC   RSTAOFAX(13),FMSFAX NO  - SAVE NUMBER AS ENTERED                 
         B     OE50                                                             
OE45     EQU   *                                                                
         MVI   RSTAOFAX,0          SET 1ST BYTE TO BINARY ZERO                  
*                                     TO SERVE AS FLAG                          
         MVI   RSTAOFAX+1,11       SET INTERNATIONAL CODE                       
*                                                                               
*   THIS IS REDUNDANT AT THIS TIME, AS '011' IS THE ONLY INTERNAT-              
*        IONAL CODE WE ARE USING.  LATER, IF MORE ARE USED, THIS                
*        BECOMES MORE MEANINGFUL.                                               
*                                                                               
         ZIC   RF,5(R2)            RETRIEVE LENGTH AGAIN                        
         LA    RE,4                SUBTRACT FOR 011 + EX                        
         SR    RF,RE                                                            
         EX    RF,OE46             PACK BY LENGTH                               
         B     OE47                                                             
OE46     PACK  DUB(8),11(0,R2)     PACK NUMBER AFTER 011                        
*                                                                               
OE47     EQU   *                                                                
         LA    RF,1(RF)            ADD 1 BACK FROM EX                           
         STC   RF,RSTAOFAX+2       SAVE LENGTH OF DATA FOR EDITING              
*                                     SIGNIFICANT DIGITS                        
         MVC   RSTAOFAX+3(8),DUB   SAVE INTERNATIONAL NUMBER                    
OE50     EQU   *                                                                
*                                                                               
         LA    R2,FMSINTH          STATION INTERFACE CODE                       
         CLI   5(R2),0                                                          
         BNZ   OE58                                                             
         LA    R2,SVPGPBIT         PROGRAM PROFILE BITS                         
         TM    0(R2),X'80'         0TH BIT ON = INTERFACE CDE NEEDED            
         BNO   OE60                NOT REQUIRED                                 
         LR    R2,RA                CHECK FOR DDS TERMINAL                      
         USING TWAD,R2                                                          
         CLI   TWAOFFC,C'*'        DDS TERMINAL?                                
         BE    OE60                YES - IGNORE TEST                            
         DROP  R2                                                               
         B     OEERR5              REQUIRED = ERROR                             
OE58     MVC   RSTAOSI(10),FMSINT                                               
OE60     EQU   *                                                                
*                                                                               
*- DESTINATION FORMAT/CONTRACT STATUS FILTER REQUIRED IF DEST ID GIVEN.         
         LA    R2,FMSRWSH               DESTINATION FORMAT                      
         CLI   5(R2),0                                                          
         BNE   OE62                                                             
         OC    RSTAORID,RSTAORID                                                
         BZ    OE70                                                             
         B     OEERR1              REQUIRED, BUT NOT GIVEN.                     
*                                                                               
*- FORMAT MAY BE ANY VALID TRAFFIC SYSTEM OR BLANK (REP FORMAT)                 
OE62     EQU   *                                                                
         MVI   RSTARWS,0           ASSUME 0.                                    
*                                                                               
         OC    RSTAOFX2,RSTAOFX2   IF 2ND FAX# ENTERED, DEST FMT                
         BZ    OE63                MUST BE A OR G                               
         CLI   FMSRWS,C'G'                                                      
         BE    OE63                                                             
         CLI   FMSRWS,C'A'                                                      
         BNE   OEERR1                                                           
*                                                                               
OE63     EQU   *                                                                
         CLI   FMSRWS,0            SCREEN INPUT 0 OR BLANK?                     
         BE    OE65                YES = LEAVE FORMAT IN ELEM AS 0.             
         CLI   FMSRWS,C' '                                                      
         BE    OE65                                                             
*                                                                               
         GOTO1 =A(EDITTRAF),DMCB,RR=RELO                                        
*                                  EDIT FORMAT AS TRAFFIC SYSTEM                
         BNZ   OEERR1                                                           
*                                                                               
         MVC   RSTARWS(1),FMSRWS   DESTINATION FORMAT CODE.                     
*                                                                               
*- 2ND BYTE OF FORMAT MUST BE C/U/B  (CONF/UNCONF/BOTH)                         
OE65     EQU   *                                                                
         MVC   RSTAWSCF(1),FMSRWS+1  CONTRACT FILTER BYTE                       
*                                                                               
         CLI   FMSRWS+1,C'C'                                                    
         BE    OE70                                                             
         CLI   FMSRWS+1,C'U'                                                    
         BE    OE70                                                             
         CLI   FMSRWS+1,C'B'                                                    
         BNE   OEERR1                                                           
*                                                                               
OE70     EQU   *                                                                
         LA    R2,FMSMCDH                                                       
         CLI   5(R2),0                                                          
         BNE   OE78                                                             
         LA    R2,SVPGPBIT         PROGRAM PROFILE BITS                         
         TM    0(R2),X'40'         1TH BIT ON = MARKET CDE NEEDED               
         BNO   OE80                NOT REQUIRED                                 
         LR    R2,RA                CHECK FOR DDS TERMINAL                      
         USING TWAD,R2                                                          
         CLI   TWAOFFC,C'*'        DDS TERMINAL?                                
         BE    OE80                YES - IGNORE TEST                            
         DROP  R2                                                               
         B     OEERR6              REQUIRED = ERROR                             
*                                                                               
OE78     GOTO1 =A(DISPMKT),DMCB,(RC),(RA),RR=RELO                               
         BNZ   OEERR4                                                           
         MVC   RSTAMKTC(4),FMSMCD                                               
OE80     EQU   *                                                                
*                                                                               
* SECONDARY AFFILIATE                                                           
         LA    R2,FMSAF2H                                                       
*                                                                               
         L     R1,ADDRLIST                                                      
         CLI   5(R2),0                                                          
         BE    OE90                                                             
*                                                                               
         CLC   0(3,R1),8(R2)                                                    
         BE    *+20                                                             
         LA    R1,3(R1)                                                         
         CLI   0(R1),0                                                          
         BNE   *-18                                                             
         B     OEERR1                                                           
*                                                                               
         MVC   RSTAAFL2,8(R2)                                                   
OE90     DS    0H                                                               
*                                                                               
* TIME ZONE                                                                     
         LA    R2,FMSTZH                                                        
         CLI   5(R2),0                                                          
         BE    OE95                                                             
         CLI   5(R2),1                                                          
         BNE   OEERR1                                                           
         LA    RF,TZTAB                                                         
         LA    R1,4                                                             
*                                                                               
         CLC   8(1,R2),0(RF)                                                    
         BE    *+16                                                             
         LA    RF,1(RF)                                                         
         BCT   R1,*-14                                                          
         B     OEERR1                                                           
*                                                                               
         MVC   RSTATZ(1),8(R2)                                                  
         B     OE95                                                             
TZTAB    DC    C'ECMP'             VALID TIME ZONES                             
OE95     DS    0H                                                               
*                                                                               
* ELECTRONIC CONTRACT                                                           
         LA    R2,FMSECH                                                        
         NI    RSTAXOPT,X'FF'-X'80'  ASSUME NO                                  
         ZIC   R1,5(R2)                                                         
         LTR   R1,R1                                                            
         BZ    OE100                                                            
         BCTR  R1,0                                                             
*                                                                               
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   8(0,R2),=C'YES'                                                  
         BE    OE97                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   8(0,R2),=C'NO '                                                  
         BE    OE100                                                            
         B     OEERR1                                                           
OE97     EQU   *                                                                
         CLI   RSTATRAF,C'B'       BIAS TRAFFIC STATION?                        
         BE    OE98                YES                                          
         CLI   RSTATRAF,C'W'       ALTERNATE BIAS TRAFFIC STATION?              
         BE    OE98                YES                                          
         CLI   RSTATRAF,C'K'       ENTERPRISE TRAFFIC STATION?                  
         BE    OE98                YES                                          
         CLI   RSTATRAF,C'C'       COLUMBINE  TRAFFIC STATION?                  
         BE    OE98                YES                                          
         CLI   RSTATRAF,C'V'       VCI  TRAFFIC STATION?                        
         BE    OE98                YES                                          
         CLI   RSTATRAF,C'J'       JDS/2000 TRAFFIC STATION?                    
         BNE   OEERR12             NO  - NOT EC TRAFFIC FORMAT                  
OE98     EQU   *                                                                
         OI    RSTAXOPT,X'80'      YES                                          
OE100    DS    0H                                                               
*                                                                               
* LUID                                                                          
         LA    R2,FMSLUH                                                        
         CLI   5(R2),0                                                          
         BE    OE107                                                            
         CLI   5(R2),8                                                          
         BNE   OEERR1                                                           
*                                                                               
         XC    KEY,KEY                                                          
         MVI   KEY,C'T'                                                         
         MVC   KEY+7(8),8(R2)                                                   
*                                                                               
         GOTO1 DATAMGR,DMCB,=C'DMREAD',=C'CTFILE',KEY,AIO2                      
         L     R3,AIO2                                                          
         CLC   26(R3),KEY                                                       
         BNE   OEERR1                                                           
* NEED A HOME-BREW GETEL FOR CTFILE RECS                                        
         LA    R1,28(R3)                                                        
         SR    RF,RF                                                            
*                                                                               
OE103    CLI   0(R1),0                                                          
         BE    OEERR1                                                           
         IC    RF,1(R1)                                                         
         AR    R1,RF                                                            
         CLI   0(R1),X'20'                                                      
         BNE   OE103                                                            
*                                                                               
         USING CTIDD,R1                                                         
OE104    CLC   22(4,R4),CTID                                                    
         BNE   OE103                                                            
         MVC   RSTALUID,FMSLU                                                   
OE107    DS    0H                                                               
         DROP  R1                                                               
*                                                                               
* INVOICE                                                                       
         LA    R2,FMSINVH                                                       
         NI    RSTAXOPT,X'FF'-X'40'  ASSUME NO                                  
         ZIC   R1,5(R2)                                                         
         LTR   R1,R1                                                            
         BZ    OE110                                                            
         BCTR  R1,0                                                             
*                                                                               
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   8(0,R2),=C'YES'                                                  
         BE    OE108                                                            
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   8(0,R2),=C'NO '                                                  
         BE    OE110                                                            
         B     OEERR1                                                           
OE108    EQU   *                                                                
         OI    RSTAXOPT,X'40'      YES                                          
OE110    EQU   *                                                                
*                                                                               
         DROP  R5                                                               
         GOTO1 ADDELEM                                                          
*                                                                               
OEGOOD   EQU   *                                                                
         LA    R0,0                                                             
OEEXIT   EQU   *                                                                
         LTR   R0,R0                                                            
         XIT1  REGS=(R2)                                                        
         DROP  R4                                                               
OEBAD    LA    R0,1                                                             
         B     OEEXIT                                                           
*                                                                               
OEERR1   EQU   *                                                                
         MVC   RERROR,=AL2(INVERR)                                              
         B     OEBAD                                                            
OEERR4   EQU   *                                                                
         MVC   RERROR,=AL2(ERRNF)  RECORD NOT FOUND                             
         B     OEBAD                                                            
OEERR5   EQU   *                                                                
         MVC   RERROR,=AL2(MSSNGERR)                                            
         B     OEBAD                                                            
OEERR6   EQU   *                                                                
         MVC   RERROR,=AL2(MSSNGERR)                                            
         B     OEBAD                                                            
OEERR12  EQU   *                                                                
         MVC   RERROR,=AL2(NOECTYP)                                             
         B     OEBAD                                                            
OEERR13  EQU   *                                                                
         MVC   RERROR,=AL2(FAXMBERR)                                            
         B     OEBAD                                                            
OEERR14  EQU   *                                                                
         MVC   RERROR,=AL2(MBOXERR)                                             
         B     OEBAD                                                            
OEERR15  EQU   *                                                                
         MVC   RERROR,=AL2(DESTERR)                                             
         B     OEBAD                                                            
OEERR16  EQU   *                                                                
         MVC   RERROR,=AL2(GRPHERR)                                             
         B     OEBAD                                                            
         EJECT                                                                  
*********************************************************************           
* ROUTINE TO CONVERT 8 Y/N TYPE OPTIONS ON SCREEN TO 1 BYTE IN BINARY           
* NOTE: R4 ADDRESSES 8-BYTE SCREEN FIELD WITH OPTIONS                           
*       R6 ADDRESSES BYTE TO HAVE BINARY WRITTEN TO                             
*********************************************************************           
PUTPROF  NTR1                                                                   
         LA    R1,8                     8 BITS                                  
         ZIC   R3,=X'80'                FIRST BIT HEX VALUE                     
PUTP10   CLI   0(R2),C'N'                                                       
         BE    PUTP20                                                           
         STC   R3,HALF                                                          
         OC    0(1,R6),HALF             BIT VALUE INTO RECORD                   
PUTP20   SRL   R3,1                     NEXT BIT VALUE                          
         LA    R2,1(R2)                 NEXT BIT ON SCREEN                      
         BCT   R1,PUTP10                                                        
         XIT1                                                                   
********************************************************************            
MOVE2    MVI   WORK,C' '                                                        
         MVC   WORK+1(L'WORK-1),WORK                                            
         SR    R1,R1                                                            
         IC    R1,5(R2)                                                         
         LTR   R1,R1                                                            
         BCR   8,RE                EXIT ON ZERO LENGTH                          
         BCTR  R1,R0                                                            
         EX    R1,VARMOVE2                                                      
         BR    RE                                                               
         SPACE 2                                                                
VARMOVE2 MVC   WORK(0),8(R2)                                                    
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO DISPLAY FIRST LINE OF CONTRACT COMMENT                             
*                                                                               
* INPUT P1: A(COMMENT CODE)                                                     
*                                                                               
*       P2: BYTE 0:   LENGHT OF OUTPUT FIELD                                    
*           BYTE 1-3: A(OUTPUT FIELD)                                           
*                                                                               
***********************************************************************         
DISPFC   NMOD1 0,*DISPFC*                                                       
         L     RC,0(R1)                                                         
         L     RA,4(R1)                                                         
         L     R2,8(R1)            COMMENT CODE                                 
         L     R3,12(R1)           L, OUTPUT FIELD                              
*                                                                               
         XC    KEY,KEY                                                          
         LA    R6,KEY                                                           
         USING RCMTD,R6                                                         
         MVI   RCMTKTYP,X'2E'      RECORD TYPE                                  
         MVC   RCMTKREP,AGENCY     REP CODE                                     
         MVC   RCMTKOFF,=X'FFFF'   OFFICE CODE                                  
         MVC   RCMTKCDE,0(R2)      COMMENT CODE                                 
         OC    RCMTKCDE,SPACES     BLANK PADDED                                 
         DROP  R6                                                               
         GOTO1 HIGH                                                             
         CLC   KEY(L'RCMTKEY),KEYSAVE                                           
         BNE   DISPFCX                                                          
*                                                                               
         MVC   AIO,AIO3                                                         
         GOTO1 GETREC                                                           
         L     R6,AIO                                                           
         MVI   ELCODE,X'02'                                                     
         BAS   RE,GETEL6                                                        
         BNE   DISPFC50                                                         
*                                                                               
         USING RCMTELM2,R6         COMMENT TEXT ELEMENT                         
*                                                                               
DISPFC10 DS    0H                                                               
         CLI   RCMT2LEN,3          GET FIRST NON-BLANK COMMT LINE               
         BH    DISPFC20                                                         
         CLI   RCMT2TXT,C' '                                                    
         BNE   DISPFC20                                                         
         BAS   RE,NEXTEL6          R4 HAS ADDRESS OF FIRST ELEMENT              
         BE    DISPFC10                                                         
         B     DISPFC50                                                         
*                                                                               
DISPFC20 DS    0H                                                               
         CLM   R3,8,RCMT2LEN       COMMT FIELD HAS THIS MUCH ROOM               
         BH    DISPFC30                                                         
         LR    R1,R3               BYTE 0 HAS LENGTH                            
         SRL   R1,24               SHIFT LENGHT TO BYTE 3                       
         B     DISPFC45                                                         
DISPFC30 ZIC   R1,RCMT2LEN                                                      
DISPFC45 SH    R1,=H'3'                                                         
         EX    R1,*+8                                                           
         B     DISPFC50                                                         
         MVC   0(0,R3),RCMT2TXT                                                 
         DROP  R6                                                               
*                                                                               
DISPFC50 EQU   *                                                                
         MVC   AIO,AIO1                                                         
*                                                                               
DISPFCX  DS    0H                                                               
         XMOD1                                                                  
         EJECT                                                                  
*                                                                               
GETEL6   AH    R6,DATADISP                                                      
FIRSTEL6 CLI   0(R6),0                                                          
         BNE   *+10                                                             
         CLI   0(R6),1                                                          
         BR    RE                                                               
         CLI   ELCODE,0                                                         
         BCR   8,RE                                                             
         CLC   ELCODE,0(R6)                                                     
         BCR   8,RE                                                             
NEXTEL6  SR    RF,RF                                                            
         IC    RF,1(R6)                                                         
         LTR   RF,RF                                                            
         BNZ   *+6                                                              
         DC    H'0'                                                             
         AR    R6,RF                                                            
         B     FIRSTEL6                                                         
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        SUBROUTINE TO EDIT STATION TRAFFIC SYSTEM CODE                         
***********************************************************************         
EDITTRAF NMOD1 0,*EDITTR*                                                       
*                                                                               
         LA    R1,EDTFLIST                                                      
EDTF10   EQU   *                                                                
         CLI   0(R1),0             END OF LIST?                                 
         BE    EDTFBAD                                                          
         CLC   8(1,R2),0(R1)                                                    
         BE    EDTFGOOD                                                         
         LA    R1,1(R1)                                                         
         B     EDTF10                                                           
*                                                                               
*        EDITTRAF EXIT                                                          
*                                                                               
EDTFGOOD EQU   *                                                                
         LA    R0,0                                                             
EDTFEXIT EQU   *                                                                
         LTR   R0,R0                                                            
         XIT1                                                                   
EDTFBAD  EQU   *                                                                
         LA    R0,1                                                             
         B     EDTFEXIT                                                         
*                                                                               
EDTFLIST DC    C'B'          BIAS                                               
         DC    C'C'          COLUMBINE                                          
         DC    C'G'          GRAPHNET                                           
         DC    C'J'          JDS                                                
         DC    C'K'          KAMAN                                              
         DC    C'M'          MARKETRON                                          
         DC    C'P'          WPVI                                               
         DC    C'R'          REP                                                
         DC    C'W'          BIAS COPY DOWN FORMAT                              
         DC    C'V'          VCI:  COLUMBINE PAPER FORMAT                       
         DC    C'A'          RADIO                                              
         DC    X'00'                                                            
         EJECT                                                                  
********************************************************************            
*   SUBROUTINE TO VALIDATE/DISPLAY A MARKET RECORD (X'2B RECORD)                
*   ZERO IS FOUND                                                               
*   NON-ZERO IS NOT FOUND                                                       
********************************************************************            
DISPMKT  NMOD1 0,*DISPMKT*                                                      
         L     RC,0(R1)                                                         
         L     RA,4(R1)                                                         
*                                                                               
         LA    R2,FMSMCDH                                                       
         MVC   AIO,AIO2                                                         
*                                                                               
         XC    KEY,KEY                                                          
         LA    R6,KEY                                                           
         USING RMKTREC,R6                                                       
         MVI   RMKTKEY,X'2B'                                                    
         MVC   RMKTKREP(2),AGENCY                                               
         OC    FMSMCD,FMSMCD                                                    
         MVC   RMKTKMKT(4),FMSMCD                                               
         GOTO1 HIGH                                                             
         CLC   KEY(27),KEYSAVE                                                  
         BNE   DMKTBAD                                                          
*                                                                               
         GOTO1 GETREC                                                           
         L     R6,AIO                                                           
         ZIC   RF,0(R2)                                                         
         AR    R2,RF                                                            
         MVC   8(20,R2),RMKTNAME                                                
*                                                                               
DMKTGOOD EQU   *                                                                
         SR    R0,R0                                                            
DMKTEXIT EQU   *                                                                
         MVC   AIO,AIO1                                                         
         LTR   R0,R0                                                            
         XIT1  REGS=(R2)                                                        
DMKTBAD  EQU   *                                                                
         LA    R0,1                                                             
         B     DMKTEXIT                                                         
         SPACE 2                                                                
         DROP  R6                                                               
         EJECT                                                                  
         SPACE 1                                                                
*******************************************************************             
*        SUBROUTINE TO EXTRACT 2 BYTE ID                                        
*******************************************************************             
GETID    NMOD1 0,*GETID*                                                        
         L     RC,0(R1)                                                         
*                                                                               
         L     R4,AIO                                                           
         USING STATD,R4                                                         
*                                                                               
         L     R6,AIO2                                                          
         XC    0(25,R6),0(R6)      BUILD CONTROL FILE KEY                       
         MVI   0(R6),C'I'                                                       
         MVC   15(10,R6),SPACES                                                 
         MVC   15(8,R6),WORK                                                    
         GOTO1 DATAMGR,DMCB,=C'DMREAD',=C'CTFILE',(R6),(R6),0                   
         CLI   DMCB+8,0                                                         
         BNE   NOID                                                             
         LA    R6,28(R6)                                                        
         SR    R5,R5                                                            
*                                                                               
TESTELE  CLI   0(R6),0                                                          
         BE    NOID                                                             
         CLI   0(R6),2                                                          
         BNE   NEXTELE                                                          
         MVC   HALF,2(R6)          ID FOUND                                     
         XIT1                                                                   
*                                                                               
NEXTELE  IC    R5,1(R6)                                                         
         AR    R6,R5                                                            
         B     TESTELE                                                          
*                                                                               
NOID     MVC   HALF,=H'43'       USE DDS ID AS DEFAULT FOR REGIONAL HQ          
         CLI   RSTAKEY,2                                                        
         BNE   NOID5                                                            
         MVC   HALF,=H'0'        FOR STATION RECORD, USE 0, NO DEFAULT          
NOID5    XIT1                                                                   
         EJECT                                                                  
         DROP  R4                                                               
*****************************************************************               
NEXTUF   NMOD1 0,*NEXTUF*                                                       
*                                                                               
         L     R2,0(R1)                                                         
NEXTUF1  EQU   *                                                                
         SR    R0,R0                                                            
         IC    R0,0(R2)                                                         
         AR    R2,R0                                                            
         CLI   0(R2),0                                                          
         BE    NEXTEX                                                           
         CLI   0(R2),9                                                          
         BE    NEXTUF2                                                          
         TM    1(R2),X'20'         TEST PROTECTED                               
         BO    NEXTUF1                                                          
NEXTUF2  CLI   9(R2),0             CHECK FOR LAST                               
         BNE   NEXTUF4                                                          
         CR    R2,R2               IF LAST, SET CC=                             
         B     NEXTEX                                                           
NEXTUF4  LTR   R2,R2               NOT LAST, SET CC NOT=                        
*                                                                               
NEXTEX   XIT1  REGS=(R2)                                                        
         EJECT                                                                  
         SPACE 2                                                                
***********************************************************************         
STATD    DSECT                                                                  
       ++INCLUDE REGENSTA                                                       
       ++INCLUDE REGENGRP                                                       
       ++INCLUDE REGENOWN                                                       
       ++INCLUDE REGENMKT                                                       
RCMTD    DSECT                                                                  
       ++INCLUDE REGENCMT                                                       
       ++INCLUDE CTGENFILE                                                      
       ++INCLUDE FATWA                                                          
       ++INCLUDE DDCOMFACS                                                      
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE REBASWORKD                                                     
*                                                                               
GEND     DSECT                                                                  
         ORG   LISTAR                                                           
LISTSTA  DS    CL6                                                              
         DS    CL3                                                              
LISTMKT  DS    CL20                                                             
         DS    CL1                                                              
LISTGRP  DS    CL2                                                              
         DS    CL3                                                              
LISTAFF  DS    CL3                                                              
         DS    CL1                                                              
LISTJDT  DS    CL8                                                              
         DS    CL2                                                              
LISTLDT  DS    CL8                                                              
         DS    CL2                                                              
**********************************************************************          
SYSD     DSECT                                                                  
         ORG   SYSSPARE                                                         
GRAPHLAG DC    XL2'FFFF'           GRAPH FLAG                                   
*                                  X'FFFF' = NO 'X05' ELEMENT                   
*                                  X'0000' - X'05' ELT W/ NO RECVNG ID          
*                                  OTHER:    GRAPHNET/ACE                       
ZEROS    DC    30C'0'                                                           
COMBAREA DS    CL70                SAVE AREA FOR OLD X'0A' ELEMENTS             
*                                  4 X 14 CHARS. LAST ENTRY = DELIM             
*                                  AS OF 11/09/93, ELT = 10 CHARS               
ADDRLIST DS    A                   =A(AFFLIST)                                  
ATVBLST  DS    A                   =A(TVBLST)                                   
         EJECT                                                                  
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'073REBAS02   05/27/03'                                      
         END                                                                    
