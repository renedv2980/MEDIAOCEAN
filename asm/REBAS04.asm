*          DATA SET REBAS04    AT LEVEL 152 AS OF 04/03/97                      
*PHASE T82404A,*                                                                
*                                                                               
***********************************************************************         
*                                                                     *         
*  TITLE: T82404 - REP FILE STATION COMPETITIVE RECORD                *         
*                                                                     *         
*  CALLED FROM: REP CONTROLLER (T82400)                               *         
*                                                                     *         
*  INPUTS: SCREENS REBASB3  (T824B3) -- MAINTENANCE                   *         
*          SCREENS SPSFMD3  (T824D3) -- LIST                          *         
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
         TITLE 'T82404 STATION COMPETITIVE MAINTENANCE'                         
T82404   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,*T82404*,R7,RR=R3                                              
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
TEAMNFND EQU   119                 TEAM NOT FOUND                               
INVOFF   EQU   151                 INVALID OFFICE                               
*                                                                               
         CLI   MODE,VALKEY         VALIDATE KEY                                 
         BNE   MAIN10                                                           
         GOTO1 =A(VK),DMCB,(RC),(RA),RR=RELO                                    
         BNZ   TRAPERR                                                          
         B     EXIT                                                             
MAIN10   EQU   *                                                                
         CLI   MODE,VALREC                                                      
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
*                                                                               
TRAPERR  GOTO1 MYERROR                                                          
*                                                                               
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
**********************************************************************          
*                  VALIDATE KEY ROUTINE                              *          
**********************************************************************          
VK       NMOD1 0,*VALKEY*                                                       
         L     RC,0(R1)                                                         
         L     RA,4(R1)            TWA                                          
*                                                                               
         CLI   ACTNUM,ACTLIST      ONLINE LIST RECORDS                          
         BE    VKGOOD                                                           
*                                                                               
         LA    R2,OTMSTAH                                                       
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
*                                                                               
         EJECT                                                                  
STATV    CLC   22(0,R5),=C'TV'                                                  
STAAM    CLC   22(0,R5),=C'AM'                                                  
STAFM    CLC   22(0,R5),=C'FM'                                                  
STACM    CLC   22(0,R5),=C'CM'                                                  
STALP    CLC   22(0,R5),=C'L'                                                   
STA1     CLC   22(0,R5),=C'1'                                                   
STA2     CLC   22(0,R5),=C'2'                                                   
*                                                                               
         EJECT                                                                  
         DROP  R4                                                               
         LTORG                                                                  
         EJECT                                                                  
**********************************************************************          
*                  DISPLAY RECORD                                    *          
**********************************************************************          
DR       NMOD1 0,*DISREC*                                                       
         L     RC,0(R1)                                                         
         L     RA,4(R1)                                                         
*                                                                               
         LA    R2,OTMOF1H          SET A(1ST OFFICE FIELD HEADER)               
         LA    R3,OTMTM1H          SET A(1ST TEAM   FIELD HEADER)               
         LA    R5,OTMOFNH          SET A(1ST OFFICE NAME FIELD)                 
         LA    R4,OTMTMNH          SET A(1ST TEAM   NAME FIELD)                 
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,X'04'                                                     
         BAS   RE,GETEL                                                         
         B     DR15                                                             
*                                                                               
DR10     BAS   RE,NEXTEL                                                        
*                                                                               
DR15     EQU   *                                                                
         BNE   DR40                                                             
*                                                                               
         ST    R6,DUB              SAVE R6 TEMPORARILY                          
*                                                                               
         USING RSTAOTEL,R6                                                      
         MVC   8(2,R2),RSTAOTOF    INSERT OFFICE                                
         MVC   8(2,R3),RSTAOTTM    INSERT TEAM                                  
         DROP  R6                                                               
*                                                                               
         FOUT  (R2)                                                             
         FOUT  (R3)                                                             
         XC    KEY,KEY             RETRIEVE OFFICE NAME                         
         MVI   KEY,4                                                            
         MVC   KEY+23(2),AGENCY                                                 
         MVC   KEY+25(2),8(R2)     OFFICE CODE FROM SCREEN                      
         GOTO1 HIGH                                                             
         CLC   KEYSAVE(27),KEY                                                  
         BNE   DR20                NOT FOUND - NO NAME???                       
         MVC   AIO,AIO2                                                         
         GOTO1 GETREC                                                           
*                                                                               
         L     R6,AIO2                                                          
         USING ROFFREC,R6                                                       
         MVC   8(20,R5),ROFFNAME   INSERT OFFICE NAME                           
         FOUT  (R5)                                                             
         DROP  R6                                                               
*                                                                               
DR20     EQU   *                                                                
         XC    KEY,KEY             RETRIEVE TEAM NAME                           
         MVI   KEY,5                                                            
         MVC   KEY+23(2),AGENCY                                                 
         MVC   KEY+25(2),8(R3)                                                  
         GOTO1 HIGH                                                             
         CLC   KEYSAVE(27),KEY                                                  
         BNE   DR30                NOT FOUND?  NO NAME??                        
         GOTO1 GETREC                                                           
*                                                                               
         L     R6,AIO2                                                          
         USING RTEMREC,R6                                                       
         MVC   8(10,R4),RTEMDVNM   INSERT DIVISION NAME                         
         MVC   19(10,R4),RTEMNAME  INSERT TEAM     NAME                         
         FOUT  (R4)                                                             
         DROP  R6                                                               
*                                                                               
DR30     EQU   *                                                                
         LA    R2,NXTOTFLD(R2)     BUMP TO NEXT OFFICE FIELD                    
         LA    R3,NXTOTFLD(R3)     BUMP TO NEXT TEAM   FIELD                    
         LA    R5,NXTOTFLD(R5)     BUMP TO NEXT OFFICE NAME                     
         LA    R4,NXTOTFLD(R4)     BUMP TO NEXT TEAM   NAME                     
         L     R6,DUB              RESTORE R6                                   
         B     DR10                                                             
*                                                                               
NXTOTFLD EQU   OTMOF2H-OTMOF1H                                                  
*                                                                               
DR40     EQU   *                                                                
         LA    RF,OTMLSTH          CLEAR TO END OF SCREEN                       
DR50     EQU   *                                                                
         CR    R2,RF               END OF SCREEN REACHED?                       
         BNL   DRGOOD              YES                                          
         ZIC   RE,0(R2)            GET FIELD LENGTH: OFFICE                     
         SH    RE,=H'9'            SUBTRACT LENGTH+1 FOR EX                     
         EX    RE,MVCSPAC2         MOVE SPACE TO FIELD                          
         FOUT  (R2)                                                             
         ZIC   RE,0(R3)            GET FIELD LENGTH: TEAM                       
         SH    RE,=H'9'            SUBTRACT LENGTH+1 FOR EX                     
         EX    RE,MVCSPAC3         MOVE SPACE TO FIELD                          
         FOUT  (R3)                                                             
         ZIC   RE,0(R5)            GET FIELD LENGTH: OFFICE                     
         SH    RE,=H'9'            SUBTRACT LENGTH+1 FOR EX                     
         EX    RE,MVCSPAC5         MOVE SPACE TO FIELD                          
         FOUT  (R5)                                                             
         ZIC   RE,0(R4)            GET FIELD LENGTH: TEAM                       
         SH    RE,=H'9'            SUBTRACT LENGTH+1 FOR EX                     
         EX    RE,MVCSPAC7         MOVE SPACE TO FIELD                          
         FOUT  (R4)                                                             
         LA    R2,NXTOTFLD(R2)     BUMP TO NEXT OFFICE FIELD                    
         LA    R3,NXTOTFLD(R3)     BUMP TO NEXT TEAM   FIELD                    
         LA    R5,NXTOTFLD(R5)     BUMP TO NEXT OFFICE NAME                     
         LA    R4,NXTOTFLD(R4)     BUMP TO NEXT TEAM   NAME                     
         B     DR50                GO BACK FOR NEXT                             
*                                                                               
MVCSPAC2 MVC   8(0,R2),SPACES      CLEAR OFFICE                                 
MVCSPAC3 MVC   8(0,R3),SPACES      CLEAR TEAM                                   
MVCSPAC5 MVC   8(0,R5),SPACES      CLEAR OFFICE NAME                            
MVCSPAC7 MVC   8(0,R4),SPACES      CLEAR TEAM   NAME                            
*                                                                               
DRGOOD   DS    0H                                                               
         MVC   AIO,AIO1                                                         
         SR    R0,R0                                                            
         B     DREX                                                             
DRBAD    LA    R0,1                                                             
DREX     EQU   *                                                                
         LTR   R0,R0               SETS THE CONDITION CODE                      
         XIT1                                                                   
         EJECT                                                                  
**********************************************************************          
         GETEL R6,DATADISP,ELCODE                                               
**********************************************************************          
         LTORG                                                                  
         EJECT                                                                  
**********************************************************************          
*                  VALIDATE RECORD                                   *          
**********************************************************************          
VR       NMOD1 0,*VALREC*                                                       
         L     RC,0(R1)                                                         
         L     RA,4(R1)            TWA                                          
*                                                                               
         OI    GENSTAT2,RETEQSEL                                                
         MVC   MYKEY(L'MYKEY),KEY  SAVE THE KEY                                 
*                                                                               
         MVI   ELCODE,X'04'        DELETE ALL                                   
         GOTO1 REMELEM                                                          
*                                  DELETE ALL OFF/TEAM ELEMENTS                 
         LA    R2,OTMOF1H          SET A(1ST OFFICE FIELD HEADER)               
         LA    R3,OTMTM1H          SET A(1ST TEAM   FIELD HEADER)               
         LA    R5,ELEM             SET A(WORK AREA FOR ELEMENT)                 
         USING RSTAOTEL,R5                                                      
*                                                                               
         XC    ELEM,ELEM           CLEAR IT OUT                                 
         MVI   RSTAOTCO,4          INITIALIZE ELEMENT CODE                      
         MVI   RSTAOTLN,11            AND LENGTH                                
*                                                                               
VR0020   EQU   *                                                                
         LA    RF,OTMLSTH          SET A(END OF SCREEN)                         
         CR    R2,RF               END OF SCREEN REACHED?                       
         BNL   VRGOOD              YES - FINISHED                               
         CLI   5(R2),0             ANY OFFICE ENTERED?                          
         BNZ   VR0040              YES -                                        
         CLI   5(R3),0             NO  - TEAM ENTERED?                          
         BZ    VR0120              NO  - CHECK NEXT FIELDS                      
         B     VRERR1              YES - IT IS ERROR                            
*                                                                               
VR0040   EQU   *                   TEST OFFICE ON FILE                          
         XC    KEY,KEY                                                          
         MVI   KEY,4                                                            
         MVC   KEY+23(2),AGENCY                                                 
         MVC   KEY+25(2),8(R2)     OFFICE CODE FROM SCREEN                      
         GOTO1 HIGH                                                             
         CLC   KEYSAVE(27),KEY                                                  
         BE    VR0060              FOUND                                        
         B     VRERR2                                                           
*                                                                               
VR0060   EQU   *                                                                
         MVC   RSTAOTOF,8(R2)      MOVE IN OFFICE                               
*                                                                               
         CLI   5(R3),0             ANY TEAM ENTERED?                            
         BZ    VRERR1              NO  - ERROR                                  
*                                                                               
*  VERIFY TEAM ON FILE                                                          
         MVI   KEY,5                                                            
         MVC   KEY+25(2),8(R3)                                                  
         GOTO1 HIGH                                                             
         CLC   KEYSAVE(27),KEY                                                  
         BNE   VRERR1              TEAM RECORD FOUND                            
*                                                                               
         MVC   RSTAOTTM,8(R3)      MOVE IN TEAM                                 
*                                                                               
         DROP  R5                                                               
         GOTO1 ADDELEM                                                          
*                                                                               
VR0120 EQU     *                                                                
         LA    R2,NXTOTFLD(R2)     BUMP TO NEXT OFFICE FIELD                    
         LA    R3,NXTOTFLD(R3)     BUMP TO NEXT TEAM   FIELD                    
         B     VR0020              GO BACK FOR NEXT FIELD                       
*                                                                               
VRGOOD   EQU   *                                                                
         GOTO1 =A(DR),DMCB,(RC),(RA),RR=RELO                                    
         MVC   KEY(L'MYKEY),MYKEY                                               
         GOTO1 HIGH                AVOID PUTREC DRAMA                           
         MVC   AIO,AIO2                                                         
         GOTO1 GETREC                                                           
         MVC   AIO,AIO1                                                         
*                                                                               
         SR    R0,R0                                                            
         B     VREX                                                             
*                                                                               
VRBAD    LA    R0,1                                                             
*                                                                               
VREX     EQU   *                                                                
         LTR   R0,R0               SET THE CONDITION CODE                       
         XIT1  REGS=(R2)           IN CASE OF ERROR MESSAGE                     
         EJECT                                                                  
*                                                                               
VRERR1   EQU   *                                                                
         LR    R2,R3                                                            
         MVC   RERROR,=AL2(TEAMNFND)                                            
         B     VRBAD                                                            
*                                                                               
VRERR2   EQU   *                                                                
         MVC   RERROR,=AL2(INVOFF)                                              
         B     VRBAD                                                            
*                                                                               
         EJECT                                                                  
*                                                                               
         LTORG                                                                  
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
         LA    R2,LOTSTATH                                                      
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
*                                                                               
LR25     EQU   *                                                                
         MVC   LISTMKT(L'RSTAMKT),RSTAMKT                                       
         DROP  R4                                                               
         GOTO1 LISTMON                                                          
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
         MVC   OTMSTA(L'OTMSTA),SPACES                                          
         MVC   OTMSTA(4),RSTAKSTA                                               
         CLI   RSTAKSTA+4,C' '                                                  
         BE    DKGOOD                                                           
         CLI   OTMSTA+3,C' '                                                    
         BNE   DK10                                                             
         MVI   OTMSTA+3,C'-'                                                    
         MVC   OTMSTA+4(1),RSTAKSTA+4                                           
         B     *+14                                                             
DK10     EQU   *                                                                
         MVI   OTMSTA+4,C'-'                                                    
         MVC   OTMSTA+5(1),RSTAKSTA+4                                           
*                                                                               
DKGOOD   EQU   *                                                                
         OI    OTMSTAH+6,X'80'                                                  
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
*****************************************************************               
*                                                                               
RELO     DS    A                                                                
MYKEY    DS    CL27                                                             
         EJECT                                                                  
*                                                                               
       ++INCLUDE DDFLDIND          EQUATED INDICATORS FOR FOUT                  
       ++INCLUDE REBASINC                                                       
       ++INCLUDE REBASTWA                                                       
         SPACE 2                                                                
*                                                                               
         ORG   CONTAGH                                                          
       ++INCLUDE REBASB3D          MAINTENANCE SCREEN                           
         ORG   CONTAGH                                                          
       ++INCLUDE REBASD3D          LIST SCREEN                                  
         EJECT                                                                  
*                                                                               
STATD    DSECT                                                                  
       ++INCLUDE REGENSTA                                                       
       ++INCLUDE REGENTEM                                                       
       ++INCLUDE REGENOFF                                                       
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
         DS    CL2                                                              
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'152REBAS04   04/03/97'                                      
         END                                                                    
