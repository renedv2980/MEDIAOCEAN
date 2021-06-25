*          DATA SET REDAR50    AT LEVEL 006 AS OF 05/27/04                      
*          DATA SET REDAR50    AT LEVEL 114 AS OF 05/31/94                      
*PHASE T80F50C                                                                  
*                                                                               
         TITLE 'T80F50 - REDAR50 - DARE ORDER HISTORY LIST'                     
***********************************************************************         
*                                                                     *         
*  REDAR50 (T80F50) --- DARE ORDER HISTORY LIST                       *         
*                                                                     *         
* ------------------------------------------------------------------- *         
* UPDATE HISTORY:                                                     *         
*                                                                     *         
* 27MAY04 (HQ ) ADDS XML COMMENTS ON HIST SCREEN                      *         
* 18APR02 (SKU) TEMPORARY BYPASS 903 ERRORS                           *         
* 04JAN02 (SKU) CHANGE APPROVE TO OPEN                                *         
* 31OCT01 (SKU) INITIAL RELEASE                                       *         
*                                                                     *         
***********************************************************************         
T80F50   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,*T80F50*,R7,RR=R3                                              
         L     RC,0(R1)            STANDARD CODING                              
         USING GEND,RC                                                          
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA      BASE SCREEN + OUR SCREEN                     
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         LA    R5,SYSSPARE         R5 = A(OVERLAY STORAGE AREA)                 
         USING MYAREAD,R5                                                       
         ST    R3,RELO                                                          
                                                                                
         MVI   MYSCRNUM,X'E0'                                                   
         MVC   HISLAST+1(2),=X'0101' RETRANSMIT ENTIRE SCREEN                   
         OI    HISHDLNH+6,X'40'    FORCE CURSOR HERE                            
                                                                                
         BAS   RE,SETPFKYS         SETUP THE PFKEYS                             
                                                                                
         CLI   MODE,VALKEY         VALIDATE KEY?                                
         BE    VK                                                               
         CLI   MODE,VALREC         VALIDATE RECORD?                             
         BE    VR                                                               
         EJECT                                                                  
***********************************************************************         
* EXITS                                                                         
***********************************************************************         
EXITOK   CR    RB,RB                                                            
         B     EXIT                                                             
EXITL    CLI   *,X'FF'                                                          
         B     EXIT                                                             
EXITH    CLI   *,0                                                              
         B     EXIT                                                             
EXIT     DS    0H                                                               
         XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
* VALIDATE KEY                                                                  
***********************************************************************         
VK       DS    0H                                                               
         CLI   CALLSP,0            MUST BE CALLED TO GET HERE                   
         BNE   VK10                                                             
         LA    R2,CONRECH                                                       
         B     INVLRCAC            INVALID REC/ACTION                           
*                                                                               
VK10     DS    0H                  VALIDATE REVISION FILTER                     
         LA    R2,HISRNUMH                                                      
         TM    4(R2),X'20'                                                      
         BO    VKX                                                              
         OI    4(R2),X'20'                                                      
         MVI   CURRPAGE,0          RESET CURRENT PAGE                           
         MVI   REVNUM,X'FF'        DEFAULT ALL                                  
         CLI   5(R2),0                                                          
         BE    VKX                                                              
         CLI   HISRNUM,C'O'        ORIGINAL                                     
         BNE   VK20                                                             
         MVI   REVNUM,0                                                         
         B     VKX                                                              
*                                                                               
VK20     DS    0H                                                               
         TM    4(R2),X'08'         VALID NUMERIC?                               
         BZ    INVLFLD             INVALID FILTER                               
         ZIC   R4,5(R2)                                                         
         BCTR  R4,0                                                             
         EX    R4,*+8                                                           
         B     *+10                                                             
         PACK  DUB,8(0,R2)                                                      
         CVB   R4,DUB                                                           
         STC   R4,REVNUM                                                        
*                                                                               
VKX      DS    0H                                                               
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* VALIDATE RECORD                                                               
***********************************************************************         
VR       DS    0H                                                               
         XC    SVHSAC,SVHSAC                                                    
         GOTO1 HEXOUT,DMCB,SELECTKY+RDARKORD-RDARKEY,HISAORD,4                  
*                                                                               
         TWAXC HISLISTH,HISENDLH,PROT=Y                                         
                                                                                
         LA    R6,KEY                                                           
         USING RDARKEY,R6                                                       
         MVC   KEY(27),SELECTKY                                                 
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         DROP  R6                                                               
*                                                                               
         XC    SVDARFL2,SVDARFL2                                                
         CLC   KEY(L'RDARKEY),KEYSAVE                                           
         BNE   VR05                                                             
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
         L     R6,AIO                                                           
         MVI   ELCODE,X'0F'                                                     
         BRAS  RE,GETEL                                                         
         BNE   VR05                                                             
         MVC   SVDARFL2,RDARFLG2-RDARFLEM(R6)                                   
*                                                                               
VR05     DS    0H                                                               
         LA    R6,KEY                                                           
         USING RDARKEY,R6                                                       
         MVC   KEY(RDARKRT-RDARKEY),SELECTKY                                    
         MVI   RDARKRT,X'70'       HISTORY RECORD ONLY                          
         DROP  R6                                                               
*                                                                               
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
*                                                                               
         CLC   KEY(L'RDARKEY),KEYSAVE                                           
         BE    VR10                                                             
         MVC   HISLIST(35),=C'NO HISTORY AVAILABLE FOR THIS ORDER'              
         B     VRX                                                              
*                                                                               
VR10     DS    0H                                                               
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
*                                                                               
         CLI   TWAOFFC,C'*'        DDS-ONLY                                     
         BNE   VR13                                                             
         GOTO1 HEXOUT,DMCB,KEY+28,HISLIST+L'HISLIST-8,4,0                       
*                                                                               
VR13     DS    0H                                                               
         SR    R3,R3                                                            
         SR    R4,R4                                                            
         L     R6,AIO                                                           
         USING RDARHSEM,R6                                                      
         MVI   ELCODE,X'50'                                                     
         BRAS  RE,GETEL                                                         
         BNE   VRX                                                              
         CLI   CURRPAGE,0                                                       
         BE    VR13AA                                                           
*                                                                               
VR13A    DS    0H                  GET TOTAL NUMBER OF HISTORY ELEMENTS         
         ZIC   RE,CURRPAGE                                                      
         MHI   RE,15               NUMBER OF ITEMS PER SCREEN                   
         CR    R4,RE                                                            
         BNE   VR13AA                                                           
         LR    R3,R6               SAVE OFF NEXT STARTING POINT                 
*                                                                               
VR13AA   DS    0H                                                               
         CLI   REVNUM,X'FF'                                                     
         BE    VR13B                                                            
         CLC   REVNUM,RDARHSVR     CHECK IF ANY FILTER USED                     
         BNE   VR13C                                                            
*                                                                               
VR13B    DS    0H                                                               
         CLI   RDARHSAC,C'D'       DON'T COUNT DELNOTS                          
         BNE   VR13BA                                                           
         CLC   SVHSAC,RDARHSAC     IF PREV ELE IS NOT DELNOT                    
         BNE   VR13C                                                            
VR13BA   DS    0H                                                               
         AHI   R4,1                                                             
*                                                                               
VR13C    DS    0H                                                               
         MVC   SVHSAC,RDARHSAC     SAVE OFF THE TYPE                            
         BRAS  RE,NEXTEL                                                        
         BE    VR13A                                                            
         STC   R4,TOTHISEL                                                      
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,X'50'                                                     
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LTR   R3,R3                                                            
         BZ    VR14A                                                            
         LR    R6,R3                                                            
*                                                                               
VR14A    DS    0H                                                               
         XC    PREVDATE,PREVDATE                                                
         LA    R2,HISLIST                                                       
         USING LHISD,R2                                                         
*                                                                               
VR14     DS    0H                                                               
         CLI   REVNUM,X'FF'        CHECK IF WE ARE FILTERING                    
         BE    VR15A               ON REVISION NUMBER                           
         CLC   REVNUM,RDARHSVR                                                  
         BNE   VR40                                                             
*                                                                               
VR15A    DS    0H                                                               
         CLI   RDARHSER,X'FF'                                                   
         BE    VR15                                                             
*                                                                               
*** TEMPORARY CODE TO SKIP BOGUS 903 ERRORS (4/18/02)                           
         CLC   =AL2(903),RDARHSER                                               
         BE    VR40                                                             
*                                                                               
         MVC   LHISACT(5),=C'Error'                                             
         EDIT  RDARHSER,(3,LHISERR),ALIGN=LEFT                                  
         B     VR40                                                             
*                                                                               
VR15     DS    0H                                                               
         EDIT  RDARHSVR,(3,LHISVER),ALIGN=LEFT                                  
*                                                                               
         CLC   PREVDATE,RDARHSDT                                                
         BE    VR18                SKIP IF SAME DATE                            
         GOTO1 DATCON,DMCB,(2,RDARHSDT),(5,LHISDATE)                            
         MVC   PREVDATE,RDARHSDT                                                
*                                                                               
VR18     DS    0H                                                               
         GOTO1 HEXOUT,DMCB,RDARHSTM,WORK,2                                      
         MVC   LHISTIME(2),WORK                                                 
         MVI   LHISTIME+2,C':'                                                  
         MVC   LHISTIME+3(2),WORK+2                                             
*                                                                               
         LA    R4,STATLIST                                                      
*                                                                               
VR20     DS    0H                                                               
         CLC   RDARHSAC,0(R4)                                                   
         BE    VR30                                                             
         AHI   R4,L'STATLIST                                                    
         CLI   0(R4),X'FF'                                                      
         BNE   VR20                                                             
         MVC   LHISACT(7),=C'Unknown'                                           
         B     VR33                                                             
*                                                                               
VR30     DS    0H                                                               
         MVC   LHISACT,1(R4)                                                    
*                                                                               
VR33     DS    0H                                                               
         CLI   RDARHSAC,C'O'                                                    
         BNE   VR35                                                             
         CLI   RDARHSVR,0                                                       
         BE    VR34                                                             
         MVC   LHISACT,=CL9'Revision'                                           
         B     VR40                                                             
VR34     DS    0H                                                               
         TM    SVDARFL2,SF2XML     XML ORDER?                                   
         BZ    VR35                                                             
         MVC   LHISDLNT(9),=C'XML Order'                                        
*                                                                               
VR35     DS    0H                                                               
         CLI   RDARHSAC,C'D'       DELIVERY NOTICE?                             
         BNE   VR40                                                             
*                                                                               
VR38     DS    0H                                                               
         MVC   LHISDLNT,=C'Delivered'                                           
         GOTO1 HEXOUT,DMCB,RDARHSTM,WORK,2                                      
         MVC   LHISDTIM(2),WORK                                                 
         MVI   LHISDTIM+2,C':'                                                  
         MVC   LHISDTIM+3(2),WORK+2                                             
*                                                                               
VR40     DS    0H                                                               
         BRAS  RE,NEXTEL                                                        
         BNE   VRX                                                              
         CLI   REVNUM,X'FF'        CHECK IF WE ARE FILTERING                    
         BE    VR50                ON REVISION NUMBER                           
         CLC   REVNUM,RDARHSVR                                                  
         BNE   VR40                                                             
*                                                                               
VR50     DS    0H                                                               
         CLI   RDARHSAC,C'D'       DELIVERY NOTICE?                             
         BNE   VR55                                                             
         CLI   LHISDLNT,C'D'                                                    
         BNE   VR38                                                             
*                                                                               
VR55     DS    0H                                                               
         AHI   R2,HISENDL-HISENDC                                               
         LA    RF,HISENDLH                                                      
         CR    R2,RF                                                            
         BH    VR60                                                             
         CLI   RDARHSAC,C'D'       DELIVERY NOTICE?                             
         BE    VR38                                                             
         B     VR14                                                             
*                                                                               
VR60     DS    0H                                                               
         ZIC   RE,CURRPAGE                                                      
         AHI   RE,1                                                             
         STC   RE,CURRPAGE                                                      
         B     NEXTPAGE                                                         
*                                                                               
VRX      DS    0H                                                               
         MVI   CURRPAGE,0                                                       
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* SET THE PFKEY INFORMATION                                                     
***********************************************************************         
SETPFKYS NTR1                                                                   
         SR    R2,R2               NO PFKEY AT TABLE FIRST                      
***************                                                                 
* FOR ACTION LIST                                                               
***************                                                                 
STPFKL00 CLI   ACTNUM,ACTLIST      ACTION LIST?                                 
         BNE   EXIT                                                             
*                                                                               
         CLI   PFKEY,0             ENTER KEY IS OKAY                            
         BE    STPFKL10                                                         
*                                                                               
STPFKL10 LA    R2,LPFTABLE         YES, USE LIST PFKEY TABLE                    
*                                                                               
STPFINIT GOTO1 INITIAL,DMCB,(R2)   INITIALIZE THE PFKEYS                        
*                                                                               
STPFX    B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* BUMP TO NEXT FIELD                                                            
***********************************************************************         
BUMPNEXT DS    0H                                                               
         ZIC   RF,0(R2)                                                         
         AR    R2,RF                                                            
         BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
* LIST PFKEY TABLE DEFINITIONS                                                  
***********************************************************************         
LPFTABLE DS    0C                                                               
*                                                                               
* RETURN TO SELECT SCREEN                                                       
         DC    AL1(LPF12X-*,12,0,0,(LPF12X-LPF12)/KEYLNQ,0)                     
         DC    CL3' ',CL8'ORDER',CL8'SELECT '                                   
LPF12    DC    AL1(KEYTYTWA,L'HISHDLN-1),AL2(HISHDLN-T80FFFD)                   
LPF12X   EQU   *                                                                
*                                                                               
         DC    X'FF'                                                            
*                                                                               
* ERROR MESSAGES                                                                
*                                                                               
MISSFLD  MVC   RERROR,=AL2(1)                                                   
         B     ERREND                                                           
*                                                                               
INVLFLD  MVC   RERROR,=AL2(2)                                                   
         B     ERREND                                                           
*                                                                               
INVLRCAC MVC   RERROR,=AL2(INVRCACT)                                            
         B     ERREND                                                           
*                                                                               
RECNTFND MVI   GERROR1,53          RECORD NOT FOUND                             
         B     ERREND                                                           
*                                                                               
INVPFERR MVI   GERROR1,ERINVPFK    INVALID PFKEY                                
         B     ERREND                                                           
*                                                                               
NEXTREQ  MVC   RERROR,=AL2(3)      ENTER NEXT REQUEST                           
         B     INFEND                                                           
*                                                                               
NEXTPAGE MVC   RERROR,=AL2(111)    HIT ENTER FOR NEXT SCREEN                    
         B     INFEND                                                           
*                                                                               
HITBOTTM MVC   RERROR,=AL2(110)    BUY DISPLAYED                                
         B     INFEND                                                           
*                                                                               
ERREND   DS    0H                                                               
         MVI   RMSGTYPE,C'E'                                                    
         GOTO1 MYERROR                                                          
*                                                                               
INFEND   DS    0H                                                               
         LA    R2,HISHDLNH                                                      
         MVI   RMSGTYPE,C'I'                                                    
         GOTO1 MYERROR             DO A GETTXT CALL                             
*                                                                               
         GETEL R6,DATADISP,ELCODE  USED FOR THE GETEL OPERATIONS                
*                                                                               
STATLIST DS    0CL10                                                            
         DC    C'O',CL9'New Order'                                              
         DC    C'S',CL9'Resent'                                                 
         DC    C'C',CL9'Recalled'                                               
         DC    C'R',CL9'Rejected'                                               
         DC    C'A',CL9'Opened'                                                 
         DC    C'F',CL9'Confirmed'                                              
         DC    C'D',CL9'Delivered'                                              
         DC    X'FF'                                                            
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE DDSPOOLD          (GENERAL PRINT AREAS)                        
       ++INCLUDE DDSPLWORKD        (GENERAL CONTROLLER AREAS)                   
       ++INCLUDE FATIOB                                                         
       ++INCLUDE REDARFFD                                                       
       ++INCLUDE DDGENTWA                                                       
         ORG   CONTAGH                                                          
       ++INCLUDE REDARE0D                                                       
       ++INCLUDE REDARWORKD                                                     
       ++INCLUDE REDARDSECT                                                     
         PRINT ON                                                               
*                                                                               
* APPLICATION STORAGE AREA                                                      
*                                                                               
MYAREAD  DSECT                                                                  
RELO     DS    A                                                                
DISPFLAG DS    X                   DISPLAY STATUS                               
FIRSTPG  EQU   X'02'               DISPLAY FIRST PAGE                           
NEXTPG   EQU   X'04'               DISPLAY NEXT PAGE                            
PREVDATE DS    XL2                                                              
TOTHISEL DS    X                   TOTAL NUMBER OF HISTORY ELEMENTS             
CURRPAGE DS    X                                                                
REVNUM   DS    X                                                                
SVHSAC   DS    C                   SAVED AUDIT TRAIL TYPE                       
SVDARFL2 DS    X                   =RDARFL2                                     
SF2XML   EQU   X'80'               XML ORDER                                    
                                                                                
*                                                                               
WORKLQ   EQU   *-MYAREAD                                                        
*                                                                               
* BUY LIST LINE                                                                 
*                                                                               
LHISD    DSECT                                                                  
LHISACT  DS    CL9                                                              
         DS    CL2                                                              
LHISVER  DS    CL3                                                              
         DS    CL2                                                              
LHISDATE DS    CL8                                                              
         DS    CL2                                                              
LHISTIME DS    CL5                                                              
         DS    CL2                                                              
LHISERR  DS    CL3                                                              
         ORG   LHISERR                                                          
LHISDLNT DS    CL9                                                              
         DS    CL2                                                              
LHISDTIM DS    CL5                                                              
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'006REDAR50   05/27/04'                                      
         END                                                                    
