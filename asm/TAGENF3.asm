*          DATA SET TAGENF3    AT LEVEL 075 AS OF 10/07/14                      
*******20140515:043940: NEW MEMBER ADDED BY GHOA FOR PROJ# TICKETNUMBER         
*******        :043940: Canadian Taxes - Gen + Reports                          
*PHASE T702F3C,*                                                                
         TITLE 'T702F3 - T4,RL1 MAINTENANCE'                                    
T702F3   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T702F3,R7                                                      
         L     RC,0(R1)            RC=GENCON STORAGE AREA                       
         USING GEND,RC                                                          
         L     RA,ATWA             RA=A(TWA)                                    
         USING T702FFD,RA                                                       
         L     R9,ASYSD            R9=ROOT STORAGE AREA                         
         USING SYSD,R9                                                          
         L     R8,ASPOOLD          R8=SPOOL DSECT                               
         USING SPOOLD,R8                                                        
         LA    R6,TWAHOLE          R7=TWAHOLE                                   
         USING TWAD,R6                                                          
         EJECT                                                                  
*              MODE CONTROLLED ROUTINES                                         
*                                                                               
         CLI   ACTNUM,ACTLIST                                                   
         BE    MD_100                                                           
*                                                                               
         GOTO1 INITIAL,DMCB,PFTABLE                                             
         CLI   MODE,SETFILE                                                     
         BNE   MD_10                                                            
         BAS   RE,SETCHK           SET CHECK FILE                               
         B     MDXIT                                                            
*                                                                               
MD_10    CLI   MODE,VALKEY         VALIDATE KEY                                 
         BE    VK                                                               
         CLI   THISLSEL,C'D'       IF DELETING FROM LIST                        
         BE    MD_30                                                            
         CLI   MODE,DISPREC        IF MODE IS DISPLAY                           
         BE    MD_70                                                            
*                                                                               
MD_30    CLI   MODE,DISPKEY        DISPLAY KEY                                  
         BE    DK                                                               
         CLI   MODE,RECDEL         DELETE RECORD                                
         BE    MD_40                                                            
         CLI   MODE,RECREST        RESTORE RECORD                               
         BE    MD_90                                                            
         CLI   MODE,XRECDEL        RECORD DELETED                               
         BE    MD_60                                                            
         CLI   MODE,XRECREST       RECORD RESTORED                              
         BE    MD_60                                                            
         CLI   MODE,XRECPUT        OR RECORD CHANGED                            
         BE    MD_60                                                            
         CLI   MODE,XRECADD        OR NEW RECORD ADDED                          
         BNE   MD_80                                                            
         MVC   CONACT,=CL8'CHANGE'                                              
         OI    CONACTH+6,X'80'                                                  
         B     MD_60                                                            
*                                                                               
MD_40    BAS   RE,DELREC                                                        
         B     MD_90                                                            
*                                                                               
MD_60    GOTO1 ADDPTRS,DMCB,PTRBLK HANDLE PASSIVE POINTERS                      
*                                                                               
MD_70    BAS   RE,DISPLAY          (RE-)DISPLAY THE RECORD                      
*                                                                               
MD_80    CLI   MODE,VALREC         VALIDATE THE RECORD                          
         BE    MD_85                                                            
         CLI   MODE,RECADD         ADDING THE RECORD                            
         BNE   MDXIT                                                            
MD_85    BAS   RE,BLDREC                                                        
         B     MDXIT                                                            
*                                                                               
MD_90    XC    PTRBLK,PTRBLK           CLEAR POINTER BLOCK                      
         GOTO1 SAVPTRS,DMCB,PTRBLK     HANDLE PASSIVE POINTERS                  
         B     MDXIT                                                            
*                                                                               
MD_100   DS    0H                                                               
         GOTO1 INITIAL,DMCB,0                                                   
         CLI   MODE,SETFILE        SET FILE FOR PAGING                          
         BNE   MD_215                                                           
         BAS   RE,SETCHK                                                        
         B     XIT                                                              
*                                                                               
MD_215   CLI   MODE,VALKEY         FIRST TIME IN                                
         BE    LVK                                                              
*                                                                               
         CLI   MODE,LISTRECS                                                    
         BNE   MD_230                                                           
         LA    R2,LISTAR                                                        
         B     LR                                                               
*                                                                               
MD_230   CLI   MODE,PRINTREP                                                    
         BNE   MDXIT                                                            
         XC    TIKEY,TIKEY         ENSURE START REPORT FROM BEG OF LIST         
         ZAP   COUNTER,=P'0'       CLEAR LINE COUNTER                           
         LA    R2,MYSPECS                                                       
         ST    R2,SPECS                                                         
         LA    R2,P                                                             
         B     LR                                                               
*                                                                               
MDXIT    B     XIT                                                              
         EJECT                                                                  
*---------------------------------------------------------------------*         
*              VALIDATE KEY                                           *         
*                 : SINCE T4, RL-1, T4A-NR AND NR4 KEYS ARE THE SAME, *         
*                 : THIS VK ROUTINE DOESN'T NEED TO CHANGE FOR EACH.  *         
*---------------------------------------------------------------------*         
VK       MVI   TGCUR,C'C'          SET CANADIAN CURRENCY                        
         GOTO1 FLDVAL,DMCB,(X'40',ST4SSNH),(X'80',ST4PRVH)                      
         BE    *+8                                                              
         MVI   PFAID,0                                                          
         TM    TRNSTAT,RACHANG     IF RECORD/ACTION CHANGED                     
         BZ    VK05                                                             
         NI    ST4SSNH+4,X'DF'     FORCE VALIDATION                             
         MVC   ST4EMPN,SPACES      CLEAR NAMES                                  
         OI    ST4EMPNH+6,X'80'                                                 
         MVC   ST4SSNN,SPACES                                                   
         OI    ST4SSNNH+6,X'80'                                                 
*                                                                               
VK05     LA    R2,ST4SSNH                                                       
         TM    4(R2),X'20'         WAS FIELD ALREADY VALIDATED                  
         BO    VK10                                                             
         NI    ST4EMPH+4,X'DF'     FORCE VALIDATION                             
         CLI   5(R2),9                                                          
         BNE   VK06                                                             
         MVC   WORK(9),=9X'F0'     CHECK FOR VALID NUMERIC                      
         MVZ   WORK(9),8(R2)                                                    
         CLC   WORK(9),=9X'F0'                                                  
         BNE   INVERR                                                           
         B     VK09                                                             
                                                                                
VK06     CLI   ACTNUM,ACTADD       IF ADDING NEW W4 RECORD,                     
******** BE    INVERR                                                           
                                                                                
         CLI   5(R2),6             INVALID ENTRIES DUE TO X'40' PARAM           
         BNE   INVERR                                                           
         MVC   TGPID,8(R2)                                                      
         LA    RE,6                ENSURE ALL PID CHARACTERS                    
         LA    RF,8(R2)                                                         
VK07     CLI   0(RF),C'A'          ARE VALID ALPHANUMERIC                       
         BL    INVERR                                                           
         CLI   0(RF),C'9'                                                       
         BH    INVERR                                                           
         CLI   0(RF),C'Z'                                                       
         BNH   VK08                                                             
         CLI   0(RF),C'0'                                                       
         BL    INVERR                                                           
VK08     LA    RF,1(RF)                                                         
         BCT   RE,VK07                                                          
                                                                                
         GOTO1 SSNUNPK,DMCB,TGPID,TGSSN                                         
         BNE   VK09                                                             
         MVC   ST4SSN,TGSSN                                                     
         MVI   ST4SSNH+5,9                                                      
                                                                                
VK09     BAS   RE,SETTAL           SET TALENT FILE                              
         GOTO1 RECVAL,DMCB,TLW4CDQ,(X'08',ST4SSNH),ST4SSNNH                     
         GOTO1 SSNPACK,DMCB,TGSSN,TGPID                                         
         CLI   ACTNUM,ACTADD       IF ADDING, DON'T CONVERT SSN                 
         BE    VK10                TO PID ON SCREEN                             
         MVC   ST4SSN,SPACES                                                    
         MVC   ST4SSN(L'TGPID),TGPID                                            
         MVI   ST4SSNH+5,6                                                      
         OI    ST4SSNH+6,X'80'                                                  
*                                                                               
VK10     OI    4(R2),X'20'                                                      
         LA    R2,ST4EMPH                                                       
         TM    4(R2),X'20'         WAS FIELD ALREADY VALIDATED                  
         BO    VK30                                                             
         NI    ST4YEARH+4,X'DF'    FORCE VALIDATION                             
         CLI   5(R2),0             IF NO INPUT                                  
         BNE   VK20                                                             
         MVC   ST4EMP,=C'P+ '      DEFAULT TO P+ EMPLOYER                       
         MVI   ST4EMPH+5,3                                                      
         OI    ST4EMPH+6,X'80'                                                  
*                                                                               
VK20     MVC   AIO,AIO2                                                         
         GOTO1 RECVAL,DMCB,TLEMCDQ,(X'08',ST4EMPH),ST4EMPNH                     
*                                                                               
VK30     OI    4(R2),X'20'                                                      
         LA    R2,ST4YEARH                                                      
         TM    4(R2),X'20'         WAS FIELD ALREADY VALIDATED                  
         BO    VK60                                                             
         NI    ST4PRVH+4,X'DF'     FORCE VALIDATION                             
         CLI   5(R2),0             DEFAULT THIS YEAR - 1                        
         BNE   VK40                                                             
         GOTO1 ADDAY,DMCB,(C'Y',TGTODAY0),WORK,F'-1'                            
         GOTO1 DATCON,DMCB,(0,WORK),(20,WORK+6)                                 
         MVC   ST4YEAR,WORK+6      SET CCYY                                     
         OI    ST4YEARH+6,X'80'    TRANSMIT                                     
         B     VK50                                                             
*                                                                               
VK40     MVC   WORK(4),=4X'F0'     INSURE VALID NUMERIC                         
         MVZ   WORK(4),8(R2)                                                    
         CLC   WORK(4),=4X'F0'                                                  
         BNE   INVERR                                                           
*                                                                               
VK50     MVC   TGYEAR,ST4YEAR      SET GLOBAL YEAR FIELD                        
*                                                                               
VK60     OI    4(R2),X'20'                                                      
         LA    R2,ST4PRVH          PROVINCE                                     
         TM    4(R2),X'20'         WAS FIELD ALREADY VALIDATED                  
         BO    VK70                                                             
         CLI   5(R2),0                                                          
         BNE   VK65                                                             
         MVC   8(2,R2),=C'CN'                                                   
         CLI   RECNUM,RL1                                                       
         BNE   VK63                                                             
         MVC   8(2,R2),=C'QC'                                                   
VK63     OI    ST4PRVH+6,X'80'     TRANSMIT                                     
         B     VK70                                                             
*                                                                               
VK65     CLI   RECNUM,RL1          RL-1 RECORD?                                 
         BNE   VK68                                                             
         CLC   ST4PRV,=C'QC'       YES, QUEBEC IS ONLY VALID                    
         BNE   INVERR                                                           
                                                                                
VK68     CLC   ST4PRV,=C'CN'                                                    
         BE    VK70                                                             
         MVC   WORK(2),ST4PRV                                                   
         MVI   WORK+2,C' '                                                      
         MVC   TGCTRY,=C'CA'       HAS TO BE CANADIAN                           
         GOTO1 TAXVAL,DMCB,(X'FF',WORK)                                         
         BNE   INVERR                                                           
*                                                                               
VK70     OI    4(R2),X'20'                                                      
         MVI   ADDNEW,C'N'                                                      
         BAS   RE,SETCHK           SET CHECK FILE                               
*                                                                               
         MVC   AIO,AIO1            SET IO AREA FOR GETREC                       
*                                                                               
         BAS   RE,SETRECCD         SET RECORD & SUB-RECORD CODE                 
         BNE   VKX                                                              
         GOTO1 RECVAL,DMCB,,(X'B4',0)                                           
         BNE   VK80                                                             
         CLI   ACTNUM,ACTADD       IF ADD & REC FOUND                           
         BNE   VK75                                                             
         MVI   ACTNUM,ACTCHA       MAKE ACTION CHANGE                           
         MVC   CONACT,=CL8'CHANGE'                                              
         OI    CONACTH+6,X'80'                                                  
                                                                                
VK75     B     VKX                                                              
                                                                                
VK80     BAS   RE,SETRECCD         SET RECORD & SUB-RECORD CODE                 
         BNE   VKX                                                              
         GOTO1 RECVAL,DMCB,,(X'C0',0)                                           
*                                                                               
VKX      B     XIT                                                              
         EJECT                                                                  
*---------------------------------------------------------------------*         
*              DISPLAY KEY                                            *         
*                 : SINCE T4, RL-1, T4A-NR AND NR4 KEYS ARE THE SAME, *         
*                 : THIS DK ROUTINE DOESN'T NEED TO CHANGE FOR EACH.  *         
*---------------------------------------------------------------------*         
DK       MVC   SVKEY,KEY           SAVE KEY                                     
         L     R4,AIO              LOCATION OF THE RECORD                       
         USING TLT4D,R4                                                         
         MVC   TGYEAR,TLT4YEAR     YEAR                                         
         MVI   TGCUR,C'C'          SET CANADIAN CURRENCY                        
         MVC   TGEMP,TLT4EMP       EMPLOYER                                     
         MVC   TGSSN,TLT4SSN       SIN NUMBER                                   
         MVC   ST4SSN,TLT4SSN      SIN NUMBER                                   
         OI    ST4SSNH+6,X'80'                                                  
         MVC   ST4EMP,TLT4EMP      EMPLOYER                                     
         OI    ST4EMPH+6,X'80'                                                  
         MVC   ST4YEAR,TLT4YEAR    YEAR                                         
         OI    ST4YEARH+6,X'80'                                                 
         MVC   ST4PRV,=C'CN'                                                    
         CLI   RECNUM,RL1                                                       
         BNE   *+10                                                             
         MVC   ST4PRV,=C'QC'                                                    
         OI    ST4PRVH+6,X'80'                                                  
*                                                                               
         MVI   IOOPT,C'N'          CLEAR I/O FLAG                               
         MVI   ADDNEW,C'N'                                                      
         LA    R4,KEY              CHECK IF THIS RECORD WAS CHANGED             
         USING TLT4D,R4                                                         
*                                                                               
DK10     MVC   AIO,AIO2                                                         
         BAS   RE,SETTAL           SET TALENT FILE                              
         GOTO1 RECVAL,DMCB,TLW4CDQ,(X'0C',ST4SSNH),ST4SSNNH                     
         GOTO1 RECVAL,DMCB,TLEMCDQ,(X'0C',ST4EMPH),ST4EMPNH                     
*                                                                               
         BAS   RE,SETCHK           SET CHECK FILE                               
         MVC   KEY,SVKEY           RESTORE KEY                                  
         MVC   AIO,AIO1                                                         
         B     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
*---------------------------------------------------------------------*         
*              DISPLAY THE RECORD                                     *         
*---------------------------------------------------------------------*         
         SPACE 1                                                                
DISPLAY  NTR1                                                                   
         CLI   RECNUM,T4           T4                                           
         BNE   DISP200                                                          
         BAS   RE,DRT4                                                          
         B     XIT                                                              
*                                                                               
DISP200  CLI   RECNUM,RL1          RL-1                                         
         BNE   DISP400                                                          
         BAS   RE,DRR1                                                          
         B     XIT                                                              
*                                                                               
DISP400  CLI   RECNUM,NR           T4A-NR                                       
         BNE   DISP600                                                          
         BAS   RE,DRNR                                                          
         B     XIT                                                              
*                                                                               
DISP600  CLI   RECNUM,N4           NR4                                          
         BNE   XIT                                                              
         BAS   RE,DRN4                                                          
         B     XIT                                                              
         EJECT                                                                  
*---------------------------------------------------------------------*         
*              BUILD THE RECORD                                       *         
*---------------------------------------------------------------------*         
BLDREC   NTR1                                                                   
         CLI   RECNUM,T4           T4                                           
         BNE   BREC200                                                          
         BAS   RE,BRT4                                                          
         B     XIT                                                              
*                                                                               
BREC200  CLI   RECNUM,RL1          RL-1                                         
         BNE   BREC400                                                          
         BAS   RE,BRR1                                                          
         B     XIT                                                              
*                                                                               
BREC400  CLI   RECNUM,NR           T4A-NR                                       
         BNE   BREC600                                                          
         BAS   RE,BRNR                                                          
         B     XIT                                                              
*                                                                               
BREC600  CLI   RECNUM,N4           NR4                                          
         BNE   XIT                                                              
         BAS   RE,BRN4                                                          
         B     XIT                                                              
         EJECT                                                                  
*---------------------------------------------------------------------*         
*              DELETE THE RECORD                                      *         
*---------------------------------------------------------------------*         
DELREC   NTR1                                                                   
         MVI   ELCODE,TAT4ELQ                                                   
         CLI   RECNUM,T4                                                        
         BE    *+8                                                              
         MVI   ELCODE,TAR1ELQ                                                   
         GOTO1 REMELEM                                                          
                                                                                
         B     XIT                                                              
         EJECT                                                                  
*---------------------------------------------------------------------*         
*        ROUTINE TO SET RECORD & SUB-RECORD CODE FOR RECVAL CALL                
*              CLOBBERS DMCB(4)                                                 
*                       DMCB+8(4)                                               
*---------------------------------------------------------------------*         
SETRECCD NTR1                                                                   
         XC    DMCB+8(4),DMCB                                                   
         LA    RE,TLPMCDQ       X'24' = SPECIAL RECORDS                         
         ST    RE,DMCB                                                          
                                                                                
         MVI   DMCB+8,TLT4SCDQ  T4 SUB-RECORD                                   
         CLI   RECNUM,T4                                                        
         JE    XIT                                                              
         MVI   DMCB+8,TLR1SCDQ  RL1 SUB-RECORD                                  
         CLI   RECNUM,RL1                                                       
         JE    XIT                                                              
         MVI   DMCB+8,TLTASCDQ  T4A-NR SUB-RECORD                               
         CLI   RECNUM,NR                                                        
         JE    XIT                                                              
         MVI   DMCB+8,TLN4SCDQ  NR4 SUB-RECORD                                  
         CLI   RECNUM,N4                                                        
         JNE   XIT                                                              
         J     XIT                                                              
                                                                                
         EJECT                                                                  
*---------------------------------------------------------------------*         
*        ROUTINE GET AMOUNT FROM FIELD (R2) POINTS AT                           
*---------------------------------------------------------------------*         
GETCASH  NTR1                                                                   
         ZIC   R3,5(R2)                                                         
         LTR   R3,R3                                                            
         JZ    NO                                                               
         GOTO1 CASHVAL,DMCB,8(R2),(R3)                                          
         CLI   0(R1),X'FF'                                                      
         JE    INVERR                                                           
         MVC   FULL,4(R1)                                                       
         J     YES                                                              
                                                                                
         EJECT                                                                  
*---------------------------------------------------------------------*         
*        ROUTINE TO TURN OFF PRINT BITS                                         
*---------------------------------------------------------------------*         
CHPRINT  NTR1                                                                   
         BAS   RE,SECURITY         CHECK SECURITY ACCESS OF USER                
         JNE   SECERR                                                           
         MVC   SVEMP,TGEMP         SAVE EMPLOYER                                
         MVC   SVCUR,TGCUR              CURRENCY                                
         MVC   SVKEY,KEY           SAVE W4 KEY                                  
         BAS   RE,BLDEMP           BLD TABLE OF EMPLOYERS                       
         XC    KEY,KEY                                                          
         MVC   AIO,AIO2                                                         
         MVI   TGCUR,C'C'          SET CAN$ CURRENCY                            
*                                                                               
CHP10    LA    R3,EMPTAB                                                        
*                                                                               
CHP20    OC    0(3,R3),0(R3)       ANY EMPLOYERS LEFT                           
         JZ    CHP30                                                            
         MVC   TGEMP,0(R3)         SET EMPLOYER                                 
         BAS   RE,UPDREC           UPDATE RECORD                                
         LA    R3,3(R3)                                                         
         J     CHP20               GET NEXT EMPLOYER                            
*                                                                               
CHP30    CLI   TGCUR,C'C'          IF ALREADY PROCESSED CANADIAN $              
         JE    CHP40               EXIT                                         
         MVI   TGCUR,C'C'          ELSE SET CANADIAN CURRENCY                   
         J     CHP10               AND DO ALL EMPLOYERS OVER                    
*                                                                               
CHP40    XC    KEY,KEY             REGET RECORD INTO AIO2 TO PREVENT            
         MVC   KEY(L'SVKEY),SVKEY                                               
         GOTO1 HIGH                GETREC/PUTREC SYNDROME                       
         GOTO1 GETREC                                                           
*                                                                               
CHPX     MVC   AIO,AIO1            RE-SET I/O AREA                              
         MVC   TGEMP,SVEMP         RESTORE EMPLOYER                             
         MVC   TGCUR,SVCUR                 CURRENCY                             
         J     XIT                                                              
         EJECT                                                                  
*                                                                               
* VALIDATE THE KEY (LISTS)                                                      
*                                                                               
LVK      DS    0H                                                               
         MVI   TIFCUR,C'C'         CANADIAN CURRENCY                            
         TM    CONRECH+4,X'20'     RECORD ALREADY VALIDATED                     
         BO    LVK03                                                            
         NI    STLOPTSH+4,X'FF'-X'20'                                           
*                                                                               
LVK03    LA    R2,STLYEARH         YEAR FILTER                                  
         TM    4(R2),X'20'         ALREADY VALIDATED                            
         BO    LVK10                                                            
         CLI   5(R2),0                                                          
         BNE   LVK05                                                            
         GOTO1 ADDAY,DMCB,(C'Y',TGTODAY0),WORK,-1                               
         MVC   TIFYEAR(2),WORK                                                  
         GOTO1 DATCON,DMCB,(0,WORK),(20,WORK+6)                                 
         MVC   STLYEAR,WORK+6      CCYY                                         
         OI    STLYEARH+6,X'80'                                                 
                                                                                
         GOTO1 DATCON,DMCB,(0,WORK),(1,WORK+6)                                  
         MVC   TIQPSTR,WORK+6                                                   
         MVC   TIQPEND,WORK+6                                                   
         MVC   TIQPSTR+1(2),=X'0101'                                            
         MVC   TIQPEND+1(2),=X'1231'                                            
         B     LVK10                                                            
*                                                                               
LVK05    CLI   5(R2),4             INPUT MUST BE 4 LONG                         
         BNE   INVERR                                                           
         MVC   WORK(4),=4X'F0'     INSURE VALID NUMERIC                         
         MVZ   WORK(4),8(R2)                                                    
         CLC   WORK(4),=4X'F0'                                                  
         BNE   INVERR                                                           
         MVC   WORK(4),STLYEAR                                                  
         MVC   WORK+4(4),=C'0101'                                               
         GOTO1 DATCON,DMCB,(9,WORK),(1,WORK+16)                                 
         MVC   TIQPSTR,WORK+16                                                  
         MVC   TIQPEND,WORK+16                                                  
         MVC   TIQPEND+1(2),=X'1231'                                            
         GOTO1 DATCON,DMCB,(9,WORK),(0,WORK+16)                                 
         MVC   TIFYEAR(2),WORK+16                                               
*                                                                               
LVK10    OI    4(R2),X'20'                                                      
                                                                                
         BAS   RE,SETTAL           SET TALENT FILE                              
         LA    R2,STLEMPH          EMP FILTER                                   
         TM    4(R2),X'20'         ALREADY VALIDATED                            
         BO    LVK15                                                            
         MVC   STLEMPN,SPACES                                                   
         OI    STLEMPNH+6,X'80'                                                 
         CLI   5(R2),0                                                          
         BNE   LVK13                                                            
         MVC   STLEMP,=C'P+ '      DEFAULT TO P+ EMPLOYER                       
         MVI   STLEMPH+5,3                                                      
         OI    STLEMPH+6,X'80'                                                  
*                                                                               
LVK13    BAS   RE,SETTAL           SET TALENT FILE                              
         GOTO1 RECVAL,DMCB,TLEMCDQ,(X'28',(R2)),STLEMPNH                        
         BAS   RE,SETCHK           SET CHECK FILE                               
*                                                                               
LVK15    MVC   TIFEMP,TGEMP                                                     
         OI    4(R2),X'20'                                                      
         LA    R2,STLSTRH          START AT                                     
         TM    4(R2),X'20'         ALREADY VALIDATED                            
         BO    LVK20                                                            
         XC    TIQSTART,TIQSTART                                                
         CLI   5(R2),0                                                          
         BE    LVK20                                                            
         ZIC   R1,5(R5)                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   TIQSTART(R1),8(R2)                                               
*                                                                               
LVK20    OI    4(R2),X'20'                                                      
         LA    R2,STLOPTSH         OPTIONS                                      
         TM    4(R2),X'20'         PREVIOUSLY VALIDATED                         
         BO    VKX                                                              
         MVI   SCROPT,0                                                         
         CLI   5(R2),0                                                          
         BE    LVK60                                                            
         LA    R3,BLOCK            SET FOR SCANNER                              
         USING SCAND,R3                                                         
         GOTO1 SCANNER,DMCB,(R2),(R3)                                           
         CLI   4(R1),0             INVALID INPUT                                
         BE    INVERR                                                           
         ZIC   R0,4(R1)                                                         
         CLC   =C'ALL',SCDATA1                                                  
         BNE   INVERR                                                           
         OI    SCROPT,SCRALL       SET OPTION BIT                               
*                                                                               
LVK40    DS    0H                                                               
*                                                                               
LVK50    LA    R3,SCANNEXT                                                      
         BCT   R0,LVK40                                                         
*                                                                               
LVK60    OI    4(R2),X'20'                                                      
         BAS   RE,INIT                                                          
*                                                                               
LVKX     B     XIT                                                              
         EJECT                                                                  
*                                                                               
*        SET UP SYSIO                                                           
*                                                                               
INIT     NTR1                                                                   
*                                                                               
         XC    SVSSN,SVSSN         CLEAR LAST SSN READ                          
         MVC   TIUSERID,TWAORIG    SET UP NECCESSARY DATA FROM                  
         MVC   TIQSTAFF,TGCTSTAF   GLOBAL STORAGE                               
         MVI   TIREAD,TLT4CDQ      LIST T4 RECORDS                              
         CLI   RECNUM,RL1                                                       
         BNE   *+8                                                              
         MVI   TIREAD,TLR1CDQ      LIST RL1 RECORDS                             
         MVI   TIRDSUBT,TLT4SCDQ   DEFAULT T4                                   
         CLI   RECNUM,T4                                                        
         BE    *+8                                                              
         MVI   TIRDSUBT,TLR1SCDQ                                                
*                                                                               
INITX    B     XIT                                                              
         EJECT                                                                  
*                                                                               
LR       LA    R0,LRHOOK           SET HOOK TO SYSIO                            
         ST    R0,TIHOOK                                                        
         MVC   TIACOMFC,ACOMFACS                                                
         MVC   TIKHOOK,SETLSTK     LIST KEY HOOK                                
         MVI   NLISTS,16           IN ORDER TO GET CONTROL BACK                 
         GOTO1 TASYSIO,DMCB,TASYSIOD                                            
         MVI   NLISTS,15              AFTER 1 FULL PAGE                         
         CLI   MODE,PRINTREP                                                    
         BNE   LRX                                                              
         EDIT  COUNTER,(8,P),COMMAS=YES,ALIGN=LEFT                              
         LR    R1,R0                                                            
         LA    R1,P+1(R1)                                                       
         MVC   0(10,R1),=C'T4 RECORDS'                                          
         GOTO1 SPOOL,DMCB,(R8)                                                  
         TM    WHEN,X'40'          IF SPOOLING AND NOW                          
         BZ    LRX                                                              
         XC    CONSERV,CONSERV     AUTO $DQU                                    
         MVC   CONSERV(4),=C'$DQU'                                              
*                                                                               
LRX      B     XIT                                                              
         EJECT                                                                  
*                                                                               
*        PROCESS SYSIO RECORDS                                                  
*                                                                               
LRHOOK   NTR1                                                                   
         CLI   TIMODE,PROCREC      PROCESS RECORDS                              
         BE    PRREC                                                            
*                                                                               
LRHX     B     XIT                                                              
         EJECT                                                                  
*                                                                               
*        PROCESS RECORD                                                         
*                                                                               
PRREC    DS    0H                                                               
         TM    SCROPT,SCRALL       IF WE ONLY WANT TO SHOW THE LATEST           
         BO    PR02                                                             
         CLC   SVSSN,TISSN         W2 - COMPARE TO PREVIOUS SSN                 
         BE    PRRX                IF IT'S THE SAME - EXIT                      
*                                                                               
PR02     MVC   LISTAR,SPACES       CLEAR PREVIOUS LINE                          
         USING LISTD,R2                                                         
         L     R4,TIAREC                                                        
         USING TLT4D,R4                                                         
         MVC   W2SSN,TISSN                SS NUMBER                             
         MVC   TGSSN,TISSN                                                      
*                                                                               
PR05     MVC   AIO,AIO2            DON'T CREAM COMMERCIAL RECORD                
         BAS   RE,SETTAL           SET TALENT FILE                              
         MVI   SSNAMEH,40          SET LENGTH OF FAKE FIELD                     
         GOTO1 RECVAL,DMCB,TLW4CDQ,(X'A8',0),SSNAMEH                            
         MVC   W2SSNN,SSNAME                                                    
         BAS   RE,SETCHK           SET CHECK FILE                               
         MVC   AIO,AIO1            RESTORE AIO                                  
*                                                                               
         MVI   ELCODE,TAWSELQ      GET W2 SUBSIDARY ELEMENT                     
         USING TAWSD,R4                                                         
         BAS   RE,GETEL                                                         
         BNE   PR30                                                             
         OC    TAWSDPRT,TAWSDPRT   CHECK IF W2 WAS PRINTED                      
         BZ    PR30                                                             
         GOTO1 DATCON,DMCB,(1,TAWSDPRT),(8,W2PRINT)                             
*                                                                               
PR30     MVC   DMDSKADD,TIDSKADD   PASS DISK ADDRESS TO LISTMON                 
         CLI   MODE,PRINTREP                                                    
         BNE   PR40                                                             
         GOTO1 CATCHIOS            ENSURE DON'T DO TOO MANY I/O-S               
         GOTO1 SPOOL,DMCB,(R8)     PRINT REPORT                                 
         AP    COUNTER,=P'1'                                                    
         B     PRRX                                                             
*                                                                               
PR40     CLI   LISTNUM,15          IF END OF PAGE                               
         BNE   PR50                BUT THERE ARE MORE RECS                      
         MVC   MYMSGNO1,OKNO           PUT MSG - HIT ENTER FOR NEXT             
         MVI   MYMSYS,X'FF'                                                     
         OI    GENSTAT2,USGETTXT                                                
         LA    R2,STLSELH                                                       
         B     ERRXIT                                                           
*                                                                               
PR50     GOTO1 LISTMON             CALL LISTMON                                 
*                                                                               
PRRX     MVC   SVSSN,TISSN         SET LAST SSN READ                            
         B     XIT                                                              
         DROP  R4,R2                                                            
         EJECT                                                                  
*---------------------------------------------------------------------*         
*        ROUTINE TO CHECK IF USER ALLOWED TO STOP A W2 REPRINT                  
*---------------------------------------------------------------------*         
SECURITY NTR1                                                                   
         MVI   BYTE,0                                                           
         CLI   TGSTDSP,0          1ST SECURITY BYTE                             
         JNE   SEC10                                                            
         MVI   BYTE,BP234+BM      SET VALID USERS - PROGRAMMER/ADMIN            
         J     SEC40              SYSTEMS MANAGER/EXECUTIVE/MANAGER             
*                                                                               
SEC10    CLI   TGSTDSP,1          2ND SECURITY BYTE                             
         JNE   SEC20                                                            
         MVI   BYTE,BAB           SET VALID USERS - ACCOUNTING/                 
         J     SEC40              ACCOUNTING MANAGER                            
*                                                                               
SEC20    CLI   TGSTDSP,2          3RD SECURITY BYTE                             
         JNE   SEC30                                                            
         MVI   BYTE,BO            SET VALID USERS - OPERATOR                    
         J     SEC40                                                            
*                                                                               
SEC30    CLI   TGSTDSP,3          4TH SECURITY BYTE                             
         JNE   NO                                                               
*                                                                               
SEC40    MVC   TBYTE,BYTE                                                       
         OC    BYTE,TGSTBIT       IS THIS A VALID USER                          
         CLC   BYTE,TBYTE                                                       
         JNE   NO                                                               
         J     YES                                                              
                                                                                
*---------------------------------------------------------------------*         
*        ROUTINE TO BUILD AN EMPLOYER TABLE                                     
*---------------------------------------------------------------------*         
BLDEMP   NTR1                                                                   
         BAS   RE,SETTAL           SET TALENT FILE                              
         XC    KEY,KEY                                                          
         LA    R1,KEY                                                           
         USING TLEMD,R1                                                         
         XC    EMPTAB(30),EMPTAB                                                
         LA    R2,EMPTAB                                                        
         MVI   KEY,TLEMCDQ         SET EMPLOYER CODE                            
         GOTO1 HIGH                                                             
         J     BE20                                                             
*                                                                               
BE10     GOTO1 SEQ                                                              
*                                                                               
BE20     CLC   KEY(1),KEYSAVE      STILL READING EMPLOYER RECORDS               
         JNE   BEX                                                              
         MVC   0(3,R2),TLEMEMP                                                  
         LA    R2,3(R2)            BUMP TO NEXT PLACE IN TABLE                  
         J     BE10                                                             
*                                                                               
BEX      BAS   RE,SETCHK           SET CHECK FILE                               
         J     XIT                                                              
                                                                                
*---------------------------------------------------------------------*         
*        ROUTINE TO GET & UPDATE A RECORD                                       
*---------------------------------------------------------------------*         
UPDREC   NTR1                                                                   
         MVC   AIO,AIO2                                                         
         BAS   RE,SETRECCD          SET RECORD AND SUB-RECORD                   
         JNE   XIT                                                              
*                                                                               
UPD090   GOTO1 RECVAL,DMCB,,(X'B4',0)                                           
         JNE   URECX                                                            
         L     R4,AIO                                                           
         USING TLT4D,R4                                                         
         NI    TLT4STAT,X'BF'       TURN OFF RE-PRINT BIT                       
         CLI   PFAID,13                                                         
         JE    UPD100                                                           
         OI    TLT4STAT,X'40'       TURN ON RE-PRINT BIT                        
*                                                                               
UPD100   GOTO1 PUTREC                                                           
*                                                                               
         LA    R1,KEY                                                           
         USING TLDRD,R1                                                         
         NI    TLDRSTAT,X'BF'       TURN OFF RE-PRINT BIT                       
         CLI   PFAID,13                                                         
         JE    UPD200                                                           
         OI    TLDRSTAT,X'40'       TURN ON RE-PRINT BIT                        
*                                                                               
UPD200   GOTO1 WRITE                                                            
*                                                                               
URECX    MVC   AIO,AIO1                                                         
         J     XIT                                                              
         EJECT                                                                  
*---------------------------------------------------------------------*         
*        ROUTINE TO SET SYSFIL/DIR TO TALENT FILE                               
*---------------------------------------------------------------------*         
SETTAL   NTR1                                                                   
         XC    FILENAME,FILENAME                                                
         MVC   SYSFIL,=C'TALFIL'                                                
         MVC   SYSDIR,=C'TALDIR'                                                
         J     XIT                                                              
*                                                                               
*                                                                               
*---------------------------------------------------------------------*         
*        ROUTINE TO SET SYSFIL/DIR TO CHECK FILE                                
*---------------------------------------------------------------------*         
SETCHK   NTR1                                                                   
         XC    FILENAME,FILENAME                                                
         MVC   SYSFIL,=C'CHKFIL'                                                
         MVC   SYSDIR,=C'CHKDIR'                                                
         J     XIT                                                              
*                                                                               
         EJECT                                                                  
*---------------------------------------------------------------------*         
*        DISPLAY T4 RECORD                                                      
*---------------------------------------------------------------------*         
DRT4     NTR1                                                                   
         TWAXC ST4INCH             CLEAR UNPROTECTED FIELDS                     
*                                                                               
         L     R4,AIO                                                           
         MVI   ELCODE,TAT4ELQ      LOOK FOR T4 ELEM FOR PROVINCE                
         GOTO1 GETL,DMCB,(2,ST4PRV)                                             
         JNE   XIT                                                              
         L     R4,TGELEM                                                        
         USING TAT4D,R4                                                         
                                                                                
         EDIT  TAT4EARN,(11,ST4INC),2,FLOAT=-,ZERO=BLANK                        
         EDIT  TAT4TAX,(11,ST4TAX),2,FLOAT=-,ZERO=BLANK                         
                                                                                
         EDIT  TAT4EINE,(11,ST4EIE),2,FLOAT=-,ZERO=BLANK                        
         EDIT  TAT4PPER,(11,ST4PEN),2,FLOAT=-,ZERO=BLANK                        
                                                                                
         CLC   ST4PRV,=C'CN'                                                    
         JE    DRT4_600                                                         
*                                                                               
DRT4_200 LA    R2,ST4CPP                                                        
         CLC   ST4PRV,=C'QC'                                                    
         JNE   DRT4_400                                                         
         EDIT  TAT4QPPR,(11,ST4QPR),2,FLOAT=-,ZERO=BLANK                        
         EDIT  TAT4QPIE,(11,ST4QPE),2,FLOAT=-,ZERO=BLANK                        
         LA    R2,ST4QPP                                                        
DRT4_400 EDIT  TAT4CPPC,(11,(R2)),2,FLOAT=-,ZERO=BLANK                          
         EDIT  TAT4EPRM,(11,ST4EIP),2,FLOAT=-,ZERO=BLANK                        
*                                                                               
DRT4_600 OI    ST4INCH+6,X'80'     TRANSMIT                                     
         OI    ST4CPPH+6,X'80'                                                  
         OI    ST4QPPH+6,X'80'                                                  
         OI    ST4EIPH+6,X'80'                                                  
         OI    ST4QPRH+6,X'80'                                                  
         OI    ST4TAXH+6,X'80'                                                  
         OI    ST4PENH+6,X'80'                                                  
         OI    ST4EIEH+6,X'80'                                                  
         OI    ST4QPEH+6,X'80'                                                  
*                                                                               
         USING TAWSD,R4                                                         
         L     R4,AIO              GET W2 SUBSIDARY ELEMENT                     
         MVI   ELCODE,TAWSELQ                                                   
         BAS   RE,GETEL                                                         
         JNE   DRT4_700                                                         
*                                                                               
         LA    R2,ST4LPRTH         LAST DATE PRINTED                            
         OC    TAWSDPRT,TAWSDPRT                                                
         JZ    DRT4_700                                                         
         GOTO1 DATCON,DMCB,(1,TAWSDPRT),(5,8(R2))                               
         OI    6(R2),X'80'         TRANSMIT                                     
*                                                                               
DRT4_700 GOTO1 ACTVOUT,DMCB,ST4LCHGH         LAST CHANGED                       
*                                                                               
         CLI   PFAID,13                                                         
         JE    DRT4_800                                                         
         CLI   PFAID,14                                                         
         JNE   XIT                                                              
*                                                                               
DRT4_800 BAS   RE,CHPRINT          STOP/REPRINT PRINT OF W2 FORM                
         J     CHPMSG                                                           
                                                                                
         EJECT                                                                  
*---------------------------------------------------------------------*         
*        DISPLAY RL-1 RECORD                                                    
*---------------------------------------------------------------------*         
DRR1     NTR1                                                                   
         TWAXC SRLINCH             CLEAR UNPROTECTED FIELDS                     
*                                                                               
         L     R4,AIO                                                           
         MVI   ELCODE,TAR1ELQ      LOOK FOR R1 ELEM FOR PROVINCE                
         GOTO1 GETL,DMCB,(2,SRLPRV)                                             
         JNE   XIT                                                              
         L     R4,TGELEM                                                        
         USING TAR1D,R4                                                         
                                                                                
         EDIT  TAR1EARN,(11,SRLINC),2,FLOAT=-,ZERO=BLANK                        
         EDIT  TAR1QPPC,(11,SRLQPP),2,FLOAT=-,ZERO=BLANK                        
         EDIT  TAR1QEIP,(11,SRLEIP),2,FLOAT=-,ZERO=BLANK                        
         EDIT  TAR1TAX,(11,SRLTAX),2,FLOAT=-,ZERO=BLANK                         
         EDIT  TAR1QPPE,(11,SRLPEN),2,FLOAT=-,ZERO=BLANK                        
         EDIT  TAR1PIPP,(11,SRLQPR),2,FLOAT=-,ZERO=BLANK                        
         EDIT  TAR1PIPW,(11,SRLQPW),2,FLOAT=-,ZERO=BLANK                        
*                                                                               
         OI    SRLINCH+6,X'80'     TRANSMIT                                     
         OI    SRLQPPH+6,X'80'                                                  
         OI    SRLEIPH+6,X'80'                                                  
         OI    SRLTAXH+6,X'80'                                                  
         OI    SRLPENH+6,X'80'                                                  
         OI    SRLQPRH+6,X'80'                                                  
         OI    SRLQPWH+6,X'80'                                                  
*                                                                               
         USING TAWSD,R4                                                         
         L     R4,AIO              GET W2 SUBSIDARY ELEMENT                     
         MVI   ELCODE,TAWSELQ                                                   
         BAS   RE,GETEL                                                         
         JNE   DRR1_700                                                         
*                                                                               
         LA    R2,SRLLPRTH         LAST DATE PRINTED                            
         OC    TAWSDPRT,TAWSDPRT                                                
         JZ    DRR1_700                                                         
         GOTO1 DATCON,DMCB,(1,TAWSDPRT),(5,8(R2))                               
         OI    6(R2),X'80'         TRANSMIT                                     
*                                                                               
DRR1_700 GOTO1 ACTVOUT,DMCB,SRLLCHGH         LAST CHANGED                       
*                                                                               
         CLI   PFAID,13                                                         
         JE    DRR1_800                                                         
         CLI   PFAID,14                                                         
         JNE   XIT                                                              
*                                                                               
DRR1_800 BAS   RE,CHPRINT          STOP/REPRINT PRINT OF W2 FORM                
         J     CHPMSG                                                           
                                                                                
         EJECT                                                                  
*---------------------------------------------------------------------*         
*        DISPLAY T4A-NR RECORD                                                  
*---------------------------------------------------------------------*         
DRNR     NTR1                                                                   
         J     XIT                                                              
         EJECT                                                                  
*---------------------------------------------------------------------*         
*        DISPLAY NR4 RECORD                                                     
*---------------------------------------------------------------------*         
DRN4     NTR1                                                                   
         J     XIT                                                              
         EJECT                                                                  
*---------------------------------------------------------------------*         
*        BUILD T4 RECORD                                              *         
*---------------------------------------------------------------------*         
BRT4     NTR1                                                                   
         GOTO1 SAVPTRS,DMCB,PTRBLK                                              
         MVI   ELCODE,TAWSELQ                                                   
         GOTO1 REMELEM                                                          
*                                                                               
         L     R4,AIO                                                           
         MVI   ELCODE,TAT4ELQ                                                   
         GOTO1 GETL,DMCB,(2,ST4PRV)                                             
         JNE   BRT4_100                                                         
         L     R4,TGELEM                                                        
         USING TAT4D,R4                                                         
         MVI   0(R4),X'FF'         MARK AND DELETE IT                           
         MVI   ELCODE,X'FF'                                                     
         GOTO1 REMELEM                                                          
*                                                                               
BRT4_100 XC    ELEMENT,ELEMENT                                                  
         LA    R4,ELEMENT                                                       
         USING TAT4D,R4                                                         
         MVI   TAT4EL,TAT4ELQ      SET ELCODE                                   
         MVI   TAT4LEN,TAT4LNQ     & LENGTH                                     
         MVC   TAT4UNIT(2),ST4PRV                                               
         MVI   TAT4UNIT+2,C' '                                                  
                                                                                
         LA    R2,ST4INCH                                                       
         BAS   RE,GETCASH                                                       
         JNE   BRT4_350                                                         
         MVC   TAT4EARN,FULL                                                    
*                                                                               
BRT4_350 CLC   ST4PRV,=C'QC'                                                    
         JE    BRT4_400                                                         
         LA    R2,ST4QPPH          NOT QUEBEC, CAN'T HAVE AMOUNT HERE           
         CLI   5(R2),0                                                          
         JNE   INVERR                                                           
         LA    R2,ST4CPPH                                                       
         J     BRT4_450                                                         
*                                                                               
BRT4_400 LA    R2,ST4CPPH          QUEBEC, CAN'T HAVE AMOUNT HERE               
         CLI   5(R2),0                                                          
         JNE   INVERR                                                           
         LA    R2,ST4QPPH          NOT QUEBEC, CAN'T HAVE AMOUNT HERE           
*                                                                               
BRT4_450 BAS   RE,GETCASH                                                       
         JNE   BRT4_500                                                         
         MVC   TAT4CPPC,FULL                                                    
*                                                                               
BRT4_500 LA    R2,ST4EIPH                                                       
         BAS   RE,GETCASH                                                       
         JNE   BRT4_550                                                         
         MVC   TAT4EPRM,FULL                                                    
*                                                                               
BRT4_550 LA    R2,ST4TAXH                                                       
         BAS   RE,GETCASH                                                       
         JNE   BRT4_600                                                         
         MVC   TAT4TAX,FULL                                                     
*                                                                               
BRT4_600 LA    R2,ST4EIEH                                                       
         BAS   RE,GETCASH                                                       
         JNE   BRT4_650                                                         
         MVC   TAT4EINE,FULL                                                    
*                                                                               
BRT4_650 LA    R2,ST4PENH                                                       
         BAS   RE,GETCASH                                                       
         JNE   BRT4_700                                                         
         MVC   TAT4PPER,FULL                                                    
*                                                                               
BRT4_700 LA    R2,ST4QPRH                                                       
         CLI   5(R2),0                                                          
         BE    BRT4_750                                                         
         CLC   ST4PRV,=C'QC'                                                    
         JNE   INVERR                                                           
         BAS   RE,GETCASH                                                       
         JNE   BRT4_750                                                         
         MVC   TAT4QPPR,FULL                                                    
*                                                                               
BRT4_750 LA    R2,ST4QPEH                                                       
         CLI   5(R2),0                                                          
         BE    BRT4_800                                                         
         CLC   ST4PRV,=C'QC'                                                    
         JNE   INVERR                                                           
         BAS   RE,GETCASH                                                       
         JNE   BRT4_800                                                         
         MVC   TAT4QPIE,FULL                                                    
*                                                                               
BRT4_800 GOTO1 ADDELEM                                                          
                                                                                
BRT4_900 XC    ELEMENT,ELEMENT                                                  
         LA    R4,ELEMENT                                                       
         USING TAWSD,R4                                                         
         MVI   TAWSEL,TAWSELQ      SET ELCODE                                   
         MVI   TAWSLEN,TAWSLNQ     & LENGTH                                     
         GOTO1 ADDELEM                                                          
*                                                                               
         GOTO1 ACTVIN,DMCB,ST4LCHGH      LAST CHANGED                           
*                                                                               
         L     R4,AIO                                                           
         USING TLT4D,R4                                                         
         CLI   ADDNEW,C'Y'         IF ADDING (CHANGING) REC - SET KEY           
         JE    BRT4_960                                                         
         CLI   ACTNUM,ACTADD       ON ACTADD                                    
         JNE   BRT4_980                                                         
         OI    TLT4STAT,X'40'      SET 'NEED TO PRINT BIT' ON                   
*                                                                               
BRT4_960 LA    R4,KEY                                                           
         USING TLT4D,R4                                                         
*                                                                               
BRT4_980 L     R4,AIO              SET KEY IN TO IO AREA                        
         USING TLT4D,R4                                                         
         MVC   0(L'TLT4KEY,R4),KEY                                              
*                                                                               
         CLI   ADDNEW,C'Y'                                                      
         JNE   BRT4_999            CHECK IF REALLY 'ADDING' FOR CHANGE          
         GOTO1 ADDREC                                                           
         BAS   RE,DRT4                                                          
         MVI   ADDNEW,C'N'                                                      
         MVI   IOOPT,C'Y'          APPLICATION WILL DO I/O                      
*                                                                               
BRT4_999 J     XIT                                                              
                                                                                
         EJECT                                                                  
*---------------------------------------------------------------------*         
*        BUILD RL-1 RECORD                                                      
*---------------------------------------------------------------------*         
BRR1     NTR1                                                                   
         GOTO1 SAVPTRS,DMCB,PTRBLK                                              
         MVI   ELCODE,TAWSELQ                                                   
         GOTO1 REMELEM                                                          
*                                                                               
         L     R4,AIO                                                           
         MVI   ELCODE,TAR1ELQ                                                   
         GOTO1 GETL,DMCB,(2,SRLPRV)                                             
         JNE   BRR1_100                                                         
         L     R4,TGELEM                                                        
         USING TAR1D,R4                                                         
         MVI   0(R4),X'FF'         MARK AND DELETE IT                           
         MVI   ELCODE,X'FF'                                                     
         GOTO1 REMELEM                                                          
*                                                                               
BRR1_100 XC    ELEMENT,ELEMENT                                                  
         LA    R4,ELEMENT                                                       
         USING TAR1D,R4                                                         
         MVI   TAR1EL,TAR1ELQ      SET ELCODE                                   
         MVI   TAR1LEN,TAR1LNQ     & LENGTH                                     
         MVC   TAR1UNIT,=C'QC '                                                 
                                                                                
         LA    R2,SRLINCH                                                       
         BAS   RE,GETCASH                                                       
         JNE   BRR1_150                                                         
         MVC   TAR1EARN,FULL                                                    
*                                                                               
BRR1_150 LA    R2,SRLQPPH                                                       
         BAS   RE,GETCASH                                                       
         JNE   BRR1_200                                                         
         MVC   TAR1QPPC,FULL                                                    
*                                                                               
BRR1_200 LA    R2,SRLEIPH                                                       
         BAS   RE,GETCASH                                                       
         JNE   BRR1_250                                                         
         MVC   TAR1QEIP,FULL                                                    
*                                                                               
BRR1_250 LA    R2,SRLTAXH                                                       
         BAS   RE,GETCASH                                                       
         JNE   BRR1_300                                                         
         MVC   TAR1TAX,FULL                                                     
*                                                                               
BRR1_300 LA    R2,SRLPENH                                                       
         BAS   RE,GETCASH                                                       
         JNE   BRR1_350                                                         
         MVC   TAR1QPPE,FULL                                                    
*                                                                               
BRR1_350 LA    R2,SRLQPRH                                                       
         BAS   RE,GETCASH                                                       
         JNE   BRR1_400                                                         
         MVC   TAR1PIPP,FULL                                                    
*                                                                               
BRR1_400 LA    R2,SRLQPWH                                                       
         BAS   RE,GETCASH                                                       
         JNE   BRR1_800                                                         
         MVC   TAR1PIPW,FULL                                                    
*                                                                               
BRR1_800 GOTO1 ADDELEM                                                          
                                                                                
BRR1_900 XC    ELEMENT,ELEMENT                                                  
         LA    R4,ELEMENT                                                       
         USING TAWSD,R4                                                         
         MVI   TAWSEL,TAWSELQ      SET ELCODE                                   
         MVI   TAWSLEN,TAWSLNQ     & LENGTH                                     
         GOTO1 ADDELEM                                                          
*                                                                               
         GOTO1 ACTVIN,DMCB,SRLLCHGH      LAST CHANGED                           
*                                                                               
         L     R4,AIO                                                           
         USING TLR1D,R4                                                         
         CLI   ADDNEW,C'Y'         IF ADDING (CHANGING) REC - SET KEY           
         JE    BRR1_960                                                         
         CLI   ACTNUM,ACTADD       ON ACTADD                                    
         JNE   BRR1_980                                                         
         OI    TLR1STAT,X'40'      SET 'NEED TO PRINT BIT' ON                   
*                                                                               
BRR1_960 LA    R4,KEY                                                           
         USING TLR1D,R4                                                         
*                                                                               
BRR1_980 L     R4,AIO              SET KEY IN TO IO AREA                        
         USING TLR1D,R4                                                         
         MVC   0(L'TLR1KEY,R4),KEY                                              
*                                                                               
         CLI   ADDNEW,C'Y'                                                      
         JNE   BRR1_999            CHECK IF REALLY 'ADDING' FOR CHANGE          
         GOTO1 ADDREC                                                           
         BAS   RE,DRR1                                                          
         MVI   ADDNEW,C'N'                                                      
         MVI   IOOPT,C'Y'          APPLICATION WILL DO I/O                      
*                                                                               
BRR1_999 J     XIT                                                              
                                                                                
         EJECT                                                                  
*---------------------------------------------------------------------*         
*        BUILD T4A-NR RECORD                                                    
*---------------------------------------------------------------------*         
BRNR     NTR1                                                                   
         J     XIT                                                              
         EJECT                                                                  
*---------------------------------------------------------------------*         
*        BUILD NR4 RECORD                                                       
*---------------------------------------------------------------------*         
BRN4     NTR1                                                                   
         J     XIT                                                              
         EJECT                                                                  
*---------------------------------------------------------------------*         
YES      SR    RC,RC                                                            
*                                                                               
NO       LTR   RC,RC                                                            
*                                                                               
XIT      XIT1                                                                   
*                                                                               
         SPACE 2                                                                
MISSERR  MVI   ERROR,MISSING                                                    
         B     ERRXIT                                                           
*                                                                               
ADDERR   MVI   ERROR,RECEXIST                                                   
         LA    R2,ST4SSNH                                                       
         B     ERRXIT                                                           
*                                                                               
RESTERR  MVI   ERROR,RECNTDEL                                                   
         LA    R2,ST4SSNH                                                       
         B     ERRXIT                                                           
*                                                                               
INVERR   MVI   ERROR,INVALID                                                    
         B     ERRXIT                                                           
*                                                                               
SECERR   MVI   ERROR,SECLOCK       SECURITY LOCK OUT                            
         B     ERRXIT                                                           
*                                                                               
ADDMSG   MVI   MYMSGNO1,69                                                      
         B     INFXIT                                                           
*                                                                               
CHPMSG   MVI   MYMSGNO1,70                                                      
         CLI   PFAID,13                                                         
         BE    INFXIT                                                           
         MVI   MYMSGNO1,69                                                      
         B     INFXIT                                                           
*                                                                               
INFXIT   LA    R2,CONRECH                                                       
         OI    GENSTAT2,USGETTXT                                                
*                                                                               
ERRXIT   XC    DMCB,DMCB                                                        
         GOTO1 EXIT,DMCB                                                        
*                                                                               
         SPACE 2                                                                
         GETEL R4,DATADISP,ELCODE                                               
         EJECT                                                                  
*              CONSTANTS, ETC.                                                  
         SPACE 2                                                                
PFTABLE  DS    0C                  PF KEYS TABLE                                
         SPACE 1                                                                
         DC    AL1(PF13X-*,13,0,0,PFTRETRN)                                     
         DC    CL3' ',CL8'        ',CL8'        '                               
PF13X    EQU   *                                                                
*                                                                               
         DC    AL1(PF14X-*,14,0,0,PFTRETRN)                                     
         DC    CL3' ',CL8'        ',CL8'        '                               
PF14X    EQU   *                                                                
         DC    X'FF'                                                            
         EJECT                                                                  
MYSPECS  DS    0H                                                               
         SSPEC H1,1,RUN                                                         
         SSPEC H1,56,REQUESTOR                                                  
         SSPEC H2,56,REPORT                                                     
         SSPEC H2,73,PAGE                                                       
         SPACE 1                                                                
         SSPEC H1,32,C'T4 LIST'                                                 
         SSPEC H2,32,C'-------'                                                 
         SPACE 1                                                                
         SSPEC H4,1,C'SIN'                                                      
         SSPEC H4,11,C'NAME'                                                    
         SSPEC H4,44,C'CHANGED'                                                 
         SSPEC H4,53,C'PRINTED'                                                 
         SPACE 1                                                                
         SSPEC H5,1,C'---'                                                      
         SSPEC H5,11,C'----'                                                    
         SSPEC H5,44,C'-------'                                                 
         SSPEC H5,53,C'-------'                                                 
         SPACE 1                                                                
         DC    X'00'                                                            
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
LISTD    DSECT                                                                  
W2SSN    DS    CL9                                                              
         DS    CL1                                                              
W2SSNN   DS    CL32                                                             
         DS    CL1                                                              
W2CHANGE DS    CL8                                                              
         DS    CL1                                                              
W2PRINT  DS    CL8                                                              
*                                                                               
         EJECT                                                                  
***********************************************************************         
       ++INCLUDE TAGENEQUS                                                      
       ++INCLUDE TAGENFFD                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE TASCR79D          T4 SCREEN                                    
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE TASCR90D          RL-1 SCREEN                                  
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE TASCR91D          T4A SCREEN                                   
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE TASCR92D          NR4 SCREEN                                   
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE TASCR05D                                                       
         EJECT                                                                  
*                                                                               
*        ORG   ST4WORK                                                          
*                                                                               
SVKEY    DS    CL38                SAVE THE KEY                                 
SVEMP    DS    CL3                 SAVED EMPLOYER                               
SVCUR    DS    CL1                       CURRENCY                               
ADDNEW   DS    CL1                 MUST ADD A NEW RECORD                        
XCDATE   DS    PL3                 COMPLEMENTED PWOS DATE                       
MYBYTE   DS    CL1                                                              
*YBYTE2  DS    CL1                                                              
TBYTE    DS    CL1                                                              
*                                                                               
SSNAMEH  DS    XL8                 FAKE FIELD HEADER                            
SSNAME   DS    CL32                OUTPUT FIELD                                 
*                                                                               
SVSSN    DS    CL9                                                              
SCROPT   DS    XL1                 OPTIONS                                      
SCRALL   EQU   X'80'               LIST ALL RECORD                              
*                                                                               
COUNTER  DS    PL4                 LINE COUNTER                                 
*                                                                               
EMPTAB   DS    10CL3               EMPLOYER TABLE                               
         DS    0D                                                               
PTRBLK   DS    CL(L'TLDRREC+1)     1 ACTIVE                                     
         EJECT                                                                  
       ++INCLUDE DDGENTWA                                                       
         EJECT                                                                  
         SPACE 5                                                                
TWAD     DSECT                                                                  
       ++INCLUDE TASYSIOD                                                       
         EJECT                                                                  
* TAGENWORKD                                                                    
* TAGENFILE                                                                     
* TASYSEQUS                                                                     
* TASYSDSECT                                                                    
* DDSPOOLD                                                                      
* DDSPLWORKD                                                                    
         PRINT OFF                                                              
       ++INCLUDE TAGENWORKD                                                     
       ++INCLUDE TAGENFILE                                                      
       ++INCLUDE TASYSEQUS                                                      
       ++INCLUDE TASYSDSECT                                                     
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'075TAGENF3   10/07/14'                                      
         END                                                                    
