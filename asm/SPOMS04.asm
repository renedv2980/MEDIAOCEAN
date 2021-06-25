*          DATA SET SPOMS04    AT LEVEL 096 AS OF 03/01/12                      
*PHASE T23404A  <=====                                                          
T23404   TITLE 'SPOMS04 - STATUS OF DARE ORDERS'                                
T23404   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,*T23404*,R7,RR=R3                                              
*                                                                               
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
*                                                                               
         BRAS  RE,SETPFKYS         SETUP THE PFKEYS                             
*                                                                               
         L     R1,=A(ERRORLST-T23404)                                           
         AR    R1,RB                                                            
         ST    R1,AERRLST                                                       
*                                                                               
         GOTO1 DATCON,DMCB,(5,0),(15,JDTTODAY)   GET CURRENT DATE               
*                                                                               
         CLI   MODE,VALKEY         VALIDATE KEY?                                
         BE    VK                                                               
         CLI   MODE,VALREC         VALIDATE RECORD?                             
         BE    DR                                                               
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* VALIDATE THE KEY                                                              
***********************************************************************         
VK       DS    0H                                                               
         NI    DMINBTS,X'FF'-X'08'                                              
         NI    MISCFLG1,MF1NOTIM+MF1BFCNF                                       
         NI    MISCFLG2,MF2VAROR+MF2VCONF+MF2REVOR                              
         NI    MISCFLG3,X'FF'-MF3RDCOM   MUST REREAD COMMENT RECORD             
***************                                                                 
* VALIDATE THE MEDIA                                                            
***************                                                                 
VKMED00  DS    0H                                                               
         LA    R2,ORDMEDH                                                       
*                                                                               
         TM    4(R2),X'20'            IF THIS KEY FIELD CHANGE                  
         BNZ   *+8                                                              
         OI    MISCFLG1,MF1KYCHG      THEN INDICATE IT                          
*                                                                               
         CLI   5(R2),0                NEED THE MEDIA                            
         BNE   VKMED05                                                          
         L     RF,ACOMFACS                                                      
         L     RF,CGLOBBER-COMFACSD(RF)                                         
         GOTO1 (RF),DMCB,=C'GETF',(R2),,GLVSPMD                                 
         CLI   8(R1),0                                                          
         BE    VKMED05                                                          
         B     NEEDFLDS                                                         
*                                                                               
VKMED05  GOTO1 VALIMED                                                          
*                                                                               
         L     RF,ACOMFACS                                                      
         L     RF,CGLOBBER-COMFACSD(RF)                                         
         GOTO1 (RF),DMCB,=C'PUTF',(R2),,GLVSPMD                                 
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         OI    4(R2),X'20'                                                      
*                                                                               
VKMEDX   DS    0H                                                               
***************                                                                 
* VALIDATE THE ORDER NUMBER                                                     
***************                                                                 
VKORD00  DS    0H                                                               
         LA    R2,ORDORDRH                                                      
         TM    4(R2),X'20'                                                      
         BNZ   *+8                                                              
         OI    MISCFLG1,MF1KYCHG                                                
*                                                                               
         CLI   5(R2),0                                                          
         BE    MISSFLD                                                          
         CLI   5(R2),8             YJJJSSSS - YEAR,JULIAN,SEQ                   
         BNE   INVLFLD                                                          
*****    TM    4(R2),X'08'         HAS TO BE NUMERIC?                           
*****    BZ    INVLFLD                                                          
         LA    RE,8                THE ABOVE CODE IS COMMENTED BECAUSE          
         LA    RF,8(R2)              WHEN WE SELECT FROM THE LIST               
VKORD10  CLI   0(RF),C'0'            SCR, IT COPIES THE VALIDATED BITS.         
         BL    INVLFLD                                                          
         CLI   0(RF),C'9'          SINCE WE'RE USING 1 BYTE FOR TRADE           
         BH    INVLFLD               IN THE ORDER NUMBER ON THE LIST            
         LA    RF,1(RF)              SCREEN IT WON'T BE A NUMERIC FLD           
         BCT   RE,VKORD10                                                       
*                                                                               
         CLI   8+1(R2),C'3'        Is 2nd digit, higher than 3?                 
         BH    VKORD20                                                          
*                                                                               
         ZAP   FULL,JDTTODAY       CENTURY/YEAR FROM TODAY'S DATE               
         SRP   FULL,61,0                                                        
*                                                                               
         GOTO1 HEXIN,DMCB,8(R2),FAKEFLD,8   SAVE AS IF ENTRY WAS HEX            
         MVC   PACKOF4B,FAKEFLD    CONVERT IT TO PACK                           
         OI    PACKOF4B+3,X'0F'                                                 
         SRP   PACKOF4B,58,0       ISOLATE THE YEAR                             
         MVC   PACKOF4B+2(1),FULL+2 COPY TODAY'S CENTURY                        
*                                                                               
         CP    PACKOF4B+3(1),FULL+3(1) LESS 10 YEARS?                           
         BNH   *+10                                                             
         SP    PACKOF4B,=P'10'     YEAR DIGIT>TODAY'S YEAR DIGIT                
*                                                                               
         SP    PACKOF4B,=P'90'     CALCULATE FROM 1990                          
         SRP   PACKOF4B,4,0                                                     
         OC    PACKOF4B+1(2),FAKEFLD   STICK IN DAYS IN YEAR                    
         SRP   PACKOF4B,63,0                                                    
*                                                                               
         ZAP   DUB,PACKOF4B        SAVE DATE PORTION                            
         CVB   R0,DUB                                                           
         STCM  R0,3,BINORDDT                                                    
         XC    BINORDDT,=4X'FF'                                                 
*                                                                               
         PACK  DUB,8+4(4,R2)       SAVE SEQUENCE PORTION                        
         CVB   R0,DUB                                                           
         STCM  R0,3,BINORDSQ                                                    
         XC    BINORDSQ,=4X'FF'                                                 
         B     VKORDX                                                           
*                                                                               
VKORD20  PACK  DUB,8(8,R2)         NEW STYLE ORDER NUMBER                       
         SP    DUB,=P'04000000'                                                 
         CVB   R1,DUB                                                           
         STCM  R1,15,BINORDER                                                   
         OI    BINORDER,X'80'                                                   
         XC    BINORDER,=4X'FF'                                                 
*                                                                               
VKORDX   OI    4(R2),X'20'         VALIDATE THE FIELD                           
         DS    0H                                                               
         EJECT                                                                  
***************                                                                 
* VALIDATE OPTION FIELD                                                         
***************                                                                 
VKOPT    DS    0H                                                               
         TM    PROGFLG1,PF1HTTP    WERE WE JUST ON THE HTTP SCREEN?             
         BZ    *+8                                                              
         OI    PROGFLG1,PF1WSHTP   YES, SO WE DON'T CLEAR ELEMDISP              
*                                                                               
         NI    PROGFLG1,PF1PPER+PF1WSHTP                                        
         NI    PROGFLG2,X'FF'-PF2EXTRA                                          
*                                                                               
         LA    R2,ORDOPTNH                                                      
         TM    4(R2),X'20'                                                      
         BNZ   *+8                                                              
         OI    MISCFLG1,MF1KYCHG                                                
*                                                                               
VKOPT05  CLI   5(R2),0                                                          
         BE    VKOPT99                                                          
*                                                                               
         XC    BLOCK(256),BLOCK                                                 
         GOTO1 SCANNER,DMCB,(R2),BLOCK                                          
         ZICM  R4,4(R1),1          NUMBER OF OPTIONS ENTERED                    
         BZ    INVLFLD                                                          
*                                                                               
         LA    R3,BLOCK                                                         
VKOPT10  CLI   1(R3),0             CAN'T HAVE =???                              
         BNE   INVLFLD                                                          
*                                                                               
         ZIC   R1,0(R3)                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         BNE   VKOPT15                                                          
         CLC   12(0,R3),=CL7'REBUILD'                                           
*                                                                               
         CLI   TWAOFFC,C'*'        TEST DDS TERMINAL                            
         BNE   INVLFLD             NO, NO VALID OPTIONS                         
*                                                                               
         OI    PROGFLG1,PF1REBLD                                                
         B     VKOPT50                                                          
*                                                                               
VKOPT15  ZIC   R1,0(R3)                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         BNE   VKOPT17                                                          
         CLC   12(0,R3),=CL7'EMDLVD'                                            
*                                                                               
         CLI   TWAOFFC,C'*'        TEST DDS TERMINAL                            
         BNE   INVLFLD             NO, NO VALID OPTIONS                         
*                                                                               
         OI    PROGFLG1,PF1EMDLV                                                
         B     VKOPT50                                                          
*                                                                               
VKOPT17  ZIC   R1,0(R3)            WBUC WANTS TO SEE HTTPS?                     
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         BNE   VKOPT20                                                          
         CLC   12(0,R3),=CL7'HTTP'                                              
*                                                                               
         CLI   ORDURLS,C'Y'        ARE THERE ANY TO SHOW?                       
         BNE   INVLFLD             NONE, WHY ARE YOU TRYING?                    
         OI    PROGFLG1,PF1HTTP    YES                                          
         B     VKOPT50                                                          
*                                                                               
VKOPT20  EX    R1,*+8                                                           
         BNE   VKOPT25                                                          
         CLC   12(0,R3),=CL7'HISTORY'                                           
*                                                                               
         CLI   QMED,C'R'                                                        
         BNE   INVLFLD                                                          
*                                                                               
         OI    PROGFLG1,PF1HSTRY                                                
         B     VKOPT50                                                          
*                                                                               
VKOPT25  EX    R1,*+8                                                           
         BNE   INVLFLD                                                          
         CLC   12(0,R3),=CL5'EXTRA'                                             
*                                                                               
         OI    PROGFLG2,PF2EXTRA                                                
*                                                                               
VKOPT50  LA    R3,32(R3)                                                        
         CLI   0(R3),0             ACCEPT ONLY 1 OPTION                         
         BNE   INVLFLD                                                          
         CLI   1(R3),0                                                          
         BNE   INVLFLD                                                          
*                                                                               
         TM    PROGFLG1,PF1REBLD                                                
         BZ    VKOPT99                                                          
*                                                                               
         XC    ORDOPTN,ORDOPTN     CLEAR IT                                     
         OI    ORDOPTNH+6,X'80'    TRANSMIT                                     
VKOPT99  OI    4(R2),X'20'         VALIDATE THE FIELD                           
*                                                                               
VKOPTX   DS    0H                                                               
*                                                                               
*****                                                                           
* BUILD THE KEY                                                                 
*****                                                                           
VKBKEY   XC    KEY,KEY             CLEAN OUT THE KEY                            
         LA    R4,KEY                                                           
         USING DOKEY,R4            OVERLAY KEY WITH OUR TEMPLATE                
         MVI   DOKTYPE,DOKTYPQ                                                  
         MVI   DOKSUBTY,DOKSTYPQ                                                
         MVC   DOKAGMD,BAGYMD                                                   
         MVC   DOKORDER,BINORDER                                                
***      MVC   DOKSTA,BSTA                                                      
*                                                                               
         GOTO1 HIGH                                                             
         CLI   DOKCMT,0            DID WE GET A COMMENT RECORD?                 
         BNE   VKBKEY05            THIS IS BAD, BUT DON'T DIE                   
*                                                                               
         CLC   KEY(DOKSTA-DOKEY),KEYSAVE                                        
         BE    VKBKEY10                                                         
*                                                                               
VKBKEY05 XC    DOKSTA,DOKSTA                                                    
*                                                                               
VKBKEY10 MVC   SAVEKEY,KEY                                                      
*                                                                               
         XC    ORDSTA,ORDSTA                                                    
         OI    ORDSTAH+6,X'80'                                                  
*        XC    ORDRCON,ORDRCON                                                  
         OI    ORDRCONH+6,X'80'                                                 
*                                                                               
         OC    DOKSTA,DOKSTA                                                    
         BZ    VKXIT                                                            
*                                                                               
         XC    WORK(5),WORK                                                     
         MVC   WORK+2(3),DOKSTA                                                 
         GOTO1 MSUNPK,DMCB,WORK,WORK+5,ORDSTA                                   
*                                                                               
         LA    R2,ORDSTAH                                                       
         MVI   5(R2),4                                                          
         CLI   11(R2),C' '                                                      
         BH    *+8                                                              
         MVI   5(R2),3                                                          
         CLI   12(R2),C' '                                                      
         BNH   *+8                                                              
         MVI   5(R2),5                                                          
*                                                                               
         GOTO1 VALISTA                                                          
         MVC   ORDMKXP,MKTNM                                                    
         OI    ORDMKXPH+6,X'80'                                                 
*                                                                               
VKXIT    XC    KEY,KEY                                                          
         MVC   KEY(L'SAVEKEY),SAVEKEY                                           
         B     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
* DISPLAY THE RECORD                                                            
***********************************************************************         
DR       DS    0H                                                               
*                                                                               
         TM    MISCFLG1,MF1KYCHG   IF THE KEY CHANGED                           
         BZ    DR04                                                             
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(L'DOKEY),KEYSAVE                                             
         BNE   RECNTFND                                                         
         GOTO1 GETREC                                                           
*                                                                               
* GET OM PROFILE!!                                                              
         XC    WORK,WORK                                                        
         MVC   WORK(4),=C'S0OM'                                                 
         MVC   WORK+4(2),AGENCY                                                 
         MVC   WORK+6(1),QMED                                                   
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,DOIDELQ      X'01'                                        
         BAS   RE,GETEL                                                         
         USING DOIDELD,R6                                                       
         GOTO1 CLUNPK,DMCB,DOIDCLT,WORK+7   GET CLIENT CODE                     
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY+1(1),BAGYMD                                                  
         MVC   KEY+2(2),DOIDCLT                                                 
         DROP  R6                                                               
*                                                                               
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(4),KEYSAVE                                                   
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   AIO,AIO2                                                         
         L     R6,AIO                                                           
         USING CLTHDRD,R6                                                       
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
*                                                                               
         CLI   COFFICE,C' '                                                     
         BNH   *+14                                                             
         MVI   WORK+10,C'*'                                                     
         MVC   WORK+11(1),COFFICE                                               
         L     R1,ACOMFACS         RF = A(GETPROF)                              
         L     RF,CGETPROF-COMFACSD(R1)                                         
         GOTO1 (RF),DMCB,WORK,PROFOM,DATAMGR                                    
         DROP  R6                                                               
*                                                                               
         MVC   AIO,AIO1                                                         
         MVC   KEY,SAVEKEY                                                      
*                                                                               
DR04     TM    PROGFLG2,PF2EXTRA                                                
         BZ    *+8                                                              
         MVI   KEY+12,X'05'        READ THE EXTRA RECORD                        
         GOTO1 HIGH                                                             
*                                                                               
         CLC   KEY(L'DOKEY),KEYSAVE                                             
         BE    DR06                                                             
         TM    PROGFLG2,PF2EXTRA                                                
         BNZ   DR12                                                             
         B     RECNTFND                                                         
DR06     MVC   DISKADDR,KEY+14                                                  
*                                                                               
         CLI   TWAOFFC,C'*'        DDS TERMINAL?                                
         BNE   DR08                                                             
         MVC   CONHED2+50(4),=CL4'D/A='                                         
         GOTO1 HEXOUT,DMCB,DISKADDR,CONHED2+54,4                                
*                                                                               
DR08     TM    PROGFLG1,PF1REBLD+PF1EMDLV  DO WE NEED TO REBUILD ORDER?         
         BZ    *+8                                                              
         MVI   RDUPDATE,C'Y'       YES, READ FOR UPDATE                         
         GOTO1 GETREC                                                           
*                                                                               
DR10     OC    ELEMDISP,ELEMDISP                                                
         BZ    DR12                                                             
         L     R6,AIO                                                           
         USING DOKEY,R6                                                         
         CLC   RECLENB4,DORLEN     IF RECORD LENGTHS DON'T MATCH                
         BE    DR12                WE COULD BE POINTING ANYWHERE                
         XC    ELEMDISP,ELEMDISP   SO WE'LL START FROM BEGINNING AGAIN          
         XC    HTTPDISP,HTTPDISP                                                
*                                                                               
DR12     LA    R2,ORDHED1H                                                      
         USING HEADLIND,R2                                                      
         MVC   HLSUB1,HEDLINE1                                                  
         TM    PROGFLG1,PF1HSTRY   SHOW THE SALESPERSON?                        
         BZ    DR14                                                             
         BAS   RE,INSSALPR         YES, INSERT INTO RECORD                      
         MVC   HLSUB2,HEDLINE2                                                  
DR14     TM    PROGFLG2,PF2EXTRA   SHOW THE EXTRA DATA?                         
         BZ    DR15                                                             
         MVC   HLSUB1,HEDLINE3                                                  
DR15     OI    6(R2),X'80'         TRANSMIT IT                                  
         DROP  R2                                                               
*                                                                               
DR16     TM    PROGFLG1,PF1EMDLV   PATCH IN A EMAIL DLVD?                       
         BZ    DR17                                                             
         BRAS  RE,PTCHEMDL                                                      
*                                                                               
DR17     TM    PROGFLG1,PF1REBLD   DO WE NEED TO REBUILD ORDER?                 
         BZ    DR18                                                             
         BRAS  RE,REBLDORD                                                      
*                                                                               
DR18     TM    PROGFLG1,PF1HTTP    DO WE SHOW HTTP TO WBUC?                     
         BZ    DR19                                                             
         BRAS  RE,SHOWURLS                                                      
         B     XIT                                                              
*                                                                               
DR19     TM    PROGFLG2,PF2EXTRA   DO WE SHOW THE EXTRA SCREEN                  
         BZ    DR20                                                             
         BRAS  RE,SHWEXTRA                                                      
         B     XIT                                                              
*                                                                               
DR20     TM    MISCFLG1,MF1KYCHG   IF THE KEY CHANGED                           
         BZ    DR30                                                             
         TM    PROGFLG1,PF1WSHTP   OPTION CHANGED FROM HTTP TO BLANKS           
         BNZ   DR30                                                             
*                                                                               
         XC    ELEMDISP,ELEMDISP   THEN START FROM 1ST TRANS ELEMENT            
         XC    HTTPDISP,HTTPDISP                                                
         XC    RECLENB4,RECLENB4   NO PREVIOUS RECORD LENGTH                    
         MVI   REVISION,0                                                       
         MVI   HTTPRVSN,0                                                       
         NI    MISCFLG1,X'FF'-MF1NOTIM-MF1AMEND                                 
         MVC   HTTPFLG1,MISCFLG1                                                
         NI    MISCFLG2,X'FF'-MF2VAROR-MF2VCONF-MF2REVOR                        
         NI    MISCFLG3,X'FF'-MF3RDCOM                                          
*                                                                               
         L     R6,AIO                                                           
         MVC   FULL,DOKORDER                                                    
         XC    FULL,=X'FFFF'                                                    
*                                                                               
         TM    FULL,X'80'          NEW STYLE ORDER #?                           
         BZ    DR23                NO, CREATION DATE IN ORDER #                 
*                                                                               
         MVI   ELCODE,DOSPELQ      x'03'                                        
         BAS   RE,GETEL                                                         
         USING DOSPELD,R6                                                       
         OC    DOSPCDAT,DOSPCDAT                                                
         BZ    DR24                                                             
         GOTO1 DATCON,DMCB,(8,DOSPCDAT),(8,ORDCDAT)                             
         B     DR24                                                             
*                                                                               
         USING DOKEY,R6                                                         
DR23     ZICM  R1,FULL,2                                                        
         CVD   R1,DUB                                                           
         ZAP   FULL,DUB                                                         
         SRP   DUB,61,0                                                         
         AP    DUB,=P'90'                                                       
         SRP   DUB,3,0                                                          
         AP    DUB,FULL+2(2)                                                    
         ZAP   FULL,DUB                                                         
         GOTO1 DATCON,DMCB,(6,FULL),(8,ORDCDAT)                                 
*                                                                               
DR24     OI    ORDCDATH+6,X'80'    TRANSMIT                                     
         MVI   ELCODE,DOI2ELQ                                                   
         BAS   RE,GETEL                                                         
         BNE   DR25                                                             
         USING DOI2ELD,R6                                                       
         TM    DOI2FLG1,DOI2FVAR   IS THIS ORDER NOW A VAR ORDER?               
         BZ    *+8                                                              
         OI    MISCFLG2,MF2VAROR                                                
         TM    DOI2FLG1,DOI2FVCN   IS THE VAR ORDER CONFIRMED?                  
         BZ    *+8                                                              
         OI    MISCFLG2,MF2VCONF                                                
*                                                                               
DR25     NI    MISCFLG2,X'FF'-MF2FAXED+MF2EMLED                                 
         L     R6,AIO                                                           
         MVI   ELCODE,DOWIGELQ                                                  
         BAS   RE,GETEL                                                         
         BNE   DR30                                                             
         USING DOWIGELD,R6                                                      
         CLI   DOWIGMTH,C'F'                                                    
         BNE   *+8                                                              
         OI    MISCFLG2,MF2FAXED                                                
         CLI   DOWIGMTH,C'E'                                                    
         BNE   *+8                                                              
         OI    MISCFLG2,MF2EMLED                                                
*                                                                               
DR30     XC    SAVECRTR,SAVECRTR   CLEAR OUT SAVED CREATOR ID NUM               
         L     R6,AIO                                                           
         MVI   ELCODE,DOSPELQ                                                   
         BAS   RE,GETEL                                                         
         BNE   DR45                                                             
         USING DOSPELD,R6                                                       
         ST    R6,ADOSPELM                                                      
*                                                                               
         OC    DOSPCRTR,DOSPCRTR   DO WE HAVE A CREATOR ID NUM                  
         BZ    *+10                 NO                                          
         MVC   SAVECRTR,DOSPCRTR   SAVE OFF THE CREATOR ID NUM                  
*                                                                               
***  LOOKING FOR DOSPAVN TO SEE IF PREVIOUSLY SENT                              
         XC    ORDPREV,ORDPREV                                                  
         TM    DOSPFLG1,DOSPAVN    (X'20')                                      
         BZ    *+10                RETRANSMIT NO MATTER WHAT                    
         MVC   ORDPREV,=C'PREV'                                                 
         OI    ORDPREVH+6,X'80'    TRANSMIT                                     
***                             07/26/02   MHC                                  
*                                                                               
         NI    MISCFLG1,X'FF'-MF1NOSTA                                          
*                                                                               
         OC    ELEMDISP,ELEMDISP   DO WE HAVE A STARTING POINT?                 
         BNZ   DR42                YES, KEEP REVISION AS IS                     
         MVC   REVISION,DOSPREVN   GET REVISION NUMBER FROM ELEMENT             
         MVC   TOTALREV,DOSPREVN   SAVE TOTAL REVISIONS                         
         OI    MISCFLG2,MF2REVOR                                                
         OI    MISCFLG1,MF1NOSTA   HAVEN'T SHOWN A STATUS YET                   
*                                                                               
         TM    PROGFLG1,PF1WSHTP                                                
         BNZ   DR35                                                             
         MVC   HTTPRVSN,DOSPREVN                                                
         MVC   HTTPFLG1,MISCFLG1                                                
*                                                                               
DR35     MVI   REVNLSS7,0          ASSUME SHOW ALL REVISIONS                    
         CLI   TOTALREV,MAXREVN                                                 
         BNH   DR40                                                             
         ZIC   R1,TOTALREV                                                      
         AHI   R1,-7                                                            
         STC   R1,REVNLSS7                                                      
*                                                                               
DR40     TM    DOSPFLG1,DOSPPPER   POINTPERSON?                                 
         BZ    *+8                                                              
         OI    PROGFLG1,PF1PPER                                                 
*                                                                               
DR42     XC    ORDSPTS,ORDSPTS                                                  
         OI    ORDDLRSH+6,X'80'                                                 
         OC    DOSPSPTS,DOSPSPTS                                                
         BZ    DR45                                                             
         EDIT  (B4,DOSPSPTS),(6,ORDSPTS),ALIGN=LEFT                             
*                                                                               
         XC    ORDDLRS,ORDDLRS                                                  
         OI    ORDDLRSH+6,X'80'                                                 
         OC    DOSPTOTL,DOSPTOTL                                                
         BZ    DR45                                                             
         EDIT  (P6,DOSPTOTL),(10,ORDDLRS),2,ALIGN=LEFT                          
*                                                                               
DR45     XC    ORDFAID,ORDFAID                                                  
         OI    ORDFAIDH+6,X'80'                                                 
         CLI   DOSPLEN,DOSPLNQ2                                                 
         BL    DR47                                                             
         OC    DOSPFAID,DOSPFAID                                                
         BZ    DR47                                                             
         GOTO1 DATCON,DMCB,(2,DOSPFAID),(8,ORDFAID)                             
         DROP  R6                                                               
*                                                                               
DR47     TM    PROGFLG1,PF1HTTP    GETTING HTTP FOR WBUC?                       
         BNZ   DR50                YES, DON'T CLEAR MY HTP FIELDS               
         XC    ORDHTP1,ORDHTP1                                                  
         XC    ORDHTP2,ORDHTP2                                                  
         XC    ORDHTP3,ORDHTP3                                                  
         XC    ORDHTP4,ORDHTP4                                                  
         XC    ORDHTP5,ORDHTP5                                                  
         XC    ORDHTP6,ORDHTP6                                                  
         OI    ORDHTP1H+6,X'80'    TRANSMIT                                     
         OI    ORDHTP2H+6,X'80'                                                 
         OI    ORDHTP3H+6,X'80'                                                 
         OI    ORDHTP4H+6,X'80'                                                 
         OI    ORDHTP5H+6,X'80'                                                 
         OI    ORDHTP6H+6,X'80'                                                 
         MVI   ORDURLS,0           NO NEED FOR WBUC TO USE HTTP OPTION          
         OI    ORDURLSH+6,X'80'                                                 
*                                                                               
DR50     MVC   ORDCRTR,SPACES      CLEAR THE CREATOR LINE                       
         OI    ORDCRTRH+6,X'80'    TRANSMIT                                     
*                                                                               
         BRAS  RE,MYTWAXC                                                       
*                                                                               
DR55     LA    R2,ORDEVT1H                                                      
         USING DISLINED,R2                                                      
*                                                                               
         XC    PREVJUDT,PREVJUDT   ALWAYS SHOW DATE ON 1ST LINE                 
*                                                                               
         L     R6,AIO              POINT TO THE RECORD                          
         LA    R6,DORFRST-DOKEY(R6)                                             
         USING DOIDELD,R6                                                       
         MVC   PREVCON,DOIDCON                                                  
         MVC   ORDRCON,DOIDCON                                                  
         MVC   SAVESALP,DOIDSPER                                                
         DROP  R6                                                               
*                                                                               
         TM    PROGFLG1,PF1WSHTP   RETURN BACK FROM HTTP SCREEN?                
         BZ    DR60                                                             
         MVC   ELEMDISP,HTTPDISP                                                
         MVC   REVISION,HTTPRVSN                                                
         MVC   MISCFLG1,HTTPFLG1                                                
*                                                                               
DR60     OC    ELEMDISP,ELEMDISP   DO WE HAVE A STARTING POINT?                 
         BZ    DR62                NO, WE'RE OKAY AT THE FIRST ELEM             
*                                                                               
         L     R6,AIO                                                           
         AH    R6,ELEMDISP         POINT TO WHERE WE'RE SUPPOSED TO BE          
         B     DR64                                                             
*                                                                               
DR62     XC    PREVREP,PREVREP     THE ONE IN DEST FIELD                        
*                                                                               
DR64     MVC   HTTPDISP,ELEMDISP                                                
         MVC   HTTPRVSN,REVISION                                                
         MVC   HTTPFLG1,MISCFLG1                                                
*                                                                               
DR65     CLI   0(R6),0                                                          
         BNE   DR68                                                             
         XC    ELEMDISP,ELEMDISP                                                
         B     DRXIT                                                            
*                                                                               
DR68     CLI   0(R6),DORPELQ2                                                   
         BE    DR70                                                             
         CLI   0(R6),DOSTELQ                                                    
         BE    XZ10                                                             
         CLI   0(R6),DOSAPELQ                                                   
         BE    DR75                                                             
         ZIC   R0,1(R6)                                                         
         AR    R6,R0                                                            
         B     DR65                                                             
***************                                                                 
* REP CHANGED ELEMENT                                                           
***************                                                                 
         USING DOREPELD,R6                                                      
DR70     TM    PROGFLG1,PF1HSTRY   AM I SHOWIING HISTORY?                       
         BO    DR74                                                             
*                                                                               
         OC    PREVREP,PREVREP     ANY REP IN DEST FIELD?                       
         BNZ   DR72                YES, SHOW ID ON THE LINE IF DIFF             
         GOTO1 SHWIDNUM,DMCB,DORPNREP,ORDDESTH                                  
         XC    ORDFDST,ORDFDST                                                  
         OI    ORDFDSTH+6,X'80'                                                 
*                                                                               
DR72     MVC   DISEVNT(18),=CL18'REP WAS CHANGED ON'                            
         GOTO1 DATCON,DMCB,(8,DORPDATE),(8,DISDATE)                             
         GOTO1 SHOWTIME,DMCB,DORPTIME,DISTIME                                   
         GOTO1 SHWIDNUM,DMCB,DORPNREP,FAKEFLDH                                  
         MVC   DISREP,FAKEFLD                                                   
         MVC   DISRCON,PREVCON                                                  
         MVC   PREVCON,DORPPRCN    SAVE PREVIOUS REP CONTRACT                   
         MVC   PREVREP,DORPNREP    WE HAVE A REP ID OVERRIDE                    
*                                                                               
         CLI   REVISION,0          AM I IN REVISION?                            
         BNE   *+8                                                              
         NI    MISCFLG2,X'FF'-MF2REVOR  NO, TURN OFF                            
*                                                                               
DR74     BAS   RE,DRLCHK                                                        
         BE    XZNXTELM                                                         
         ST    R6,ACURELEM         SAVE R6                                      
         B     DRXIT                                                            
***************                                                                 
* SALESPERSON ELEMENT                                                           
***************                                                                 
DR75     DS    0H                                                               
*                                                                               
         ZIC   R1,1(R6)                                                         
         LA    R4,0(R1,R6)                                                      
*                                                                               
         CLI   0(R4),0             EOF?                                         
         BE    DR97                YES!                                         
         CLI   0(R4),DORPELQ2                                                   
         BE    DR80                                                             
         CLI   0(R4),DOSTELQ                                                    
         BE    DR80                                                             
         CLI   0(R4),DOSAPELQ                                                   
         BNE   DR97                                                             
*                                                                               
         USING DOSALPRD,R6                                                      
DR80     GOTO1 DATCON,DMCB,(8,DOSADATE),(8,DISDATE)                             
*                                                                               
         MVC   DISSALT1,=CL15'NEW SALESPERSON'                                  
         CLI   DOSALEN,DOSALNQ                                                  
         BNE   DR85                                                             
         MVC   DISSALT2,=CL8'FROM REP'                                          
         MVC   DISSALP,SAVESALP                                                 
         B     DR90                                                             
*                                                                               
DR85     TM    DOSAFLAG,DOSABUYR                                                
         BO    *+10                                                             
         MVC   DISSALT2,=CL8'FROM REP'                                          
         MVC   DISSALP,DOSASALP                                                 
*                                                                               
DR90     MVC   SAVESALP,DOSASALP                                                
*                                                                               
         CLI   DOSALEN,DOSALNQ                                                  
         BNE   DR92                                                             
         MVI   DISPPER,C'N'                                                     
         TM    PROGFLG1,PF1PPER                                                 
         BZ    *+8                                                              
         MVI   DISPPER,C'Y'                                                     
         B     DR95                                                             
*                                                                               
DR92     MVI   DISPPER,C'N'                                                     
         TM    DOSAFLAG,DOSAPPER                                                
         BZ    *+8                                                              
         MVI   DISPPER,C'Y'                                                     
*                                                                               
DR95     NI    PROGFLG1,X'FF'-PF1PPER                                           
         TM    DOSAFLAG,DOSAPPER                                                
         BZ    *+8                                                              
         OI    PROGFLG1,PF1PPER                                                 
         BAS   RE,DRLCHK                                                        
         B     XZNXTELM                                                         
*                                                                               
DR97     CLI   DOSALEN,DOSALNQ                                                  
         BE    DR400                                                            
         MVC   SAVESALP,DOSASALP                                                
DR400    NI    PROGFLG1,X'FF'-PF1PPER                                           
         TM    DOSAFLAG,DOSAPPER                                                
         BZ    *+8                                                              
         OI    PROGFLG1,PF1PPER                                                 
         B     XZ190                                                            
***************                                                                 
* TRANSMISSION ELEMENT                                                          
***************                                                                 
         USING DOSTELD,R6                                                       
XZ10     CLI   REVISION,0                                                       
         BNE   *+12                                                             
         NI    MISCFLG2,X'FF'-MF2REVOR                                          
         B     XZ15                                                             
         TM    PROGFLG1,PF1HSTRY                                                
         BO    XZ15                                                             
         EDIT  (B1,REVISION),(3,DISREV),FILL=0                                  
*                                                                               
XZ15     CLI   DOSTSTAT,DSENT      STATUS MUST BE SENT...                       
         BE    XZ16                                                             
         CLI   DOSTSTAT,DFXSENT    ...FAX SENT...                               
         BE    XZ16                                                             
         CLI   DOSTSTAT,DFXRSNT    ...FAX RESENT...                             
         BE    XZ16                                                             
         CLI   DOSTSTAT,DEMSENT    ...OR EMAIL SENT...                          
         BNE   XZ20                                                             
XZ16     CLC   PREVID,DOSTIDNM     IS THE ID THE SAME AS THE PREVIOUS?          
         BE    XZ20                                                             
         MVC   PREVID,DOSTIDNM     STORE NEW ID NUM                             
         GOTO1 SHWIDNUM,DMCB,DOSTIDNM,FAKEFLDH   DISPLAY THE ORIGIN ID          
         MVC   DISORGID,FAKEFLD                                                 
***  ADDED 07/24/02 TO SHOW THE ORIGINATOR ID    MHC                            
*                                                                               
XZ20     CLI   DOSTSTAT,QCFMD      AM I CONFIRMED?                              
         BNE   XZ23                                                             
         NI    MISCFLG1,X'FF'-MF1CNFCM-MF1AUTCF                                 
         CLI   DOSTLEN,DOSTLNQ3    DO I HAVE A TYPE FIELD?                      
         BNE   XZ22                                                             
XZ21     TM    DOSTTYPE,DCNFMCOM   CONFIRM WITH COMMENTS?                       
         BZ    XZ21A                                                            
         OI    MISCFLG1,MF1CNFCM                                                
         B     XZ24                                                             
*                                                                               
XZ21A    TM    DOSTTYPE,DCNFMFUL                                                
         BZ    XZ24                                                             
         OI    MISCFLG1,MF1AUTCF   ORDER IS AUTO-CONFIRMED                      
         B     XZ24                                                             
*                                                                               
XZ22     ST    R6,ACURELEM         SAVE R6                                      
         L     R6,ADOSPELM         POINT TO DOSPELEM                            
         USING DOSPELD,R6                                                       
         TM    DOSPFLG1,DOSPCFCM   CONFIRM WITH COMMENT?                        
         BZ    *+8                                                              
         OI    MISCFLG1,MF1CNFCM   YES                                          
         L     R6,ACURELEM         RESTORE R6                                   
*                                                                               
         USING DOSTELD,R6                                                       
XZ23     NI    MISCFLG1,X'FF'-MF1AMEND                                          
         CLI   DOSTSTAT,QRJCT      AM I REJECTED?                               
         BNE   XZ24                                                             
         CLI   DOSTLEN,DOSTLNQ3                                                 
         BNE   XZ24                                                             
         OI    MISCFLG1,MF1AMEND                                                
*                                                                               
XZ24     LA    R3,ORDSTAB                                                       
         USING ORDD,R3                                                          
         SR    R0,R0                                                            
XZ25     CLC   DOSTSTAT,ORDSTAT           MATCH STATUS?                         
         BNE   XZNXTSTT                   NO: CHECK NEXT STATUS ENTRY           
*                                                                               
* CODE TO DISPLAY REP CODE                                                      
*                                                                               
         TM    ORDIND2,ORD2CKID           DISPLAY REP CODE?                     
         BZ    XZ40                       NO                                    
         OC    PREVREP,PREVREP            ANY REP IN DEST FIELD?                
         BNZ   XZ30                                                             
         MVC   SAVEREP,DOSTIDNM                                                 
         MVC   PREVREP,DOSTIDNM           NO: DISPLAY THE REP                   
         CLI   DOSTSTAT,DEMDLVD           IS IT EMAIL ORDER?                    
         BNE   *+10                                                             
         MVC   PREVREP,=X'FFFD'                                                 
         CLI   DOSTSTAT,DFXDLVD           IS IT FAX ORDER?                      
         BNE   *+10                                                             
         MVC   PREVREP,=X'FFFF'                                                 
         XC    ORDDEST,ORDDEST                                                  
         GOTO1 SHWIDNUM,DMCB,PREVREP,ORDDESTH                                   
         MVC   PREVREP,SAVEREP                                                  
         XC    ORDFDST,ORDFDST                                                  
         OI    ORDFDSTH+6,X'80'                                                 
         B     XZ35                                                             
*                                                                               
XZ30     CLC   DOSTIDNM,PREVREP                                                 
         BE    XZ40                                                             
         OC    DOSTIDNM,DOSTIDNM                                                
         BZ    XZ40                                                             
         MVC   PREVREP,DOSTIDNM                                                 
         GOTO1 SHWIDNUM,DMCB,PREVREP,FAKEFLDH                                   
         MVC   DISREP,FAKEFLD                                                   
         OC    PREVCON,PREVCON                                                  
         BZ    XZ35                                                             
         MVC   DISRCON,PREVCON                                                  
         XC    PREVCON,PREVCON                                                  
*                                                                               
XZ35     CLI   DOSTSTAT,DFXDLVD           IS IT FAX?                            
         BNE   XZ40                                                             
         CLC   DOSTIDNM,=X'FFFF'          ANY ID TO DISPLAY?                    
         BE    XZ40                                                             
         CLC   =C'FAX',ORDDEST                                                  
         BE    XZ37                                                             
         CLC   =C'EMAIL',ORDDEST                                                
         BNE   XZ40                                                             
XZ37     CLC   ORDFDST,SPACES      WE ALREADY SHOWED MOST RECENT?               
         BH    XZ40                                                             
         GOTO1 SHWIDNUM,DMCB,DOSTIDNM,ORDFDSTH   DISPLAY THE REP                
         OI    ORDFDSTH+6,X'80'                                                 
*                                                                               
XZ40     TM    ORDIND2,ORD2NOTM           DON'T SHOW TIME?                      
         BZ    *+8                                                              
         OI    MISCFLG1,MF1NOTIM                                                
*                                                                               
         TM    ORDIND2,ORD2HIGH    HIGHLIGHT THE LINE?                          
         BZ    *+8                                                              
         MVI   DISEVNTH+1,X'28'    HIGHLIGHT THE LINE                           
*                                                                               
         TM    ORDIND2,ORD2TIME           SHOULD WE ONLY SHOW TIME?             
         BNZ   XZSHWTIM                                                         
         NI    MISCFLG1,X'FF'-MF1NOTIM    TURN IT OFF                           
*                                                                               
         CLI   ORDTYP,0                   CONFIRMED/REJECTED/AMENDED?           
         BE    XZSHWIT                    NO                                    
*                                                                               
         TM    ORDTYP,ORDTAMND                                                  
         BZ    XZ40A10                                                          
         TM    MISCFLG1,MF1AMEND                                                
         BO    XZSHWIT                                                          
*                                                                               
XZ40A10  TM    ORDTYP,ORDTRJCT                                                  
         BZ    XZ41                                                             
         TM    MISCFLG1,MF1AMEND                                                
         BZ    XZSHWIT                                                          
*                                                                               
XZ41     TM    ORDTYP,ORDTBCNF            IS IT AN BYRCNFM?                     
         BNO   XZ42                       NO                                    
         TM    MISCFLG1,MF1NOSTA          HAVE WE SHOWN A STATUS?               
         BZ    XZ45                       YES                                   
         CLI   TOTALREV,MAXREVN                                                 
         BL    XZ45                                                             
         ZIC   R1,TOTALREV                                                      
         AHI   R1,-7                                                            
         STC   R1,REVNLSS7                                                      
         B     XZ45                       YES                                   
*                                                                               
XZ42     TM    ORDTYP,ORDAUTCF     AUTO CONFIRM?                                
         BZ    XZ43                                                             
         TM    MISCFLG1,MF1AUTCF                                                
         BZ    XZNXTSTT                                                         
         B     XZ45                                                             
*                                                                               
XZ43     TM    ORDTYP,ORDTCMTS            YES:TEST WITH COMMENTS                
         BNO   XZ43A10                                                          
         TM    MISCFLG1,MF1CNFCM          ARE THERE COMMENTS ?                  
         BO    XZ45                                                             
*                                                                               
XZ43A10  TM    ORDTYP,ORDTNCMT            TEST WITH NO COMMENTS                 
         BNO   XZNXTSTT                                                         
         TM    MISCFLG1,MF1CNFCM          ARE THERE COMMENTS ?                  
         BO    XZNXTSTT                                                         
*                                                                               
XZ45     CLI   REVISION,0                                                       
         BNH   XZSHWIT                                                          
         TM    MISCFLG1,MF1NOSTA                                                
         BNZ   XZSHWIT                                                          
         TM    MISCFLG1,MF1BFCNF   WAS IT PREV BYRCNFM?                         
         BNZ   XZSHWIT             YES!! DON'T DEC REVISION                     
         ZIC   R0,REVISION                                                      
         AHI   R0,-1                                                            
         STC   R0,REVISION                                                      
         CLI   REVISION,0                                                       
         BNE   XZ50                                                             
         XC    DISREV,DISREV                                                    
         NI    MISCFLG2,X'FF'-MF2REVOR                                          
         B     XZSHWIT                                                          
XZ50     EDIT  (B1,REVISION),(3,DISREV),FILL=0                                  
         B     XZSHWIT                                                          
*                                                                               
* ONLY SHOW THE TIME IN THE DLVD COLUMN                                         
*                                                                               
XZSHWTIM TM    PROGFLG1,PF1HSTRY   SHOWING HISTORY?                             
         BO    XZST20              YES, SKIP IT                                 
         TM    MISCFLG1,MF1NOTIM                                                
         BNZ   XZST20                                                           
         GOTO1 SHOWTIME,DMCB,DOSTTIME,DISDLVD                                   
         CLC   PREVJUDT,DOSTDATE                                                
         BE    XZST20                                                           
         GOTO1 DATCON,DMCB,(8,DOSTDATE),(8,DISDATE)                             
         MVC   PREVJUDT,DOSTDATE                                                
XZST20   NI    MISCFLG1,X'FF'-MF1NOSTA    WE'VE SHOWN A STATUS                  
         B     XZNXTELM                                                         
*                                                                               
XZNXTSTT ZIC   R0,ORDLN                                                         
         AR    R3,R0                                                            
         CLI   ORDSTAT,X'FF'              ERROR, STATUS NOT FOUND?              
         BNE   XZ25                       NO: CHECK NEXT STATUS                 
*                                                                               
XZSHWIT  MVC   DISEVNT,ORDDFLT            NOT A DELIVER NOTICE                  
         XR    R0,R0                                                            
         ICM   R0,1,ORDNFLG               R0=NUMBER OF CNTLS TO TEST            
         BZ    XZ70                                                             
         LA    R4,ORDDATA                                                       
         USING ORDDATA,R4                                                       
         SR    R1,R1                                                            
XZ55     IC    R1,ORDFLG                                                        
         EX    R1,*+8                                                           
         BO    XZ60                                                             
         TM    MISCFLG2,0                                                       
         LA    R4,L'ORDDATA(R4)                                                 
         BCT   R0,XZ55                                                          
         B     XZ70                                                             
*                                                                               
XZ60     MVC   DISEVNT,ORDCODE                                                  
         DROP  R4                                                               
*                                                                               
XZ70     CLI   DOSTSTAT,QCFMD      IS IT CONFIRMED?                             
         BNE   XZ75                                                             
*                                                                               
         LA    R1,ORDHTP1                                                       
XZ72     OC    0(L'ORDHTP1,R1),0(R1)                                            
         BNZ   XZ73                                                             
         MVC   0(1,R1),REVISION                                                 
         CLI   REVISION,0                                                       
         BNE   *+8                                                              
         MVI   0(R1),X'FF'                                                      
         MVI   ORDURLS,C'Y'         WE HAVE URLS                                
         B     XZ74                                                             
XZ73     AHI   R1,ORDHTP2H-ORDHTP1H BUMP TO NEXT HTP FIELD                      
         LA    R0,ORDHTP6          CAN WE FIT ANOTHER?                          
         CR    R1,R0                                                            
         BNH   XZ72                MIGHT BE ABLE TO                             
*                                                                               
XZ74     TM    MISCFLG2,MF2VCONF   DID I JUST DISPLAY VARCNFM?                  
         BNZ   *+8                 YES: DON'T DISPLAY IT AGAIN.                 
         NI    MISCFLG2,X'FF'-MF2VAROR  NO: DON'T DISPLAY VARSENT AGAIN         
         NI    MISCFLG2,X'FF'-MF2VCONF  DON'T DISPLAY VARCNFM                   
*                                                                               
XZ75     NI    MISCFLG1,X'FF'-MF1NOSTA    WE'VE SHOWN A STATUS                  
         TM    PROGFLG1,PF1HSTRY                                                
         BO    XZ80                                                             
         GOTO1 SHOWTIME,DMCB,DOSTTIME,DISTIME                                   
         CLC   PREVJUDT,DOSTDATE                                                
         BE    XZ100                                                            
         MVC   PREVJUDT,DOSTDATE                                                
XZ80     GOTO1 DATCON,DMCB,(8,DOSTDATE),(8,DISDATE)                             
*                                                                               
* BYRCNFM ROUTINE TO DISPLAY COMMENTS                                           
XZ100    CLI   DOSTSTAT,QBYRCNFM   ONLY BYRCNFM HAS COMMENTS TO DISPLAY         
         BNE   XZ160                                                            
         CLI   REVISION,0          DISPLAY REV=0 COMMENTS                       
         BE    XZ110                                                            
         CLC   REVISION,REVNLSS7   DON'T SHOW THIS COMMENT?                     
         BNH   XZ160               YES                                          
XZ110    ST    R6,ACURELEM                                                      
         ST    R2,ACURLINE                                                      
*                                                                               
XZ115    BAS   RE,DRLCHK           DO WE HAVE SPACE FOR COMMENTS?               
         BE    XZ125               YES                                          
XZ120    ZIC   R0,REVISION                                                      
         AHI   R0,1                                                             
         STC   R0,REVISION                                                      
XZ122    L     R1,ACURELEM         NO, SHOW ON NEXT PAGE                        
         L     R6,AIO                                                           
         SR    R1,R6                                                            
         STH   R1,ELEMDISP                                                      
         MVC   RECLENB4,DORLEN-DOKEY(R6)                                        
         L     R2,ACURLINE                                                      
         TWAXC (R2),ORDEVTLH,PROT=Y CLEAR LINES AND EXIT                        
         B     DRXIT                                                            
*                                                                               
XZ125    MVC   AIO,AIO3                                                         
         TM    MISCFLG3,MF3RDCOM   HAVE WE READ THE BYRCNFM RECORD?             
         BO    XZ135               YES                                          
         MVC   KEY,SAVEKEY         RESTORE KEY FIRST                            
         MVI   KEY+DOKCMT-DOKEY,X'03' BYRCNFM COMMENT RECORD                    
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         OI    MISCFLG3,MF3RDCOM                                                
         CLC   KEY(L'DOKEY),KEYSAVE                                             
         BNE   XZ155               SHOULD BE THERE BUT NOT WORTH DYING          
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
*                                                                               
XZ135    L     R6,AIO                                                           
         LA    R6,DORFRST-DOKEY(R6)                                             
         USING DOCM2ELD,R6                                                      
XZ140    CLI   0(R6),0             END OF RECORD?                               
         BE    XZ155               YES, EXIT                                    
         CLC   DOCM2REV,REVISION                                                
         BE    XZ145                                                            
         ZIC   R0,1(R6)                                                         
         AR    R6,R0                                                            
         B     XZ140                                                            
*                                                                               
XZ145    ZIC   R1,1(R6)                                                         
         AHI   R1,-5                                                            
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   DISEVNT(0),DOCM2TXT                                              
         MVI   DISEVNTH+1,X'28'    HIGHLIGHT THE LINE                           
         CLI   DOCM2LIN,2          IS THIS THE SECOND(LAST) COMMENT?            
         BE    XZ155               YES, THEN WE'RE DONE                         
         ZIC   R1,1(R6)                                                         
         AR    R6,R1                                                            
         CLI   0(R6),0             END OF RECORD?                               
         BE    XZ155                                                            
         CLC   DOCM2REV,REVISION   IS THERE A SECOND LINE?                      
         BNE   XZ155               NO, THEN WE'RE DONE                          
         BAS   RE,DRLCHK           YES, DO WE HAVE ROOM?                        
         BE    XZ145                                                            
*                                                                               
         MVC   AIO,AIO1            RESTORE AIO BEFORE RETURN                    
         B     XZ120                                                            
*                                                                               
XZ155    MVC   AIO,AIO1                                                         
         L     R6,ACURELEM                                                      
XZ156    OI    MISCFLG1,MF1BFCNF   ORDER WAS PREV BYRCNFM!!                     
         B     XZ165                                                            
*                                                                               
XZ160    TM    MISCFLG1,MF1AUTCF   AUTO-CONFIRMED?                              
         BZ    XZ164                                                            
         NI    MISCFLG1,X'FF'-MF1AUTCF                                          
         B     XZ156                                                            
*                                                                               
XZ164    NI    MISCFLG1,X'FF'-MF1BFCNF  ORDER NOT BUYER CONFIRM                 
XZ165    BAS   RE,DRLCHK                                                        
         BNE   DRXIT                                                            
*                                                                               
XZNXTELM ZIC   R0,1(R6)                                                         
         AR    R6,R0                                                            
         CLI   0(R6),0                                                          
         BE    XZ190                                                            
         CLI   0(R6),DORPELQ2                                                   
         BE    DR70                                                             
         CLI   0(R6),DOSTELQ                                                    
         BE    XZ15                                                             
         CLI   0(R6),DOSAPELQ                                                   
         BE    DR75                                                             
         B     XZNXTELM                                                         
*                                                                               
XZ190    TM    PROGFLG1,PF1HSTRY                                                
         BZ    XZ200                                                            
         L     R2,APRVLINE                                                      
         MVC   DISSALP,SAVESALP                                                 
         MVI   DISPPER,C'N'                                                     
         TM    PROGFLG1,PF1PPER                                                 
         BZ    *+8                                                              
         MVI   DISPPER,C'Y'                                                     
         LA    R2,DISNEXTL                                                      
XZ200    XC    ELEMDISP,ELEMDISP   NOWHERE TO START FROM LATER                  
         NI    MISCFLG1,X'FF'-MF1NOTIM-MF1AMEND                                 
         NI    MISCFLG2,X'FF'-MF2VAROR-MF2VCONF-MF2REVOR                        
*                                                                               
*****                                                                           
*   NOTE:  ALL OLD ORDERS WILL NOT HAVE A CREATOR ID NUMBER                     
*          THE CREATOR WILL EITHER SHOW ON THE NEXT AVAILABLE LINE              
*          OR THE LINE ABOVE THE PFKEY LINE (ORDCRTR) ON EVERY SCREEN           
*                                   08/01/02   MHC                              
*****                                                                           
DRXIT    OC    SAVECRTR,SAVECRTR   IS THERE A CREATOR ID NUM?                   
         BZ    DRXITXX              NOPE, OLD ORDER, SKIP DISPLAY               
*                                                                               
         CLC   2(2,R2),ORDCRTRH+2  ARE WE AFTER THE CREATOR LINE?               
         BNH   DRXIT1               NO                                          
         LA    R2,ORDCRTRH                                                      
*                                                                               
DRXIT1   MVC   DISEVNT(14),=C'* CREATED BY *'                                   
         GOTO1 SHWIDNUM,DMCB,SAVECRTR,FAKEFLDH   DISPLAY THE CREATOR            
         MVC   DISORGID,FAKEFLD                                                 
         OI    6(R2),X'80'         TRANSMIT                                     
         OI    1(R2),X'08'         HIGHLIGHT IT                                 
*                                                                               
DRXITXX  XC    PREVID,PREVID       CLEAR OUT PREVID FOR THE NEW SCREEN          
*                                                                               
         NI    PROGFLG1,X'FF'-PF1WSHTP                                          
         CLI   ORDURLS,C'Y'        DO WE NEED URLS?                             
         BNE   DRXITXXX            NO, WASN'T SET IN XZ11 NOR XZ13 LBL          
         BRAS  RE,GETURLS                                                       
*                                                                               
DRXITXXX BAS   RE,CHKORERR         SEE IF ORDER IS IN ERROR                     
         BNE   XIT                 NONERR DISPLAYED - ACTION COMPLETED          
         B     OURMESGE            SHOW OUR MESSAGE                             
***********************************                                             
* CHECK IF ANY MORE DISPLAY LINES LEFT                                          
***********************************                                             
DRLCHK   ST    R2,APRVLINE         STORE ADDRESS OF PREVIOUS LINE               
         LA    R2,DISNEXTL         R2 = A(NEXT DISPLAY LINE)                    
         LA    R0,ORDEVTLH         WENT BEYOND THE LAST LINE?                   
         CR    R2,R0                                                            
         BNH   DRLCYES             NO, CAN'T CONTINUE                           
*                                                                               
         ZIC   R0,1(R6)                                                         
         AR    R6,R0                                                            
*                                                                               
         CLI   0(R6),DOSTELQ       ANY MORE X'12' ELEMS?                        
         BE    DRLCHK10                                                         
         CLI   0(R6),DORPELQ2      ANY MORE X'09' REP CHANGE ELEMS?             
         BE    DRLCHK10                                                         
         CLI   0(R6),DOSAPELQ      SALESPERSON ELEMS                            
         BE    DRLCHK10                                                         
         XC    ELEMDISP,ELEMDISP                                                
         B     DRLCNO                                                           
*                                                                               
DRLCHK10 LR    R1,R6               CALCULATE WHICH ELEMENT TO CONTINUE          
         L     R0,AIO                  FROM NEXT TRANSACTION                    
         SR    R1,R0                                                            
         STH   R1,ELEMDISP                                                      
         L     R1,AIO                                                           
         MVC   RECLENB4,DORLEN-DOKEY(R1)                                        
         B     DRLCNO                                                           
*                                                                               
DRLCYES  SR    R0,R0                                                            
DRLCNO   LTR   R0,R0                                                            
         BR    RE                                                               
         DROP  R2,R6                                                            
         EJECT                                                                  
***********************************************************************         
* DISPLAYS THE ID NAME BASED ON AN ID NUMBER                                    
*                                                                               
* ON ENTRY:    PARAM 1             A(ID NUMBER)                                 
*              PARAM 2             A(FIELD HEADER OF USER ID)                   
*                                                                               
* ON EXIT:     'KEY' & 'KEYSAVE' GOT CLOBBERED                                  
*              AIO2 WAS USED                                                    
*                                                                               
* NOTE: ELCODE GETS CLOBBERED                                                   
***********************************************************************         
SHWIDNUM NTR1                                                                   
         L     R3,0(R1)            SET UP VARIABLES                             
         L     R2,4(R1)                                                         
*                                                                               
         CLC   =X'FFFF',0(R3)      TEST FAX                                     
         BNE   SIDN1                                                            
         MVC   8(3,R2),=C'FAX'                                                  
         OI    6(R2),X'80'                                                      
         B     SIDNX                                                            
*                                                                               
SIDN1    CLC   =X'FFFD',0(R3)      TEST EMAIL                                   
         BNE   SIDN2                                                            
         MVC   8(5,R2),=C'EMAIL'                                                
         OI    6(R2),X'80'                                                      
         B     SIDNX                                                            
*                                                                               
SIDN2    XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING CTIKEY,R4                                                        
         MVI   CTIKTYP,CTIKTYPQ                                                 
         MVC   CTIKNUM,0(R3)                                                    
         DROP  R4                                                               
*                                                                               
         GOTO1 DATAMGR,DMCB,(0,=C'DMREAD'),=C'CTFILE',(R4),AIO2                 
         CLI   8(R1),0                                                          
         BNE   SIDNX                                                            
*                                                                               
         L     R6,AIO2                                                          
         USING CTIREC,R6                                                        
         LA    R6,CTIDATA                                                       
         MVI   ELCODE,X'02'        LOOK FOR THE DESCRIPTION ELEMENT             
         BAS   RE,NEXTEL                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         USING CTDSCD,R6           COPY USER ID TO SCREEN                       
         ZIC   R1,CTDSCLEN                                                      
         SH    R1,=Y(CTDSC-CTDSCD+1)                                            
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   8(0,R2),CTDSC                                                    
         DROP  R6                                                               
         OI    6(R2),X'80'         TRANSMIT THE FIELD                           
*                                                                               
SIDNX    B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* DISPLAY THE TIME IN THE SPECIFIED DISPLAY FIELD                               
*                                                                               
* ON ENTRY:    PARAM 1             A(2 BYTE TIME)                               
*              PARAM 2             A(TIME FIELD)                                
***********************************************************************         
SHOWTIME NTR1                                                                   
         L     R2,0(R1)                                                         
         L     R3,4(R1)                                                         
*                                                                               
         GOTO1 HEXOUT,DMCB,0(R2),WORK,L'DOXMTTIM                                
         MVC   0(2,R3),WORK                                                     
         MVI   2(R3),C'.'                                                       
         MVC   3(2,R3),WORK+2                                                   
STIMX    B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* CHECKS TO SEE IF THE ORDER HAS ANY ERRORS                                     
***********************************************************************         
CHKORERR NTR1                                                                   
         L     R6,AIO              POINT TO THE RECORD                          
         USING DOKEY,R6                                                         
         CLI   DOKCMT,0            STILL LOOKING AT DARE ORDER RECORD?          
         BE    CKOER10             YES, NO NEED TO FETCH IT                     
*                                                                               
         XC    KEY,KEY             NO, GET THE DARE ORDER RECORD                
         MVC   KEY(L'DOKEY),0(R6)                                               
         LA    R6,KEY                                                           
         MVI   DOKCMT,0                                                         
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 READ                                                             
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
         L     R6,AIO                                                           
         DROP  R6                                                               
*                                                                               
CKOER10  MVI   ELCODE,DOCOMELQ     GET DARE COMMENT ELEMENT                     
         BAS   RE,GETEL                                                         
         BNE   CKOERNO                                                          
*                                                                               
         USING DOCOMELD,R6                                                      
         ZIC   R1,DOCOMLEN                                                      
         SHI   R1,DOCOMOVH-1                                                    
         CHI   R1,4                MORE THAN 3 BYTES?                           
         BNH   CKOER15             NO, OLD SPDARERROR                           
         XC    CONHEAD,CONHEAD                                                  
         SHI   R1,3                YES, NEW STYLE AND NO 3 BYTE ERR #           
         CHI   R1,L'CONHEAD-1      MAKE SURE WE DON'T KILL THIS FIELD           
         BNH   *+8                                                              
         LHI   R1,L'CONHEAD-1                                                   
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   BLOCK+1(0),DOCOMTXT+3                                            
         AHI   R1,1                                                             
         STC   R1,BLOCK                                                         
         B     CKOERYES                                                         
*                                                                               
CKOER15  STC   R1,BLOCK                L(MESSAGE WITH TERMINATING 0)            
         LA    RE,BLOCK(R1)                                                     
         MVI   0(RE),0                 TERMINATING 0                            
         SH    R1,=H'2'                                                         
         EX    R1,*+8                                                           
         B     *+10                WE CAN SHOW OUR MESSAGE NOW                  
         MVC   BLOCK+1(0),DOCOMTXT     MESSAGE                                  
*                                                                               
         L     RE,AERRLST          FIND OUR ERROR                               
         USING ERRDSECT,RE                                                      
         SR    R0,R0                                                            
CKOER20  CLI   ERRLNGTH,0          END OF ERROR LIST?                           
         BE    CKOERYES            ERROR NUMBER NOT IN LIST                     
*                                                                               
         CLC   ERRNUMBR,BLOCK+1    MATCH ON THIS ERROR NUMBER?                  
         BE    CKOER30                                                          
         IC    R0,0(RE)            NO, CHECK NEXT ENTRY                         
         AR    RE,R0                                                            
         B     CKOER20                                                          
*                                                                               
CKOER30  ZIC   R1,ERRLNGTH                                                      
         SH    R1,=Y(ERRTEXT-ERRDSECT-1)                                        
         STC   R1,BLOCK                L(MESSAGE WITH TERMINATING 0)            
         LA    RF,BLOCK(R1)                                                     
         MVI   0(RF),0                 TERMINATING 0                            
         SH    R1,=H'2'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   BLOCK+1(0),ERRTEXT                                               
*                                                                               
CKOERYES MVC   CONHED2(28),=CL28'** Order came back with the '                  
         MVC   CONHED2+28(14),=CL14'above error **'                             
         B     YES                                                              
*                                                                               
CKOERNO  B     NO                                                               
         EJECT                                                                  
*=====================================================================*         
* READ THE ORDER RECORD/DELETE CURRENT COLOR PTR/ADD NEW COLOR PTR              
* SVSTAT HAS STATUS OF CURRENT NOTICE REC IN IO1                                
* ELEM CONTAINS STATUS ELEMENT FROM MAKEGOOD NOTICE RECORD                      
*=====================================================================*         
         SPACE 1                                                                
BLDCOLOR NTR1                                                                   
*                                                                               
         MVC   SAVEKEY,KEY                                                      
         NI    PROGFLG1,X'FF'-PF1CNFCM                                          
         L     R6,AIO                                                           
         MVI   ELCODE,DOSPELQ                                                   
         BAS   RE,GETEL                                                         
         USING DOSPELD,R6                                                       
         TM    DOSPFLG1,DOSPCFCM          CONFIRM WITH COMMENT?                 
         BZ    *+8                                                              
         OI    PROGFLG1,PF1CNFCM                                                
*                                                                               
BLDCLR5  LA    R1,STATTAB          R1 = A(STATUS TABLE)                         
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,DOSTELQ                                                   
         BAS   RE,GETEL                                                         
         BNE   BLDCLR75                                                         
         USING DOSTELD,R6                                                       
         MVC   SVSTAT,DOSTSTAT     MOVE STATUS                                  
*                                                                               
         CLI   DOSTSTAT,QCFMD      AM I CONFIRMED?                              
         BNE   BLDCLR10                                                         
         CLI   DOSTLEN,DOSTLNQ3    DO I HAVE A TYPE FIELD?                      
         BNE   BLDCLR10            ALREADY SET BEFORE                           
         NI    PROGFLG1,X'FF'-PF1CNFCM                                          
         TM    DOSTTYPE,DCNFMCOM   CONFIRMED WITH COMMENTS?                     
         BZ    BLDCLR10                                                         
         OI    PROGFLG1,PF1CNFCM                                                
         DROP  R6                                                               
*                                                                               
         USING ORDD1,R1                                                         
BLDCLR10 CLC   SVSTAT,OR1STAT                                                   
         BNE   BLDCLR15                                                         
*                                                                               
         CLI   OR1TYP,0            IS TYPE BIT SET?                             
         BE    BLDCLR20                                                         
*                                                                               
         CLI   POMAUCFM,C'A'       PROFILE SET TO AUTO-CONFIRM?                 
         BE    BLDCLR12                                                         
         CLI   POMAUCFM,C'B'       PROFILE SET TO BUYER-CONFIRM?                
         BNE   BLDCLR20            ALL CONFIRMS ARE BLACK!!!                    
*                                   CHOOSE FIRST CNFM ENTRY IN TABLE            
BLDCLR12 TM    OR1TYP,OR1TCMTS            TEST WITH COMMENTS                    
         BNO   *+12                                                             
         TM    PROGFLG1,PF1CNFCM          ARE THERE COMMENTS ?                  
         BO    BLDCLR20                   YES, GOT IT                           
         TM    OR1TYP,OR1TNCMT            TEST WITH NO COMMENTS                 
         BNO   BLDCLR15                                                         
         TM    PROGFLG1,PF1CNFCM          ARE THERE COMMENTS ?                  
         BZ    BLDCLR20                   NO, GOT IT                            
*                                                                               
BLDCLR15 LA    R1,L'STATTAB(R1)                                                 
         CLI   0(R1),X'FF'                                                      
         BNE   BLDCLR10                                                         
         DC    H'0'                                                             
*                                                                               
BLDCLR20 MVC   BYTE,1(R1)          SAVE CORRESPONDING COLOR                     
*                                                                               
BLDCLR40 CLI   BYTE,C'G'           TEST IF CURRENT CHANGE GREEN                 
         BE    BLDCLR75            YES - GREEN ALWAYS WINS                      
*                                                                               
         L     R6,AIO              FIND MG GROUP COLOR ELEMENTS                 
         USING DOKEY,R6                                                         
         MVI   ELCODE,MGCOLELQ                                                  
*                                                                               
BLDCLR55 BAS   RE,GETEL                                                         
         BNE   BLDCLR75                                                         
         USING MGCOLEL,R6                                                       
         CLI   MGCOLCOL,C'G'                                                    
         BNE   BLDCLR60                                                         
         MVI   BYTE,C'G'           SET TO GREEN AND DONE                        
         B     BLDCLR75                                                         
*                                                                               
BLDCLR60 CLI   MGCOLCOL,C'R'                                                    
         BNE   BLDCLR55                                                         
         MVI   BYTE,C'R'                                                        
         B     BLDCLR55                                                         
         DROP  R6                                                               
*                                                                               
BLDCLR75 L     R6,AIO              FIND COLOR ELEMENT IN ORDER REC              
         LA    R6,DORFRST-DOKEY(R6)                                             
         USING DOIDELD,R6                                                       
         MVC   QBUYER,DOIDBYR      SAVE THE BUYER CODE                          
         DROP  R6                                                               
*                                                                               
         L     R6,AIO              FIND COLOR ELEMENT IN ORDER REC              
         USING DOKEY,R6                                                         
         MVI   ELCODE,COLELQ                                                    
         BAS   RE,GETEL                                                         
         BNE   BLDCLR80                                                         
*                                                                               
K        USING DSCKTYPE,KEY                                                     
         USING COLOREL,R6                                                       
*                                                                               
         XC    KEY,KEY             READ AND DELETE OLD COLOR POINTER            
         MVI   K.DSCKTYPE,DSCKTYPQ                                              
         MVI   K.DSCKSTYP,DSCKSTYQ                                              
         MVC   K.DSCKAGMD,BAGYMD                                                
         MVC   K.DSCKBYR,QBUYER                                                 
         OI    K.DSCKBYR+2,C' '                                                 
         MVC   K.DSCKSTAT,COLCOL                                                
         MVC   K.DSCKDATE,COLDATE                                               
         MVC   K.DSCKORDR,BINORDER                                              
         MVC   KEY+14(4),DISKADDR                                               
         DROP  K                                                                
         DROP  R6                                                               
*                                                                               
         MVI   DMINBTS,X'80'       READ FOR DETELED                             
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BNE   BLDCLR80                  CONVERSION NOT YET RUN !               
         OI    KEY+13,X'80'        DELETE OLD                                   
         GOTO1 WRITE                                                            
*                                                                               
         L     R6,AIO              RESTORE KEY BECAUSE A DELETE CLOBBER         
         USING DOKEY,R6                WHAT IS IN AIO ALSO!!                    
         MVC   DOKEY,SAVEKEY                                                    
*                                                                               
Y        USING COLOREL,ELEM+128                                                 
BLDCLR80 XC    ELEM+128(128),ELEM+128    BUILD NEW COLOR ELEMENT                
         MVI   Y.COLEL,COLELQ                                                   
         MVI   Y.COLELLEN,COLLENQ                                               
         MVC   Y.COLCOL,BYTE                                                    
*                                                                               
         GOTO1 DATCON,DMCB,(5,0),(2,Y.COLDATE)                                  
         XC    Y.COLDATE,=X'FFFF'       COMPLEMENT DATE                         
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,COLELQ                                                    
         BAS   RE,GETEL                                                         
         BNE   BLDCLR85                                                         
*                                                                               
BLDCLR82 GOTO1 RECUP,DMCB,(C'S',AIO),(R6) DELETE OLD COLOR ELEM                 
*                                                                               
BLDCLR85 GOTO1 RECUP,DMCB,(C'S',AIO),ELEM+128,(R6)   ADD NEW                    
*                                                                               
K        USING DSCKTYPE,KEY                                                     
BLDCLR87 XC    KEY,KEY             CREATE NEW COLOR POINTER                     
         MVI   K.DSCKTYPE,DSCKTYPQ                                              
         MVI   K.DSCKSTYP,DSCKSTYQ                                              
         MVC   K.DSCKAGMD,BAGYMD                                                
         MVC   K.DSCKBYR,QBUYER                                                 
         OI    K.DSCKBYR+2,C' '                                                 
         MVC   K.DSCKSTAT,Y.COLCOL                                              
         MVC   K.DSCKDATE,Y.COLDATE                                             
         MVC   K.DSCKORDR,BINORDER                                              
         MVC   KEY+14(4),DISKADDR  MOVE SAVED DISK ADDRESS                      
         DROP  K,Y                                                              
*                                                                               
         MVI   DMINBTS,X'88'       SET TO PASS DELETES                          
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BNE   BLDCLR90                                                         
         NI    KEY+13,X'7F'        UNDELETE                                     
         GOTO1 WRITE                                                            
         B     BLDCLRX                                                          
*                                                                               
BLDCLR90 MVC   KEY,KEYSAVE         RESTORE                                      
         GOTO1 ADD                                                              
*                                                                               
BLDCLRX  L     R6,AIO              RESTORE KEY BECAUSE A WRITE CLOBBER          
         USING DOKEY,R6                WHAT IS IN AIO                           
         MVC   DOKEY,SAVEKEY                                                    
         B     YES                                                              
         DROP  R6                                                               
*                                                                               
         EJECT                                                                  
***********************************************************************         
* INSERT THE SALESPERSON HISTORY                                                
*                                                                               
*  ON ENTRY:  AIO1   A(ORDER RECORD)                                            
*                                                                               
*  ON EXIT :  AIO1   A(ORDER RECORD WITH SALESPERSON ELEMS INSERTED)            
*             AIO2   CLOBBERED!                                                 
***********************************************************************         
INSSALPR NTR1                                                                   
         L     R6,AIO1                                                          
         XC    KEY,KEY                                                          
         MVC   KEY(L'DOKEY),0(R6)                                               
         OI    KEY+DOKCMT-DOKEY,X'04'    SALESPERSON REASSIGNMENT REC           
         GOTO1 HIGH                                                             
*                                                                               
         CLC   KEY(L'DOKEY),KEYSAVE  DID I FIND IT?                             
         BNE   ISPX                NO, EXIT                                     
*                                                                               
         MVC   AIO,AIO2                                                         
         L     RE,AIO                                                           
         LHI   RF,LIOS                                                          
         XCEFL                                                                  
*                                                                               
         GOTO1 GETREC              YES, GET THE RECORD                          
         MVC   AIO,AIO1                                                         
*                                                                               
         L     R4,AIO2             R4 = A(SALESPERSON ELEMS)                    
         LA    R4,24(R4)                                                        
ISP10    L     R6,AIO1                                                          
         MVI   ELCODE,DOSTELQ                                                   
         BAS   RE,GETEL            R6 = A(X'12' DOSTEL ELEMS)                   
*                                                                               
ISP20    CLI   0(R4),0             ANY MORE SALESPERSON ELEMS?                  
         BE    ISPX                NO, EXIT                                     
         CLI   0(R4),DOSAPELQ                                                   
         BE    ISP30                                                            
         DC    H'0'                                                             
*                                                                               
ISP30    CLC   2(DOSTLNQ-2,R4),2(R6)  DO I MATCH?                               
         BE    ISP50               YES, INSERT HERE!!                           
ISP35    ZIC   R1,1(R6)            NO, BUMP TO NEXT DOSTELEM                    
         AR    R6,R1                                                            
         CLI   0(R6),0             END OF RECORD?                               
         BNE   ISP40               NO                                           
         ZIC   R1,1(R4)            YES, NO MATCH WAS FOUND                      
         AR    R4,R1               SKIP THIS SALESPERSON ELEM                   
         B     ISP10                                                            
*                                                                               
ISP40    CLI   0(R6),DOSTELQ       ANOTHER DOSTELEM?                            
         BE    ISP30               YES, COMPARE IT                              
         B     ISP35               NO, BUMP                                     
*                                                                               
ISP50    ZIC   R1,1(R6)                                                         
         AR    R6,R1                                                            
         GOTO1 RECUP,DMCB,(C'S',AIO),(R4),(R6)                                  
         ZIC   R1,1(R6)                                                         
         AR    R6,R1                                                            
         ZIC   R1,1(R4)                                                         
         AR    R4,R1                                                            
         B     ISP20                                                            
*                                                                               
ISPX     B     XIT                                                              
***********************************************************************         
* REBUILD FULL STATUS HISTORY OF THIS ORDER                                     
***********************************************************************         
REBLDORD NTR1                                                                   
*                                                                               
         LHI   R2,-1               SET R2 TO NEGATIVE                           
         L     R3,AIO                                                           
         LA    R3,24(R3)           R3=A(ADDRESS CURRENT ELEM)                   
RBORD06  CLI   0(R3),0             DID WE HIT END OF RECORD?                    
         BE    RBORD10             YES, NO MORE                                 
         CLI   0(R3),DORPELQ2                                                   
         BE    RBORD10                                                          
         CLI   0(R3),DORPELQ                                                    
         BE    RBORD08                                                          
         CLI   0(R3),DOXMTELQ                                                   
         BL    RBORD09                                                          
         BH    RBORD10     <-- NO MORE DORPELEM/DOXMELEM!! EXIT LOOP!           
RBORD08  CLR   R3,R2               FOUND A DORPELEM/DOXMELEM                    
         BNL   RBORD09             WAS IT THE FIRST ONE?                        
         LR    R2,R3               YES! R2=A(FIRST DORPELEM/DOXMELEM)           
RBORD09  ZIC   R0,1(R3)            BUMP TO NEXT ELEM                            
         AR    R3,R0                                                            
         B     RBORD06                                                          
RBORD10  LR    R6,R3               R6=A(INSERTION)                              
         ST    R6,CURINSRT                                                      
         XC    NEWINSRT,NEWINSRT                                                
*                                                                               
         LTR   R2,R2               IF R2<0, NOTHING TO CONVERT                  
         BM    RBORDNO             DO NOTHING, EXIT                             
*                                                                               
         LR    R3,R2               R3=A(CURRENT DORPELEM/DOXMELEM)              
RBORD12  CLI   0(R3),0             FINISHED?                                    
         BE    RBORDYES            YES, WRITE TO RECORD!!                       
         CLI   0(R3),DORPELQ       CONVERT DORPELEM?                            
         BE    RBORD15                                                          
         CLI   0(R3),DOXMTELQ      CONVERT DOXMELEM?                            
         BE    RBORD20                                                          
         ZIC   R0,1(R3)                                                         
         AR    R3,R0                                                            
         B     RBORD12                                                          
*                                                                               
* ADD THE NEW REP CHANGE ELEMENT                                                
*                                                                               
RBORD15  LA    R4,ELEM                                                          
         USING DOREPELD,R4                                                      
         ZIC   RE,1(R3)                                                         
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R4),0(R3)                                                    
         MVI   DORPEL,DORPELQ2                                                  
         DROP  R4                                                               
*                                                                               
         BRAS  RE,ELEMCHEK         SHOULD I ADD THIS ELEMENT?                   
         BE    RBORD16             YES                                          
         L     R6,NEWINSRT                                                      
         ST    R6,CURINSRT                                                      
         B     RBORDNXT                                                         
*                                                                               
RBORD16  GOTO1 RECUP,DMCB,(C'S',AIO),ELEM,(R6)                                  
         ZIC   R0,1(R6)                                                         
         AR    R6,R0               BUMP R6 TO POINT TO NEXT LOCATION            
         ST    R6,CURINSRT                                                      
         B     RBORDNXT                                                         
*                                                                               
* ADD THE NEW STATUS ELEMENT                                                    
*                                                                               
         USING DOXMTELD,R3                                                      
RBORD20  LA    R4,ELEM                                                          
         USING DOSTELD,R4                                                       
*                                                                               
         NI    PROGFLG1,X'FF'-PF1RCLDT                                          
*                                                                               
         CLI   DOXMTSTA,QSNTPNDG                                                
         BE    RBORD70                                                          
*                                                                               
         LA    R2,RCLTABLE                                                      
RBORD30  CLI   0(R2),X'FF'                                                      
         BE    RBORD40                                                          
         CLC   DOXMTSTA,0(R2)                                                   
         BE    *+12                                                             
         AHI   R2,1                                                             
         B     RBORD30                                                          
*                                                                               
         CLC   DOXMTSTD(5),DOXMTDND                                             
         BH    RBORD40                                                          
         OI    PROGFLG1,PF1RCLDT                                                
         B     RBORD60                                                          
*                                                                               
RBORD40  OC    DOXMTSTD,DOXMTSTD   DO I HAVE A STATUS DATE?                     
         BNZ   RBORD45             YES!!                                        
         CLI   DOXMTSTA,0          NO, BUT DO I HAVE A STATUS?                  
         BNE   RBORDNO               YES, BAD RECORD!! DMXKEEP!!!               
         B     RBORD60                                                          
*                                                                               
RBORD45  XC    ELEM,ELEM           CLEAR ELEM                                   
         MVI   DOSTEL,DOSTELQ                                                   
         MVI   DOSTLEN,DOSTLNQ                                                  
         MVC   DOSTDATE,DOXMTSTD                                                
         MVC   DOSTTIME,DOXMTSTT                                                
         MVC   DOSTSTAT,DOXMTSTA                                                
*                                                                               
         CLI   DOXMTSTA,QFAXDLVD                                                
         BNE   RBORD50                                                          
         MVI   DOSTLEN,DOSTLNQ2    EXTENDED AUDIT ELEMENT                       
         MVI   DOSTSTAT,DFXDLVD    NEW FAX DELIVERED STATUS                     
         OC    DOXMTDNT,DOXMTDNT   DO WE HAVE A REP?                            
         BZ    *+10                                                             
         MVC   DOSTIDNM,DOXMTDNT                                                
*                                                                               
RBORD50  DS    0H                                                               
         CLI   DOXMTSTA,DEMDLVD                                                 
         BNE   RBORD50A                                                         
         MVI   DOSTLEN,DOSTLNQ2    EXTENDED AUDIT ELEMENT                       
         MVI   DOSTSTAT,DEMDLVD    NEW FAX DELIVERED STATUS                     
         OC    DOXMTDNT,DOXMTDNT   DO WE HAVE A REP?                            
         BZ    *+10                                                             
         MVC   DOSTIDNM,DOXMTDNT                                                
*                                                                               
RBORD50A DS    0H                                                               
         BRAS  RE,ELEMCHEK         SHOULD I ADD THIS ELEMENT?                   
         BE    RBORD51             YES                                          
         L     R6,NEWINSRT                                                      
         B     RBORD55                                                          
*                                                                               
RBORD51  GOTO1 RECUP,DMCB,(C'S',AIO),ELEM,(R6)                                  
         ZIC   R0,1(R6)                                                         
         AR    R6,R0               BUMP R6 TO POINT TO NEXT LOCATION            
*                                                                               
RBORD55  TM    PROGFLG1,PF1RCLDT                                                
         BNZ   RBORD70                                                          
*                                                                               
RBORD60  OC    DOXMTDND,DOXMTDND   DELIVERED STATUS?                            
         BZ    RBORD70                                                          
         XC    ELEM,ELEM           CLEAR ELEM                                   
         MVI   DOSTEL,DOSTELQ                                                   
         MVI   DOSTLEN,DOSTLNQ2    EXTENDED AUDIT ELEMENT                       
         MVC   DOSTDATE,DOXMTDND                                                
         MVC   DOSTTIME,DOXMTDNT                                                
         MVI   DOSTSTAT,DDLVRD                                                  
         MVC   DOSTIDNM,DOXMTDID                                                
*                                                                               
         BRAS  RE,ELEMCHEK         SHOULD I ADD THIS ELEMENT?                   
         BE    RBORD61             YES                                          
         L     R6,NEWINSRT                                                      
         B     RBORD69                                                          
*                                                                               
RBORD61  GOTO1 RECUP,DMCB,(C'S',AIO),ELEM,(R6)                                  
         ZIC   R0,1(R6)                                                         
         AR    R6,R0               BUMP R6 TO POINT TO NEXT LOCATION            
*                                                                               
RBORD69  TM    PROGFLG1,PF1RCLDT                                                
         BNZ   RBORD40                                                          
*                                                                               
RBORD70  DS    0H                                                               
         XC    ELEM,ELEM           CLEAR ELEM                                   
         MVI   DOSTEL,DOSTELQ                                                   
         MVI   DOSTLEN,DOSTLNQ2    EXTENDED AUDIT ELEMENT                       
         MVC   DOSTDATE,DOXMTYMD                                                
         MVC   DOSTTIME,DOXMTTIM                                                
*                                                                               
         CLI   DOXMTSTA,QSNTPNDG                                                
         BE    RBORD80                                                          
         CLI   DOXMTSTA,QSNTXCNF                                                
         BE    RBORD80                                                          
         CLI   DOXMTSTA,QSNTXREJ                                                
         BE    RBORD80                                                          
         CLI   DOXMTSTA,QTOBESNT                                                
         BNE   RBORD90                                                          
*                                                                               
RBORD80  MVI   DOSTLEN,DOSTLNQ                                                  
         MVI   DOSTSTAT,QSNTPNDG                                                
         B     RBORD100                                                         
*                                                                               
RBORD90  MVI   DOSTSTAT,DSENT                                                   
         MVC   DOSTIDNM,DOXMTOID                                                
*                                                                               
         CLC   DOXMTDID,=X'FFFF'   FAX SENT STATUS?                             
         BNE   RBORD99                                                          
         MVI   DOSTSTAT,DFXSENT    CAN'T REBUILD A FAX RESENT                   
*                                                                               
RBORD99  CLC   DOXMTDID,=X'FFFD'   FAX SENT STATUS?                             
         BNE   RBORD100                                                         
         MVI   DOSTSTAT,DEMSENT                                                 
*                                                                               
RBORD100 DS    0H                                                               
*                                                                               
         BRAS  RE,ELEMCHEK         SHOULD I ADD THIS ELEMENT?                   
         BE    RBORD101            YES                                          
         L     R6,NEWINSRT                                                      
         CLI   DOSTSTAT,QSNTPNDG                                                
         BE    RBORDNXT                                                         
         ST    R6,CURINSRT                                                      
         B     RBORDNXT                                                         
*                                                                               
RBORD101 GOTO1 RECUP,DMCB,(C'S',AIO),ELEM,(R6)                                  
         ZIC   R0,1(R6)                                                         
         AR    R6,R0               BUMP R6 TO POINT TO NEXT LOCATION            
         CLI   DOSTSTAT,QSNTPNDG                                                
         BE    RBORDNXT                                                         
         ST    R6,CURINSRT                                                      
         DROP  R3                                                               
*                                                                               
RBORDNXT ZIC   R0,1(R3)                                                         
         AR    R3,R0                                                            
         B     RBORD12                                                          
*                                                                               
RBORDYES BAS   RE,BLDCOLOR                                                      
         GOTO1 PUTREC                                                           
*                                                                               
RBORDNO  B    NO                                                                
*-------------------------------------------------------------------            
         EJECT                                                                  
***********************************************************************         
* GENERAL ERROR MESSAGES                                                        
***********************************************************************         
MISSFLD  MVI   GERROR1,MISSING                                                  
         B     ERREXIT                                                          
*                                                                               
INVLFLD  MVI   GERROR1,INVALID                                                  
         B     ERREXIT                                                          
*                                                                               
RECNTFND MVI   GERROR1,NOTFOUND    RECORD NOT FOUND                             
         B     ERREXIT                                                          
***********************************************************************         
* GENERAL INFO MESSAGES                                                         
***********************************************************************         
NEEDFLDS MVI   GERROR1,REQFIELD    PLEASE ENTER FIELDS AS REQUIRED              
         B     INFEXIT                                                          
***********************************************************************         
* INFO MESSAGES (SYSTEM 23)                                                     
***********************************************************************         
OURMESGE MVI   GERROR1,INFOMESS    &1                                           
         B     INFRTEXT                                                         
***********************************************************************         
* MESSAGE ROUTINES                                                              
***********************************************************************         
INFEXIT  MVI   GETMSYS,255         GENERAL MESSAGES                             
MYINFXIT MVI   GMSGTYPE,C'I'       INFO MESSAGES                                
ERREXIT  GOTO1 MYERR                                                            
*                                                                               
INFRTEXT LA    R1,MYINFXIT                                                      
         B     *+8                                                              
ERRRTEXT LA    R1,ERREXIT                                                       
         OI    GENSTAT2,USGETTXT                                                
         XC    GETTXTCB,GETTXTCB                                                
         LA    RF,GETTXTCB                                                      
         USING GETTXTD,RF                                                       
         MVI   GTMSYS,23                                                        
         LA    RE,BLOCK                                                         
         STCM  RE,7,GTASUBST                                                    
         DROP  RF                                                               
         BR    R1                                                               
*                                                                               
NOTHING  DC    H'0'                                                             
*                                                                               
HEDLINE1 DC    CL60'     Origin ID  Date     Time  Dlvd  Rev Rep       X        
                Contract'                                                       
HEDLINE2 DC    CL35'Salesperson          Point(Y/N)'                            
HEDLINE3 DC    CL60'Date     Dest/Route  Fax#           Spots  Dollars X        
                  PID'                                                          
*                                                                               
         GETEL R6,DATADISP,ELCODE  USED FOR THE GETEL OPERATIONS                
         EJECT                                                                  
YES      SR    RC,RC                                                            
NO       LTR   RC,RC                                                            
XIT      XIT1                                                                   
         LTORG                                                                  
         SPACE 2                                                                
RELO     DS    A                                                                
         SPACE 2                                                                
SPCTABLE DS    0H                                                               
         DC    AL1(QSNTXCNF)                                                    
         DC    AL1(QSNTXREJ)                                                    
         DC    AL1(QTOBESNT)                                                    
RCLTABLE DS    0H                                                               
         DC    AL1(QRECALL)                                                     
         DC    AL1(QRCLAPPR)                                                    
         DC    AL1(QRCLCONF)                                                    
         DC    AL1(QRCLDELN)                                                    
         DC    AL1(QRCLREJD)                                                    
         DC    AL1(QRCLUNKN)                                                    
         DC    AL1(QRCLTRNS)                                                    
         DC    AL1(QRCLWIP)                                                     
         DC    X'FF'                                                            
*                                                                               
STATTAB  DS    0XL3                                                             
         DC    AL1(DSENT),C'G',AL1(0)                                           
         DC    AL1(DFXSENT),C'G',AL1(0)                                         
         DC    AL1(DFXRSNT),C'G',AL1(0)                                         
         DC    AL1(DEMSENT),C'G',AL1(0)                                         
         DC    AL1(QRJCT),C'G',AL1(0)                                           
         DC    AL1(QEMPTY),C'G',AL1(0)                                          
         DC    AL1(QERRORED),C'G',AL1(0)                                        
         DC    AL1(QFAXCNCL),C'G',AL1(0)                                        
         DC    AL1(QRCLAPPR),C'G',AL1(0)                                        
         DC    AL1(QRCLDELN),C'G',AL1(0)                                        
         DC    AL1(QRCLUNKN),C'G',AL1(0)                                        
         DC    AL1(QRCLTRNS),C'G',AL1(0)                                        
         DC    AL1(QRCLWIP),C'G',AL1(0)                                         
         DC    AL1(QSNTPNDG),C'G',AL1(0)                                        
         DC    AL1(QSNTXCNF),C'G',AL1(0)                                        
         DC    AL1(QSNTXREJ),C'G',AL1(0)                                        
         DC    AL1(QTOBESNT),C'G',AL1(0)                                        
*                                                                               
         DC    AL1(QAPP),C'R',AL1(0)                                            
         DC    AL1(QRECALL),C'R',AL1(0)                                         
         DC    AL1(QRCLCONF),C'R',AL1(0)                                        
         DC    AL1(QRCLREJD),C'R',AL1(0)                                        
         DC    AL1(DDLVRD),C'R',AL1(0)                                          
         DC    AL1(QFAXDLVD),C'R',AL1(0)                                        
         DC    AL1(DFXDLVD),C'R',AL1(0)                                         
         DC    AL1(DEMDLVD),C'R',AL1(0)                                         
*                                                                               
* DON'T CHANGE ORDER OF QCFMD, I HAVE CODE DEPENDING ON THIS ORDER!!            
         DC    AL1(QCFMD),C'K',AL1(ORDTNCMT) CONFIRMED W/O COMMENTS             
         DC    AL1(QCFMD),C'R',AL1(ORDTCMTS) CONFIRMED WITH COMMENTS            
*                                                                               
         DC    AL1(QBYRCNFM),C'K',AL1(0)                                        
         DC    AL1(QCFMDPND),C'K',AL1(0)                                        
         DC    AL1(QNODARE),C'K',AL1(0)                                         
         DC    AL1(QUNDARE),C'K',AL1(0)                                         
         DC    X'FF'                                                            
***********************************************************************         
* DISPLAY PFKEY TABLE DEFINITIONS                                               
***********************************************************************         
ORDSTAB  DS    0H                                                               
*                                                                               
ORDSENT  DC    AL1(DSENT,ORDSENTX-*+1),CL20'SENT'                               
         DC    AL1(0,0,0)                                                       
         DC    AL2(0)                                                           
         DC    AL1((ORDSENTX-*)/L'ORDDATA)                                      
         DC    AL1(MF2REVOR),CL20'REVISION SENT'                                
         DC    AL1(MF2VAROR),CL20'VAR ORDER SENT'                               
ORDSENTX EQU   *                                                                
*                                                                               
ORDDLVD  DC    AL1(DDLVRD,ORDDLVDX-*+1),CL20'DELIVERED'                         
         DC    AL1(0,0,ORD2TIME+ORD2CKID)                                       
         DC    AL2(0)                                                           
         DC    AL1((ORDDLVDX-*)/L'ORDDATA)                                      
ORDDLVDX EQU   *                                                                
*                                                                               
ORDFAX   DC    AL1(DFXSENT,ORDFAXX-*+1),CL20'FAX SENT'                          
         DC    AL1(0,0,0)                                                       
         DC    AL2(0)                                                           
         DC    AL1((ORDFAXX-*)/L'ORDDATA)                                       
ORDFAXX  EQU   *                                                                
*                                                                               
ORDFAXRS DC    AL1(DFXRSNT,ORDFXRSX-*+1),CL20'FAX RESENT'                       
         DC    AL1(0,0,0)                                                       
         DC    AL2(0)                                                           
         DC    AL1((ORDFXRSX-*)/L'ORDDATA)                                      
ORDFXRSX EQU   *                                                                
*                                                                               
ORDFXDEL DC    AL1(DFXDLVD,ORDFXDEX-*+1),CL20'FAX DELIVERED'                    
         DC    AL1(0,0,ORD2TIME+ORD2CKID)                                       
         DC    AL2(0)                                                           
         DC    AL1((ORDFXDEX-*)/L'ORDDATA)                                      
ORDFXDEX EQU   *                                                                
*                                                                               
ORDBYCFM DC    AL1(QBYRCNFM,ORDBYCFX-*+1),CL20'BUYER CONFIRMED'                 
         DC    AL1(ORDTBCNF,0,0)                                                
         DC    AL2(0)                                                           
         DC    AL1((ORDBYCFX-*)/L'ORDDATA)                                      
ORDBYCFX EQU   *                                                                
*                                                                               
ORDFXCAN DC    AL1(QFAXCNCL,ORDFXCAX-*+1),CL20'FAX CANCELLED'                   
         DC    AL1(0,0,0)                                                       
         DC    AL2(0)                                                           
         DC    AL1((ORDFXCAX-*)/L'ORDDATA)                                      
ORDFXCAX EQU   *                                                                
*                                                                               
ORDEMSN  DC    AL1(DEMSENT,ORDEMSNX-*+1),CL20'EMAIL SENT'                       
         DC    AL1(0,0,0)                                                       
         DC    AL2(0)                                                           
         DC    AL1((ORDEMSNX-*)/L'ORDDATA)                                      
ORDEMSNX EQU   *                                                                
*                                                                               
ORDEMDEL DC    AL1(DEMDLVD,ORDEMDEX-*+1),CL20'EMAIL DELIVERED'                  
         DC    AL1(0,0,ORD2TIME+ORD2CKID)                                       
         DC    AL2(0)                                                           
         DC    AL1((ORDEMDEX-*)/L'ORDDATA)                                      
ORDEMDEX EQU   *                                                                
*                                                                               
ORDAPP   DC    AL1(QAPP,ORDAPPX-*+1),CL20'OPENED'                               
         DC    AL1(0,0,0)                                                       
         DC    AL2(0)                                                           
         DC    AL1((ORDAPPX-*)/L'ORDDATA)                                       
         DC    AL1(MF2REVOR),CL20'REVISION OPENED'                              
         DC    AL1(MF2VAROR),CL20'VAR ORDER OPENED'                             
ORDAPPX  EQU   *                                                                
*                                                                               
ORDCFMPD DC    AL1(QCFMDPND,ORDCFMPX-*+1),CL20'CONFIRM PENDING'                 
         DC    AL1(0,0,0)                                                       
         DC    AL2(0)                                                           
         DC    AL1((ORDCFMPX-*)/L'ORDDATA)                                      
ORDCFMPX EQU   *                                                                
*                                                                               
ORDCFM1  DC    AL1(QCFMD,ORDCFM1X-*+1),CL20'AUTO CONFIRMED'                     
         DC    AL1(ORDAUTCF,0,0)                                                
         DC    AL2(0)                                                           
         DC    AL1((ORDCFM1X-*)/L'ORDDATA)                                      
         DC    AL1(MF2REVOR),CL20'REV AUTO CONFIRMED'                           
ORDCFM1X EQU   *                                                                
*                                                                               
ORDCFM   DC    AL1(QCFMD,ORDCFMX-*+1),CL20'CONFIRMED'                           
         DC    AL1(ORDTNCMT,0,0)                                                
         DC    AL2(0)                                                           
         DC    AL1((ORDCFMX-*)/L'ORDDATA)                                       
         DC    AL1(MF2REVOR),CL20'REVISION CONFIRMED'                           
         DC    AL1(MF2VCONF),CL20'VAR ORDER CONFIRMED'                          
ORDCFMX  EQU   *                                                                
*                                                                               
ORDPCFM  DC    AL1(QCFMD,ORDPCFMX-*+1),CL20'PARTIAL CONFIRM'                    
         DC    AL1(ORDTCMTS,0,0)                                                
         DC    AL2(0)                                                           
         DC    AL1((ORDPCFMX-*)/L'ORDDATA)                                      
         DC    AL1(MF2REVOR),CL20'REV PARTIAL CONFIRM'                          
         DC    AL1(MF2VAROR),CL20'VAR PARTIAL CONFIRM'                          
ORDPCFMX EQU   *                                                                
*                                                                               
ORDERR   DC    AL1(QERRORED,ORDERRX-*+1),CL20'ERROR'                            
         DC    AL1(0,0,ORD2HIGH)                                                
         DC    AL2(0)                                                           
         DC    AL1((ORDERRX-*)/L'ORDDATA)                                       
         DC    AL1(MF2FAXED),CL20'FAXED ERROR'                                  
         DC    AL1(MF2EMLED),CL20'EMAILED ERROR'                                
ORDERRX  EQU   *                                                                
*                                                                               
ORDRJCT  DC    AL1(QRJCT,ORDRJCTX-*+1),CL20'REJECTED'                           
         DC    AL1(ORDTRJCT,0,0)                                                
         DC    AL2(0)                                                           
         DC    AL1((ORDRJCTX-*)/L'ORDDATA)                                      
         DC    AL1(MF2REVOR),CL20'REVISION REJECTED'                            
         DC    AL1(MF2VAROR),CL20'VAR ORDER REJECTED'                           
ORDRJCTX EQU   *                                                                
*                                                                               
ORDAMND  DC    AL1(QRJCT,ORDAMNDX-*+1),CL20'AMENDED'                            
         DC    AL1(ORDTAMND,0,0)                                                
         DC    AL2(0)                                                           
         DC    AL1((ORDAMNDX-*)/L'ORDDATA)                                      
         DC    AL1(MF2REVOR),CL20'REVISION AMENDED'                             
         DC    AL1(MF2VAROR),CL20'VAR ORDER AMENDED'                            
ORDAMNDX EQU   *                                                                
*                                                                               
ORDEMPTY DC    AL1(QEMPTY,ORDEMPTX-*+1),CL20'EMPTY'                             
         DC    AL1(0,0,0)                                                       
         DC    AL2(0)                                                           
         DC    AL1((ORDEMPTX-*)/L'ORDDATA)                                      
ORDEMPTX EQU   *                                                                
*                                                                               
ORDNODAR DC    AL1(QNODARE,ORDNODAX-*+1),CL20'NOT DARED'                        
         DC    AL1(0,0,0)                                                       
         DC    AL2(0)                                                           
         DC    AL1((ORDNODAX-*)/L'ORDDATA)                                      
ORDNODAX EQU   *                                                                
*                                                                               
ORDRECAL DC    AL1(QRECALL,ORDRECAX-*+1),CL20'RECALL'                           
         DC    AL1(0,0,0)                                                       
         DC    AL2(0)                                                           
         DC    AL1((ORDRECAX-*)/L'ORDDATA)                                      
         DC    AL1(MF2REVOR),CL20'REVISION RECALL'                              
         DC    AL1(MF2VAROR),CL20'VAR ORDER RECALL'                             
ORDRECAX EQU   *                                                                
*                                                                               
ORDRCAPP DC    AL1(QRCLAPPR,ORDRCAPX-*+1),CL20'RECALLED, REP OPENED'            
         DC    AL1(0,0,0)                                                       
         DC    AL2(0)                                                           
         DC    AL1((ORDRCAPX-*)/L'ORDDATA)                                      
ORDRCAPX EQU   *                                                                
*                                                                               
ORDRCCFM DC    AL1(QRCLCONF,ORDRCCFX-*+1),CL20'NOT RECALLED, CNFRMD'            
         DC    AL1(0,0,0)                                                       
         DC    AL2(0)                                                           
         DC    AL1((ORDRCCFX-*)/L'ORDDATA)                                      
ORDRCCFX EQU   *                                                                
*                                                                               
ORDRCDEL DC    AL1(QRCLDELN,ORDRCDEX-*+1),CL20'RECALLED, REP DELVRD'            
         DC    AL1(0,0,0)                                                       
         DC    AL2(0)                                                           
         DC    AL1((ORDRCDEX-*)/L'ORDDATA)                                      
ORDRCDEX EQU   *                                                                
*                                                                               
ORDRCRJT DC    AL1(QRCLREJD,ORDRCRJX-*+1),CL20'NOT RECALLED, RJCTED'            
         DC    AL1(0,0,0)                                                       
         DC    AL2(0)                                                           
         DC    AL1((ORDRCRJX-*)/L'ORDDATA)                                      
ORDRCRJX EQU   *                                                                
*                                                                               
ORDRCUK  DC    AL1(QRCLUNKN,ORDRCUKX-*+1),CL20'NOT RECALLED, UNKNWN'            
         DC    AL1(0,0,0)                                                       
         DC    AL2(0)                                                           
         DC    AL1((ORDRCUKX-*)/L'ORDDATA)                                      
ORDRCUKX EQU   *                                                                
*                                                                               
ORDRCTRN DC    AL1(QRCLTRNS,ORDRCTRX-*+1),CL20'RECALLED, REP XMITED'            
         DC    AL1(0,0,0)                                                       
         DC    AL2(0)                                                           
         DC    AL1((ORDRCTRX-*)/L'ORDDATA)                                      
ORDRCTRX EQU   *                                                                
*                                                                               
ORDRCWP  DC    AL1(QRCLWIP,ORDRCWPX-*+1),CL20'RECALLED, WIP'                    
         DC    AL1(0,0,0)                                                       
         DC    AL2(0)                                                           
         DC    AL1((ORDRCWPX-*)/L'ORDDATA)                                      
ORDRCWPX EQU   *                                                                
*                                                                               
ORDUNDAR DC    AL1(QUNDARE,ORDUNDAX-*+1),CL20'UNDARED'                          
         DC    AL1(0,0,0)                                                       
         DC    AL2(0)                                                           
         DC    AL1((ORDUNDAX-*)/L'ORDDATA)                                      
ORDUNDAX EQU   *                                                                
*                                                                               
ORDSNTP  DC    AL1(QSNTPNDG,ORDSNTPX-*+1),CL20'SEND PENDING'                    
         DC    AL1(0,0,0)                                                       
         DC    AL2(0)                                                           
         DC    AL1((ORDSNTPX-*)/L'ORDDATA)                                      
ORDSNTPX EQU   *                                                                
*                                                                               
ORDSNTC  DC    AL1(QSNTXCNF,ORDSNTCX-*+1),CL20'SEND CANCELLED, PCFM'            
         DC    AL1(0,0,ORD2NOTM)                                                
         DC    AL2(0)                                                           
         DC    AL1((ORDSNTCX-*)/L'ORDDATA)                                      
ORDSNTCX EQU   *                                                                
*                                                                               
ORDSNTR  DC    AL1(QSNTXREJ,ORDSNTRX-*+1),CL20'SEND CANCELLED, RJCT'            
         DC    AL1(0,0,ORD2NOTM)                                                
         DC    AL2(0)                                                           
         DC    AL1((ORDSNTRX-*)/L'ORDDATA)                                      
ORDSNTRX EQU   *                                                                
*                                                                               
ORD2BSN  DC    AL1(QTOBESNT,ORD2BSNX-*+1),CL20'TO BE SENT'                      
         DC    AL1(0,0,ORD2NOTM)                                                
         DC    AL2(0)                                                           
         DC    AL1((ORD2BSNX-*)/L'ORDDATA)                                      
ORD2BSNX EQU   *                                                                
*                                                                               
ORDEND   DC    AL1(255,ORDENDX-*+1),CL20'ERROR'                                 
         DC    AL1(0,0,0)                                                       
         DC    AL2(0)                                                           
         DC    AL1((ORDENDX-*)/L'ORDDATA)                                       
ORDENDX  EQU   *                                                                
*                                                                               
         EJECT                                                                  
***********************************************************************         
* THIS WILL CLEAR EVERY LINE FROM ORDEVT1H TO ORDEVTLH                          
***********************************************************************         
MYTWAXC  NTR1  BASE=*,LABEL=*                                                   
*   TWAXC ORDEVT1H,ORDEVTLH,PROT=Y                                              
         XR    RE,RE               ** MODIFIED TWAXC TO VALIDATE FIELD          
         LA    R1,ORDEVT1H                                                      
         LA    RF,ORDEVTLH                                                      
MTXC10   IC    RE,0(R1)                                                         
         AHI   RE,-9                                                            
         TM    1(R1),X'02'                                                      
         BZ    *+8                                                              
         AHI   RE,-8                                                            
         LTR   RE,RE                                                            
         BM    MTXCX                                                            
         EX    RE,*+8                                                           
         B     *+10                                                             
         XC    8(0,R1),8(R1)                                                    
         NI    1(R1),X'FF'-X'08'   <----  THIS IS THE ADDITIONAL INSTR          
         OI    6(R1),X'80'                                                      
         IC    RE,0(R1)                                                         
         BXLE  R1,RE,MTXC10        ** MODIFIED TWAXC TO VALIDATE FIELD          
MTXCX    J     XIT                                                              
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* FETCHS THE URLS FROM THE URL TYPE RECORD TO MATCH ONE ON SCREEN               
***********************************************************************         
GETURLS  NTR1  BASE=*,LABEL=*                                                   
         LA    R4,KEY                                                           
         USING DOKEY,R4                                                         
         L     R6,AIO                                                           
         XC    KEY,KEY                                                          
         MVC   DOKEY,0(R6)                                                      
         MVI   DOKCMT,DOKCURLQ     LOOK FOR URL RECORD                          
         DROP  R4                                                               
*                                                                               
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BNE   GTURL10                                                          
*                                                                               
         GOTO1 GETREC                                                           
         L     R6,AIO                                                           
         MVI   ELCODE,DOCM2ELQ     LOOK FOR SPECIFIC REV NUMBERS                
         BAS   RE,GETEL                                                         
         BNE   GTURL10                                                          
         USING DOCM2ELD,R6                                                      
*** CHECK REVISION NUMBER AGAINST WHAT REV IN ON THAT LINE                      
GTURL0   LA    R2,ORDHTP1                                                       
GTURL1   CLI   DOCM2REV,0          URL FOR ORIGINAL CONFIRM?                    
         BNE   GTURL2                                                           
         CLI   0(R2),X'FF'         WE COULD USE NULLS IN XZ11                   
         BE    GTURL5                                                           
         B     GTURL3                                                           
GTURL2   CLC   0(1,R2),DOCM2REV    MATCHES OUR REVISION NUMBER?                 
         BE    GTURL5                                                           
GTURL3   AHI   R2,ORDHTP2H-ORDHTP1H                                             
         LA    R0,ORDHTP6                                                       
         CR    R2,R0                                                            
         BNH   GTURL1                                                           
*                                                                               
GTURL4   BAS   RE,NEXTEL           NO HTP LINES MATCH REV # IN URL              
         BNE   GTURL10             GET NEXT URL IF ANY                          
         B     GTURL0                                                           
*                                                                               
GTURL5   XR    R1,R1                                                            
         IC    R1,DOCM2LEN                                                      
         SHI   R1,DOCM2OVH+1       R1 = # CHARACTERS FOR THE URL                
         EX    R1,*+8                                                           
         B     GTURL4                                                           
         MVC   0(0,R2),DOCM2TXT    MOVE IT INTO MY NOP LINE                     
*                                                                               
GTURL10  LA    R2,ORDHTP1                                                       
GTURL15  CLI   1(R2),0             ANY URL ON THIS LINE?                        
         BNE   *+8                                                              
         MVI   0(R2),0             NO, CLEAR REVISION # AS WELL                 
         AHI   R2,ORDHTP2H-ORDHTP1H                                             
         LA    R0,ORDHTP6                                                       
         CR    R2,R0                                                            
         BNH   GTURL15                                                          
*                                                                               
GTURLX   J     XIT                                                              
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* SHOWS THE URLS ON THE DETAIL LINES FROM THE URL FIELDS                        
***********************************************************************         
SHOWURLS NTR1  BASE=*,LABEL=*                                                   
         BRAS  RE,MYTWAXC                                                       
*                                                                               
         LA    R2,ORDEVT1              WE'LI TAKE 2 DISPLAY LINES FOR           
         LA    RE,ORDHTP1                 EACH URL LINE                         
*                                                                               
SWURL10  OC    0(L'ORDHTP1,RE),0(RE)   FLDH+5 NOT SET, NOP FIELD                
         BZ    SWURL50                 GOTO NEXT URL                            
         MVC   0(L'ORDEVT1,R2),0(RE)                                            
         MVC   ORDEVT2H-ORDEVT1H(L'ORDEVT1,R2),L'ORDEVT1(RE)                    
*                                                                               
SWURL50  AHI   R2,ORDEVT3H-ORDEVT1H    BUMP TO NEXT 2 DISPLAY LINES             
         AHI   RE,ORDHTP2H-ORDHTP1H    BUMP TO NEXT 2 DISPLAY LINES             
*                                                                               
         LA    R0,ORDHTP6              PAST THE LAST LINE?                      
         CR    RE,R0                                                            
         BNH   SWURL10                 NO, MIGHT HAVE MORE TO DISPLAY           
*                                                                               
SWURLX   J     XIT                                                              
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* SHOWS THE EXTRA DATA                                                          
***********************************************************************         
SHWEXTRA NTR1  BASE=*,LABEL=*                                                   
         BRAS  RE,MYTWAXC                                                       
*                                                                               
         L     R6,AIO                                                           
         CLC   KEY(L'DOKEY),0(R6)                                               
         BNE   SHWEXTX                                                          
*                                                                               
         OC    ELEMDISP,ELEMDISP   DID WE COME BACK?                            
         BZ    *+12                NO                                           
         AH    R6,ELEMDISP         START WHERE WE LEFT OFF                      
         B     *+8                                                              
         LA    R6,24(R6)           START AT BEGINNING                           
*                                                                               
         LA    R2,ORDEVT1H                                                      
         USING DISLINED,R2                                                      
*                                                                               
SWEXT05  CLI   0(R6),0                                                          
         BE    SHWEXT60                                                         
*                                                                               
         CLI   0(R6),X'12'                                                      
         BE    *+6                                                              
         DC    H'0'                                                             
         USING DOSTELD,R6                                                       
         NI    MISCFLG2,X'FF'-MF2REVOR                                          
         CLI   DOSTREVN,0          IS THIS A REVISION?                          
         BE    *+8                                                              
         OI    MISCFLG2,MF2REVOR                                                
*                                                                               
         LA    R3,ORDSTAB                                                       
         USING ORDD,R3                                                          
         SR    R0,R0                                                            
SWEXT10  CLC   DOSTSTAT,ORDSTAT           MATCH STATUS?                         
         BE    SWEXT15                    YES                                   
         ZIC   R0,ORDLN                   NO: CHECK NEXT STATUS ENTRY           
         AR    R3,R0                                                            
         CLI   ORDSTAT,X'FF'              ERROR, STATUS NOT FOUND?              
         BNE   SWEXT10                    NO: CHECK NEXT STATUS                 
*                                                                               
SWEXT15  MVC   DISEVNT,ORDDFLT            NOT A DELIVER NOTICE                  
         XR    R0,R0                                                            
         ICM   R0,1,ORDNFLG               R0=NUMBER OF CNTLS TO TEST            
         BZ    SWEXT30                                                          
         LA    R4,ORDDATA                                                       
         USING ORDDATA,R4                                                       
         SR    R1,R1                                                            
SWEXT20  IC    R1,ORDFLG                                                        
         EX    R1,*+8                                                           
         BO    SWEXT25                                                          
         TM    MISCFLG2,0                                                       
         LA    R4,L'ORDDATA(R4)                                                 
         BCT   R0,SWEXT20                                                       
         B     SWEXT30                                                          
SWEXT25  MVC   DISEVNT,ORDCODE                                                  
         DROP  R4,R3                                                            
*                                                                               
SWEXT30  GOTO1 DATCON,DMCB,(8,DOSTDATE),(8,DISEDATE)                            
         MVC   DISEDEST,=C'Rep'                                                 
         CLI   DOSTDEST,C'R'                                                    
         BE    *+10                                                             
         MVC   DISEDEST,=C'Sta'                                                 
         MVI   DISESLSH,C'/'                                                    
         GOTO1 SHWIDNUM,DMCB,DOSTDID,FAKEFLDH                                   
         MVC   DISEMTHD,FAKEFLD                                                 
         MVC   DISEFAXN,DOSTFXN                                                 
         OC    DOSTSPTS,DOSTSPTS                                                
         BZ    SWEXT35                                                          
         EDIT  (B4,DOSTSPTS),(6,DISESPTS),ALIGN=LEFT                            
         OC    DOSTTOTL,DOSTTOTL                                                
         BZ    SWEXT35                                                          
         EDIT  (P6,DOSTTOTL),(10,DISEDLRS),2,ALIGN=LEFT                         
*                                                                               
SWEXT35  OC    DOSTPID,DOSTPID                                                  
         BZ    SWEXT50                                                          
         XC    KEY,KEY                                                          
         MVI   KEY,C'0'                                                         
         MVC   KEY+1(2),AGENCY    SET SECURITY AGENCY                           
         MVC   KEY+23(2),DOSTPID                                                
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'CTFILE',KEY,AIO2                      
*                                                                               
         L     RE,AIO2                                                          
         MVC   DISEPID,=C'????????'   IN CASE WE FIND NOTHING                   
         CLC   KEY(25),0(RE)       DID WE FIND THE RECORD?                      
         BNE   SWEXT50             NO, SKIP                                     
*                                                                               
         LA    RE,28(RE)                                                        
SWEXT40  CLC   =X'C30A',0(RE)      - NEW SECURITY - PERSON ELEMENT              
         BE    SWEXT45                                                          
         SR    R0,R0                                                            
         IC    R0,1(RE)                                                         
         AR    RE,R0                                                            
         CLI   0(RE),0             DID WE FIND IT?                              
         BE    SWEXT50             NO, SKIP                                     
         B     SWEXT40                                                          
*                                                                               
SWEXT45  MVC   DISEPID,2(RE)       DISPLAY SIGN-ON ID                           
*                                                                               
         DROP  R6,R2                                                            
*                                                                               
SWEXT50  BRAS  RE,DRLCHK                                                        
         BNE   SHWEXTX                                                          
         ZIC   R0,1(R6)                                                         
         AR    R6,R0                                                            
         B     SWEXT05                                                          
*                                                                               
SHWEXT60 XC    ELEMDISP,ELEMDISP                                                
SHWEXTX  J     XIT                                                              
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* CHECK TO SEE IF ELEMENT ALREADY EXISTS IN THE RECORD                          
*                                                                               
* ON ENTRY:    ELEM CONTIANS ELEMENT WE WANT TO ADD                             
*              R6 = A(INSERTION)                                                
*              R3 = A(TARGET ELEMENT X'10' OR X'11')                            
*                                                                               
* ON EXIT:     CONDITION CODE: YES IF WE NEED TO ADD IT                         
*                            : NO  IF NOT                                       
*                                                                               
***********************************************************************         
ELEMCHEK NTR1  BASE=*,LABEL=*                                                   
         USING DOXMTELD,R3                                                      
         LA    R4,ELEM                                                          
         USING DOSTELD,R4                                                       
*                                                                               
         SR    R0,R0                                                            
         ZIC   R1,ELEM+1           R1 = LENGTH OF COMPARE                       
         AHI   R1,-3           -1 FOR EX STATEMENT, -2 EXCLUDE TYPE&LEN         
         L     R6,CURINSRT                                                      
*                                                                               
EC10     CLI   0(R6),0                                                          
         BE    ECYES                                                            
*                                                                               
         CLI   0(R6),DOSTELQ                                                    
         BH    ECYES                                                            
*                                                                               
         CLI   0(R4),DOSTELQ        X'12'?                                      
         BNE   EC50                                                             
*                                                                               
         LA    R2,SPCTABLE                                                      
EC20     CLI   0(R2),X'FF'                                                      
         BE    EC40                                                             
         CLC   DOSTSTAT,0(R2)                                                   
         BE    EC30                                                             
         AHI   R2,1                                                             
         B     EC20                                                             
*                                                                               
EC30     CLI   0(R6),0             DID I HIT EOR?                               
         BE    ECYES               YES, THEN ADD                                
         CLI   0(R6),DOSTELQ       NO MORE X'12'?                               
         BH    ECYES               YES, THEN ADD                                
         CLI   DOSTSTAT-DOSTEL(R6),DSENT  DID I GET TO A PREV SENT?             
         BE    ECYES                        YES, THEN ADD                       
         CLC   DOSTSTAT,DOSTSTAT-DOSTEL(R6) DID I FIND IT?                      
         BE    ECNO                         YES, DON'T ADD                      
*                                                                               
         IC    R0,1(R6)                                                         
         AR    R6,R0               BUMP TO NEXT ELEMENT                         
         B     EC30                                                             
*                                                                               
EC40     CLI   DOSTSTAT,DEMDLVD                                                 
         BE    EC42                                                             
         CLI   DOSTSTAT,DFXDLVD                                                 
         BNE   EC50                                                             
EC42     CLC   ELEM+2(DOSTSTAT-DOSTLEN),2(R6)                                   
         BE    ECYESA                                                           
         B     EC60                                                             
         DROP  R3,R4                                                            
*                                                                               
EC50     EX    R1,*+8              DO I NEED TO ADD THIS ELEMENT?               
         BE    ECNO                NO, ELEMENT ALREADY THERE!!                  
         CLC   ELEM+2(0),2(R6)     OTHERWISE, CHECK NEXT ELEMENTS               
*                                                                               
EC60     IC    R0,1(R6)                                                         
         AR    R6,R0               BUMP TO NEXT ELEMENT                         
         B     EC10                                                             
*                                                                               
ECNO     IC    R0,1(R6)                                                         
         AR    R6,R0               BUMP TO NEXT ELEMENT                         
         L     R1,NEWINSRT                                                      
         CR    R6,R1                                                            
         BNH   NO                                                               
         ST    R6,NEWINSRT                                                      
         J     NO                                                               
*                                                                               
ECYESA   GOTO1 RECUP,DMCB,(C'S',AIO),(R6)                                       
*                                                                               
ECYES    J     YES                                                              
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* PATCH IN AN EMAIL DELVD                                                       
***********************************************************************         
PTCHEMDL NTR1  BASE=*,LABEL=*                                                   
         L     R6,AIO                                                           
         MVI   ELCODE,DOXMTELQ                                                  
         BAS   RE,GETEL                                                         
         BNE   PEMDLX                                                           
*                                                                               
         USING DOXMTELD,R6                                                      
         GOTO1 DATCON,DMCB,(5,0),(19,DOXMTSTD)                                  
         XC    DOXMTSTT,DOXMTSTT                                                
         MVI   DOXMTSTA,DEMDLVD    LOWER CASE F                                 
         DROP  R6                                                               
*                                                                               
         OI    PROGFLG1,PF1REBLD   WE NEED TO REBUILD ORDER                     
*                                                                               
PEMDLX   J     XIT                                                              
         LTORG                                                                  
***********************************************************************         
* SET THE PFKEY INFORMATION                                                     
***********************************************************************         
SETPFKYS NTR1  BASE=*,LABEL=*                                                   
         SR    R2,R2               NO PFKEY AT TABLE FIRST                      
         LA    R2,PFTABLE          ACTION STATUS?                               
*                                                                               
         GOTO1 INITIAL,DMCB,(R2)   INITIALIZE THE PFKEYS                        
***************                                                                 
* SETUP THE PFKEY LINE                                                          
***************                                                                 
         MVC   ORDPFLN(14),=CL14'PF7=Up  8=Down'                                
*                                                                               
         CLI   CALLSP,0            ANYTHING TO RETURN TO?                       
         BE    *+10                NO                                           
         MVC   ORDPFLN+16(9),=CL9'12=Return'                                    
*                                                                               
         OI    ORDPFLNH+6,X'80'                                                 
*                                                                               
STPFX    J     XIT                                                              
*                                                                               
PFTABLE  DS    0C                                                               
*                                                                               
* UP                                                                            
         DC    AL1(PF07X-*,07,0,0,0,PFTRETRN)                                   
         DC    CL3' ',CL8' ',CL8' '                                             
PF07X    EQU   *                                                                
*                                                                               
* DOWN                                                                          
         DC    AL1(PF08X-*,08,0,0,0,PFTRETRN)                                   
         DC    CL3' ',CL8' ',CL8' '                                             
PF08X    EQU   *                                                                
*                                                                               
* RETURN TO CALLER                                                              
         DC    AL1(PF12X-*,12,PFTRPROG,0,0,0)                                   
         DC    CL3' ',CL8' ',CL8' '                                             
PF12X    EQU   *                                                                
*                                                                               
         DC    X'FF'                                                            
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*                                                                               
       ++INCLUDE SPOMSERROR                                                     
         EJECT                                                                  
* FAGETTXTD                                                                     
* DDGLOBEQUS                                                                    
* DDCOMFACSD                                                                    
* DMPRTQL                                                                       
* FAFACTS                                                                       
* CTGENFILE                                                                     
* DDSPOOLD                                                                      
* DDSPLWORKD                                                                    
         PRINT OFF                                                              
       ++INCLUDE FAGETTXTD                                                      
       ++INCLUDE DDGLOBEQUS                                                     
       ++INCLUDE DDCOMFACS                                                      
       ++INCLUDE DMPRTQL                                                        
       ++INCLUDE FAFACTS                                                        
       ++INCLUDE CTGENFILE                                                      
       ++INCLUDE DDSPOOLD          (GENERAL PRINT AREAS)                        
       ++INCLUDE DDSPLWORKD        (GENERAL CONTROLLER AREAS)                   
         PRINT ON                                                               
       ++INCLUDE SPOMSFFD          (BASE SCREEN FOR SYSTEM)                     
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE SPOMSD9D          (OUR DISPLAY SCREEN)                         
         EJECT                                                                  
       ++INCLUDE DDGENTWA                                                       
         EJECT                                                                  
       ++INCLUDE SPOMSWORKD        (SYSTEM AREAS)                               
         EJECT                                                                  
       ++INCLUDE SPGENDRORD        (RECORD DSECTS)                              
         EJECT                                                                  
CLTHDRD  DSECT                                                                  
       ++INCLUDE SPGENCLT                                                       
         EJECT                                                                  
***********************************************************************         
* MY STORAGE AREA                                                               
***********************************************************************         
MYAREAD  DSECT                                                                  
ACURLINE DS    F                   A(HEADER OF CURRENT LINE)                    
ACURELEM DS    F                   A(CURRENT ELEMENT)                           
ADOSPELM DS    F                   A(DOSPELEM)                                  
APRVLINE DS    F                   A(HEADER OF PREVIOUS LINE)                   
*                                                                               
RECLENB4 DS    H                   RECORD LENGTH B4 IN THE LAST TRANS           
ELEMDISP DS    H                   ELEMENT DISPLACEMENT (USED FOR DIS)          
HTTPDISP DS    H                   ELEM DISP FOR RETURNING FROM HTTP            
*                                                                               
PREVID   DS    XL2                 PREVIOUS ORIGIN ID NUMBER                    
SAVEREP  DS    XL2                                                              
SAVECRTR DS    XL2                 THE CREATOR FOR THIS ORDER                   
PREVREP  DS    XL2                 PREVIOUS REP ID NUMBER                       
PREVCON  DS    CL8                 PREVIOUS REP CONTRACT NUMBER                 
*                                                                               
PROGFLG1 DS    XL1                 PROGRAM FLAG                                 
PF1REBLD EQU   X'80'                 REBUILD THE ORDER RECORD                   
PF1RCLDT EQU   X'40'                 ORDER HAS A RECALL DELNOT                  
PF1CNFCM EQU   X'20'                 ORDER IS CONFIRMED WITH COMMENTS           
PF1HSTRY EQU   X'10'                 HISTORY OPTION                             
PF1PPER  EQU   X'08'                 POINTPERSON                                
PF1EMDLV EQU   X'04'                 **TEMP **  PATCH IN EMAIL DELVD            
PF1HTTP  EQU   X'02'                 WBUC WANTS HTTP                            
PF1WSHTP EQU   X'01'                    PREVIOUS SCREEN WAS HTP                 
*                                                                               
PROGFLG2 DS    XL1                 PROGRAM FLAG 2                               
PF2EXTRA EQU   X'80'                 DISPLAY EXTRA INFO                         
*                                                                               
MISCFLG1 DS    XL1                 MISCELLANEOUS FLAGS                          
MF1KYCHG EQU   X'80'                 A KEY FIELD HAS BEEN CHANGED               
MF1AMEND EQU   X'40'                                                            
MF1CNFCM EQU   X'10'                 CONFIRMED WITH COMMENTS                    
MF1AUTCF EQU   X'08'                 AUTO CONFIRMED                             
MF1NOSTA EQU   X'04'                 HAVEN'T SHOWN A STATUS YET                 
MF1NOTIM EQU   X'02'                 DO NOT SHOW DELIVERY TIME                  
MF1BFCNF EQU   X'01'                 ORDER WAS BUYER/FULLY CONFIRMED            
HTTPFLG1 DS    XL1                 SAVED MISC FLAGS FOR HTTP                    
*                                                                               
MISCFLG2 DS    XL1                 MISCELLANEOUS FLAGS                          
MF2VAROR EQU   X'80'                 VAR ORDER                                  
MF2VCONF EQU   X'40'                 VAR ORDER IS CONFIRMED                     
MF2REVOR EQU   X'20'                 ORDER HAS BEEN REVISED                     
MF2FAXED EQU   X'08'                 ORDER HAS BEEN PREVIOUSLY FAXED            
MF2EMLED EQU   X'04'                 ORDER HAS BEEN PREVIOUSLY EMAILED          
*                                                                               
MISCFLG3 DS    XL1                 MISCELLANEOUS FLAGS                          
MF3RDCOM EQU   X'80'                 READ THE BYRCNFM COMMENT RECORD            
*                                                                               
MAXREVN  EQU   8                                                                
TOTALREV DS    XL1                 TOTAL REVISIONS                              
REVNLSS7 DS    XL1                 TOTAL REVISION MINUS 7                       
REVISION DS    XL1                 REVISION NUMBER                              
HTTPRVSN DS    XL1                 SAVED REVISION NUMBER                        
PREVJUDT DS    XL3                 PREVIOUS JULIAN DATE (PWOS)                  
*                                                                               
SVSTAT   DS    CL1                                                              
*                                                                               
SAVESALP DS    CL20                                                             
*                                                                               
PROFOM   DS    CL16                OM PROFILE                                   
POMUSEOM EQU   PROFOM               - USES ORDER MANAGER?                       
POMAUCFM EQU   PROFOM+2             - PARITAL CONFIRM WORKFLOW                  
POMMGREP EQU   PROFOM+15            - CREATE MKGD TRANSACTION REPORT?           
*                                                                               
BINORDER DS    0XL4                BINARY ORDER NUMBER                          
BINORDDT DS    XL2                     DATE PORTION                             
BINORDSQ DS    XL2                     SEQUENCE PORTION                         
*                                                                               
CURINSRT DS    F                   CURRENT INSERTION ADDRESS                    
NEWINSRT DS    F                   NEW INSERTION ADDRESS                        
*                                                                               
PACKOF4B DS    PL4                 PACKED OF 4 BYTES                            
*                                                                               
SAVEKEY  DS    XL(L'DOKEY)                                                      
DISKADDR DS    XL4                 DISK ADDRESS                                 
*                                                                               
FAKEFLDH DS    CL8                 FAKE FIELD HEADER AND FIELD DATA             
FAKEFLD  DS    CL60                                                             
*                                                                               
AERRLST  DS    A                                                                
         EJECT                                                                  
***********************************************************************         
* DISPLAY LINE DSECT                                                            
***********************************************************************         
DISLINED DSECT                                                                  
DISEVNTH DS    CL8                                                              
DISEVNT  DS    0CL20                                                            
         DS    CL17                                                             
DISEXTRA DS    0C                                                               
         DS    CL5                                                              
DISORGID DS    CL10                                                             
         DS    CL1                                                              
         ORG   DISEVNT                                                          
DISSALT1 DS    CL15                                                             
         DS    CL1                                                              
DISSALT2 DS    CL8                                                              
         DS    CL9                                                              
DISDATE  DS    CL8                                                              
         DS    CL1                                                              
DISTIME  DS    CL5                                                              
         DS    CL1                                                              
DISDLVD  DS    CL5                                                              
         DS    CL1                                                              
DISREV   DS    CL3                                                              
         DS    CL1                                                              
DISREP   DS    CL10                                                             
         DS    CL1                                                              
DISRCON  DS    CL8                                                              
         DS    CL2                                                              
         ORG   DISTIME                                                          
DISSALP  DS    CL20                                                             
         DS    CL1                                                              
DISPPER  DS    CL1                                                              
         DS    CL15                                                             
         ORG   DISEXTRA                                                         
DISEDATE DS    CL8                                                              
         DS    CL1                                                              
DISEDEST DS    CL3                                                              
DISESLSH DS    C                                                                
DISEMTHD DS    CL7                                                              
         DS    C                                                                
DISEFAXN DS    CL14                                                             
         DS    C                                                                
DISESPTS DS    CL6                                                              
         DS    C                                                                
DISEDLRS DS    CL10                                                             
         DS    C                                                                
DISEPID  DS    CL8                                                              
DISNEXTL DS    0C                                                               
       ++INCLUDE SPOMSDARED                                                     
         EJECT                                                                  
***********************************************************************         
ORDD1    DSECT                                                                  
OR1STAT  DS    XL1                 ORDER OR MG STATUS                           
OR1COL   DS    XL1                 COLOR CODE                                   
OR1TYP   DS    XL1                 SECONDARY TYPE COMPARE                       
OR1TCMTS EQU   X'80'               WITH COMMENTS                                
OR1TNCMT EQU   X'40'               NO COMMENTS                                  
OR1LNQ   EQU   *-ORDD1             LENGTH OF EACH ENTRY                         
***********************************************************************         
* ERROR LIST DSECT                                                              
***********************************************************************         
ERRDSECT DSECT                                                                  
ERRLNGTH DS    XL1                 LENGTH OF ERROR ENTRY                        
ERRNUMBR DS    CL3                 ERROR NUMBER (EBCDIC NUMERIC)                
ERRTEXT  DS    0C                  NUMBER OF SPOTS                              
***********************************************************************         
* ORDER STATUS TABLE DSECT                                                      
***********************************************************************         
ORDD     DSECT                                                                  
ORDSTAT  DS    XL1                 ORDER STATUS                                 
ORDLN    DS    XL1                 LENGTH OF TABLE ENTRY                        
ORDDFLT  DS    CL20                DEFAULT CODE TO DISPLAY                      
ORDTYP   DS    XL1                 SECONDARY TYPE COMPARE                       
ORDTCMTS EQU   X'80'               WITH COMMENTS                                
ORDTNCMT EQU   X'40'               NO COMMENTS                                  
ORDTBCNF EQU   X'20'               BUYER CONFIRM                                
ORDTRJCT EQU   X'10'               REJECTED                                     
ORDTAMND EQU   X'08'               AMENDED                                      
ORDAUTCF EQU   X'04'               AUTO CONFIRMED                               
ORDIND   DS    XL1                 INDICATOR                                    
ORDIND2  DS    XL1                 SECOND INDICATOR                             
ORD2TIME EQU   X'80'               ONLY SHOW TIME                               
ORD2NOTM EQU   X'40'               DO NOT SHOW TIME AFTER THIS                  
ORD2HIGH EQU   X'20'               HIGHLIGHT THE LINE                           
ORD2CKID EQU   X'10'               CHECK IF REP CHANGE                          
         DS    XL2                 N/D                                          
ORDNFLG  DS    XL1                 NUMBER OF CNTLS TO TEST                      
ORDDATA  DS    0XL21                                                            
ORDFLG   DS    XL1                 FLAG                                         
ORDCODE  DS    CL20                CODE                                         
***********************************************************************         
* HEADLINE DSECT                                                                
***********************************************************************         
HEADLIND DSECT                                                                  
         DS    CL8                                                              
HLSUB    DS    0CL42                                                            
         DS    CL17                                                             
HLSUB1   DS    0CL60                                                            
         DS    CL25                                                             
HLSUB2   DS    0CL35                                                            
         DS    CL35                                                             
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'096SPOMS04   03/01/12'                                      
         END                                                                    
