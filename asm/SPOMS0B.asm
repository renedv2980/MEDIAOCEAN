*          DATA SET SPOMS0B    AT LEVEL 219 AS OF 01/03/07                      
*PHASE T2340BA                                                                  
T2340B   TITLE 'SPOMS0B - BUYER CONFIRM'                                        
T2340B   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,*T2340B*,R7,RR=R3                                              
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
         OI    GENSTAT1,RDUPAPPL   DO NOT READ FOR UPDATE                       
         BAS   RE,SETPFKYS         SETUP THE PFKEYS                             
*                                                                               
         GOTO1 DATCON,DMCB,(5,0),(15,JDTTODAY)   GET CURRENT DATE               
         GOTO1 DATCON,DMCB,(5,0),(2,CFFDATE)                                    
         XC    CFFDATE,=2X'FF'                                                  
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
         NI    MISCFLG1,X'FF'-MF1KYCHG-MF1CNFCM                                 
***************                                                                 
* VALIDATE THE MEDIA                                                            
***************                                                                 
VKMED00  DS    0H                                                               
         LA    R2,ORCMEDH                                                       
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
         LA    R2,ORCORDRH                                                      
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
         XC    BINORDDT,=X'FFFF'                                                
*                                                                               
         PACK  DUB,8+4(4,R2)       SAVE SEQUENCE PORTION                        
         CVB   R0,DUB                                                           
         STCM  R0,3,BINORDSQ                                                    
         XC    BINORDSQ,=X'FFFF'                                                
*                                                                               
         OI    4(R2),X'20'         VALIDATE THE FIELD                           
*                                                                               
VKORDX   DS    0H                                                               
         EJECT                                                                  
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
         CLC   KEY(DOKSTA-DOKEY),KEYSAVE                                        
         BNE   RECNTFND                                                         
*                                                                               
         MVC   SAVEKEY,KEY                                                      
VKXIT    B     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
* VALIDATE THE COMMENTS                                                         
***********************************************************************         
DR       DS    0H                                                               
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
*                                                                               
         CLC   KEY(L'DOKEY),KEYSAVE                                             
         BNE   RECNTFND                                                         
         MVC   DISKADDR,KEY+14                                                  
*                                                                               
         MVI   RDUPDATE,C'N'                                                    
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
         MVI   RDUPDATE,C'Y'                                                    
         GOTO1 HIGH                                                             
*                                                                               
         CLC   KEY(L'DOKEY),KEYSAVE                                             
         BNE   RECNTFND                                                         
         MVC   DISKADDR,KEY+14                                                  
*                                                                               
         MVI   RDUPDATE,C'Y'                                                    
         GOTO1 GETREC                                                           
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,DOSPELQ                                                   
         BAS   RE,GETEL                                                         
         BNE   DR10                                                             
         USING DOSPELD,R6                                                       
*                                                                               
         TM    DOSPFLG1,DOSPCFCM   CONFIRMED WITH COMMENTS?                     
         BZ    *+8                                                              
         OI    MISCFLG1,MF1CNFCM                                                
*                                                                               
         MVI   REVISION,0                                                       
         OC    DOSPREVN,DOSPREVN                                                
         BZ    DR10                                                             
         MVC   REVISION,DOSPREVN                                                
         EDIT  (B1,DOSPREVN),(3,ORCREVN),FILL=0                                 
*                                                                               
DR10     LA    R2,ORCCOM1H                                                      
         ZICM  R0,5(R2),1          MUST HAVE ATLEAST 1 COMMENT                  
         BZ    NEEDFLDS                                                         
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,DOIDELQ                                                   
         BAS   RE,GETEL                                                         
         USING DOIDELD,R6                                                       
         MVC   SVBUYER,DOIDBYR                                                  
         MVC   SVSTA,DOISTA                                                     
         MVC   BCLT,DOIDCLT                                                     
         MVC   BPRD,DOIDPRD                                                     
         MVC   BPRD2,DOIDPRD2                                                   
         MVC   BEST,DOIDEST                                                     
         MVC   SVBFLT,DOIDFLTN                                                  
         DROP  R6                                                               
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,DOXMTELQ                                                  
         BAS   RE,GETEL                                                         
         BNE   DR100                                                            
*                                                                               
         USING DOXMTELD,R6                                                      
         CLI   DOXMTSTA,QFAXDLVD                                                
         BE    DR20                                                             
         CLI   DOXMTSTA,DEMDLVD                                                 
         BE    DR20                                                             
         CLI   DOXMTSTA,QCFMD      AM I CONFIRMED!!                             
         BNE   DR100                                                            
         TM    MISCFLG1,MF1CNFCM   WITH COMMENTS?                               
         BZ    DR100                                                            
         CLI   POMAUCFM,C'B'       PROFILE SET TO MANUAL CONFIRM?               
         BNE   DR100                                                            
*                                                                               
DR20     THMS  DDSTIME=YES                                                      
         STCM  R0,15,PACKOF4B                                                   
         ST    R1,FULL                                                          
         AP    PACKOF4B,FULL                                                    
                                                                                
         GOTO1 DATCON,DMCB,(5,0),(0,DUB)   THE TIME                             
         CP    PACKOF4B,=P'240000'    PAST MIDNIGHT?                            
         BL    DR30                                                             
         SP    PACKOF4B,=P'240000'    YES, BUMP TO NEXT DAY AND ADJUST          
         GOTO1 ADDAY,DMCB,DUB,DUB,F'1'                                          
                                                                                
DR30     GOTO1 DATCON,DMCB,(0,DUB),(19,DOXMTSTD)                                
         ICM   R1,15,PACKOF4B                                                   
         SRL   R1,12               GET RID OF SECONDS AND SIGN                  
         STCM  R1,3,DOXMTSTT                                                    
*                                                                               
         MVI   DOXMTSTA,QBYRCNFM                                                
         DROP  R6                                                               
*                                                                               
DR100    L     R6,AIO                                                           
         MVI   ELCODE,DOSTELQ                                                   
         BAS   RE,GETEL                                                         
*                                                                               
         USING DOSTELD,R6                                                       
         CLI   DOSTSTAT,DFXDLVD                                                 
         BE    DR110                                                            
         CLI   DOSTSTAT,DEMDLVD                                                 
         BE    DR110                                                            
         CLI   DOSTSTAT,QCFMD      AM I CONFIRMED!!                             
         BNE   CANTCFM                                                          
         TM    MISCFLG1,MF1CNFCM   WITH COMMENTS?                               
         BZ    CANTCFM                                                          
         CLI   POMAUCFM,C'B'       PROFILE SET TO MANUAL CONFIRM?               
         BNE   CANTCFM                                                          
         DROP  R6                                                               
*                                                                               
DR110    LA    R2,DOSTELEM         NEW STATUS ELEMENT (AUDIT TRAIL)             
         USING DOSTELD,R2                                                       
         XC    DOSTELEM,DOSTELEM                                                
         MVI   DOSTEL,DOSTELQ                                                   
         MVI   DOSTLEN,DOSTLNQ                                                  
*                                                                               
         THMS  DDSTIME=YES                                                      
         STCM  R0,15,PACKOF4B                                                   
         ST    R1,FULL                                                          
         AP    PACKOF4B,FULL                                                    
                                                                                
         GOTO1 DATCON,DMCB,(5,0),(0,DUB)   THE TIME                             
         CP    PACKOF4B,=P'240000'    PAST MIDNIGHT?                            
         BL    DR125                                                            
         SP    PACKOF4B,=P'240000'    YES, BUMP TO NEXT DAY AND ADJUST          
         GOTO1 ADDAY,DMCB,DUB,DUB,F'1'                                          
                                                                                
DR125    GOTO1 DATCON,DMCB,(0,DUB),(19,DOSTDATE)                                
         ICM   R1,15,PACKOF4B                                                   
         SRL   R1,12               GET RID OF SECONDS AND SIGN                  
         STCM  R1,3,DOSTTIME                                                    
         MVI   DOSTSTAT,QBYRCNFM                                                
         DROP  R2                                                               
*                                                                               
         L     R6,AIO                                                           
         LA    R6,DORFRST-DOKEY(R6)  R6 = A(FIRST ELEMENT IN RECORD)            
DR130    CLI   0(R6),0          ADD AFTER ALL TRANSMISSION ELEMENTS             
         BE    DR140            AND BEFORE AN AUDIT ELEMENT                     
         CLI   0(R6),DOSTELQ                                                    
         BNL   DR140                                                            
         ZIC   R0,1(R6)                                                         
         AR    R6,R0                                                            
         B     DR130                                                            
*                                                                               
DR140    GOTO1 RECUP,DMCB,(C'S',AIO),DOSTELEM,(R6)                              
*                                                                               
         GOTO1 COLPASKY,DMCB,=C'K'                                              
*                                                                               
         GOTO1 PUTREC                                                           
*                                                                               
         BAS   RE,UPDEXTRA         UPDATE THE X'05' EXTRA RECORD                
*                                                                               
DR120    NI    MISCFLG1,X'FF'-MF1RECNF                                          
         LA    R4,KEY              GET THE BYRCNFM RECORD                       
         USING DOKEY,R4                                                         
         MVC   DOKEY,SAVEKEY                                                    
         MVI   DOKCMT,X'03'        BYRCNFM COMMENT RECORD                       
         MVI   RDUPDATE,C'Y'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(L'DOKEY),KEYSAVE                                             
         BNE   DR220               RECORD NOT FOUND, MUST ADD RECORD            
         MVI   RDUPDATE,C'Y'                                                    
         GOTO1 GETREC                                                           
         L     R6,AIO                                                           
         B     DR230                                                            
         DROP  R4                                                               
*                                                                               
DR220    OI    MISCFLG1,MF1RECNF                                                
         L     RE,AIO                                                           
         LA    RF,2000                                                          
         XCEFL                                                                  
*                                                                               
         L     R6,AIO                                                           
         USING DOKEY,R6                                                         
         MVC   DOKEY,SAVEKEY                                                    
         MVI   DOKCMT,X'03'        BYRCNFM COMMENT RECORD                       
         MVC   DORLEN,=Y(DORFRST-DOKEY)                                         
         MVC   DORAGY,AGENCY                                                    
         DROP  R6                                                               
*                                                                               
DR230    LA    R6,DORFRST-DOKEY(R6)  R6 = A(FIRST ELEMENT IN RECORD)            
         LA    R2,ORCCOM1H                                                      
*                                                                               
         LA    R3,ELEM                                                          
         USING DOCM2ELD,R3                                                      
         XC    ELEM,ELEM                                                        
         MVI   DOCM2EL,DOCM2ELQ                                                 
         MVC   DOCM2REV,REVISION                                                
         MVI   DOCM2LIN,1                                                       
         ZIC   R1,5(R2)                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   DOCM2TXT(0),8(R2)                                                
         LA    R1,1+DOCM2OVH(R1)                                                
         STC   R1,DOCM2LEN                                                      
         GOTO1 RECUP,DMCB,(C'S',AIO),ELEM,(R6)                                  
*                                                                               
         ZIC   R0,ELEM+1                                                        
         AR    R6,R0                                                            
         LA    R2,ORCCOM2H                                                      
         CLI   5(R2),0                                                          
         BE    DR240                                                            
*                                                                               
         LA    R3,ELEM                                                          
         XC    ELEM,ELEM                                                        
         MVI   DOCM2EL,DOCM2ELQ                                                 
         MVC   DOCM2REV,REVISION                                                
         MVI   DOCM2LIN,2                                                       
         ZIC   R1,5(R2)                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   DOCM2TXT(0),8(R2)                                                
         LA    R1,1+DOCM2OVH(R1)                                                
         STC   R1,DOCM2LEN                                                      
         DROP  R3                                                               
         GOTO1 RECUP,DMCB,(C'S',AIO),ELEM,(R6)                                  
*                                                                               
DR240    TM    MISCFLG1,MF1RECNF   ADD OR PUT?                                  
         BZ    DR250                                                            
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(L'DOKEY),0(R6)                                               
         GOTO1 ADDREC              DIRECTORY KEY AUTOMATICALLY ADDED            
         B     DRX                                                              
*                                                                               
DR250    CLI   REVISION,MAXREVN    BEYOND MAX # OF REVISIONS?                   
         BL    DR290                                                            
         ZIC   R0,REVISION                                                      
         AHI   R0,-7                                                            
         STC   R0,BYTE                                                          
*                                                                               
         SR    R0,R0                                                            
         L     R6,AIO                                                           
         LA    R6,DORFRST-DOKEY(R6)  R6 = A(FIRST ELEMENT IN RECORD)            
         USING DOCM2ELD,R6                                                      
DR260    CLI   0(R6),0                                                          
         BNE   *+6                                                              
         DC    H'0'                SHOULD NEVER HIT END OF RECORD               
         CLC   DOCM2REV,BYTE                                                    
         BL    DR290                                                            
         BE    DR270                                                            
         IC    R0,1(R6)                                                         
         AR    R6,R0                                                            
         B     DR260                                                            
*                                                                               
DR270    GOTO1 RECUP,DMCB,(C'S',AIO),(R6)  DELETE COMMENTS TO PREVENT           
         B     DR260                       OVERFLOW                             
*                                                                               
DR290    GOTO1 PUTREC                                                           
*                                                                               
         BAS   RE,CHKAUTH                                                       
*                                                                               
DRX      B     XIT                                                              
***********************************************************************         
* SET THE PFKEY INFORMATION                                                     
***********************************************************************         
SETPFKYS NTR1                                                                   
         SR    R2,R2               NO PFKEY AT TABLE FIRST                      
*                                                                               
         CLI   PFKEY,6                                                          
         BE    STPF10                                                           
*                                                                               
         LA    R2,PFTABLE          ACTION STATUS?                               
*                                                                               
STPF10   GOTO1 INITIAL,DMCB,(R2)   INITIALIZE THE PFKEYS                        
***************                                                                 
* SETUP THE PFKEY LINE                                                          
***************                                                                 
         CLI   CALLSP,0            ANYTHING TO RETURN TO?                       
         BE    *+10                NO                                           
         MVC   ORCPFLN(11),=CL11'PF12=Return'                                   
*                                                                               
         OI    ORCPFLNH+6,X'80'                                                 
*                                                                               
STPFX    B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* WRITE/ADD COLOR PASSIVE KEY                                                   
*                                                                               
* ON ENTRY: PARAM1                 NEW COLOR(1 CHAR)                            
*           DISKADDR IS SET                                                     
*                                                                               
***********************************************************************         
COLPASKY NTR1                                                                   
         L     R2,0(R1)                                                         
         MVC   BYTE,0(R2)                                                       
*                                                                               
         L     R6,AIO                                                           
         USING DOKEY,R6                                                         
         MVC   PREVKEY,DOKEY                                                    
         MVI   ELCODE,COLELQ                                                    
         BAS   RE,GETEL                                                         
         BE    CPK020                                                           
*                                                                               
* ADD NEW COLOR ELEMENT THEN ADD PASSIVE KEY                                    
*                                                                               
         LA    R2,COLOELEM                                                      
         USING COLOREL,R2                                                       
         XC    COLOELEM,COLOELEM                                                
         MVI   COLEL,COLELQ                                                     
         MVI   COLELLEN,COLLENQ                                                 
         MVC   COLCOL,BYTE                                                      
         MVC   COLDATE,CFFDATE                                                  
         DROP  R2                                                               
*                                                                               
         L     R6,AIO                                                           
         USING DOKEY,R6                                                         
         LA    R6,DORFRST                                                       
CPK005   CLI   0(R6),0             IF NO MORE ELEMENTS                          
         BE    CPK010              ADD AT TO THE END                            
         CLI   0(R6),COLELQ        ELSE ADD AFTER ALL ELEMENTS                  
         BNL   CPK010              < X'13'                                      
         ZIC   R0,1(R6)                                                         
         AR    R6,R0                                                            
         B     CPK005                                                           
CPK010   GOTO1 RECUP,DMCB,(C'S',AIO),COLOELEM,(R6)                              
         DROP  R6                                                               
*                                                                               
         LA    R4,KEY                                                           
         USING DOKEY,R4                                                         
         MVI   DSCKTYPE,DSCKTYPQ   X'0D'                                        
         MVI   DSCKSTYP,DSCKSTYQ   X'B8'                                        
         MVC   DSCKAGMD,BAGYMD                                                  
         MVC   DSCKBYR,SVBUYER                                                  
         OC    DSCKBYR,SPACES                                                   
         MVC   DSCKSTAT,BYTE                                                    
         MVC   DSCKDATE,CFFDATE                                                 
         MVC   DSCKORDR,BINORDER                                                
         OI    DMINBTS,X'08'       PASS BACK DELETED RECORDS                    
         GOTO1 HIGH                                                             
         CLC   KEY(L'DOKEY),KEYSAVE                                             
         BE    CPKUNDEL                                                         
         B     CPKADD                                                           
         DROP  R4                                                               
*                                                                               
*                                                                               
* FROM HERE, COLOR ELEMENT EXISTS                                               
*                                                                               
         USING COLOREL,R6                                                       
CPK020   LA    R4,KEY                                                           
         USING DOKEY,R4                                                         
         MVI   DSCKTYPE,DSCKTYPQ   X'0D'                                        
         MVI   DSCKSTYP,DSCKSTYQ   X'B8                                         
         MVC   DSCKAGMD,BAGYMD                                                  
         MVC   DSCKBYR,SVBUYER                                                  
         OC    DSCKBYR,SPACES                                                   
         MVC   DSCKSTAT,COLCOL                                                  
         MVC   DSCKDATE,COLDATE                                                 
         MVC   DSCKORDR,BINORDER                                                
*                                                                               
         OI    DMINBTS,X'08'       PASS BACK DELETED RECORDS                    
         GOTO1 HIGH                                                             
         CLC   KEY(L'DOKEY),KEYSAVE                                             
         BNE   CPKUPDAT            NO PASSIVE KEY (MUST RESTORE KEY)            
*                                                                               
         CLC   DSCKSTAT,BYTE                                                    
         BNE   CPKDELET            TURN DELETE BIT ON                           
         CLC   DSCKDATE,CFFDATE                                                 
         BNE   CPKDELET            TURN DELETE BIT ON                           
*                                                                               
* UNDELETE THE KEY AND UPDATE                                                   
*                                                                               
CPKUNDEL TM    KEY+13,X'80'                                                     
         BZ    CPKXIT              JUST EXIT                                    
CPKWRITE NI    KEY+13,X'FF'-X'80'  ELSE UNDELETE,                               
         GOTO1 WRITE               UPDATE KEY THEN                              
         B     CPKXIT              EXIT                                         
*                                                                               
CPKDELET DS    0H                                                               
         TM    KEY+13,X'80'        DELETE BIT ON?                               
         BNZ   CPKUPDAT            NO                                           
         OI    KEY+13,X'80'        YES: TURN DELETE BIT ON                      
         GOTO1 WRITE               AND UPDATE KEY                               
*                                                                               
*  UPDATE COLOR ELEMENT AND WRITE/ADD PASSIVE KEY                               
CPKUPDAT DS    0H                                                               
         MVC   COLCOL,BYTE                                                      
         MVC   COLDATE,CFFDATE                                                  
*                                                                               
         MVC   KEY,KEYSAVE         RESTORE THE KEY                              
         MVC   DSCKSTAT,BYTE                   NEW COLOR                        
         MVC   DSCKDATE,CFFDATE                TODAYS DATE                      
         OI    DMINBTS,X'08'       PASS BACK DELETED RECORDS                    
         GOTO1 HIGH                                                             
         CLC   KEY(L'DOKEY),KEYSAVE                                             
         BNE   CPKADD              KEY DOESN'T EXISTS, ADD IT                   
         CLC   DISKADDR,KEY+14     SAME DISK ADDRESS AS WELL?                   
         BE    CPKUNDEL            KEY EXISTS, UNDELETE IT                      
         MVC   KEY+14(4),DISKADDR  SAME DISK ADDRESS AS WELL?                   
         B     CPKWRITE                                                         
*                                  ELSE ADD                                     
CPKADD   DS    0H                                                               
         LA    R4,KEY                                                           
         USING DOKEY,R4                                                         
         MVC   KEY,KEYSAVE                                                      
         MVC   DSCKSTAT,BYTE                                                    
         MVC   DSCKDATE,CFFDATE                                                 
         MVC   KEY+14(4),DISKADDR                                               
         GOTO1 ADD                                                              
         DROP  R4                                                               
*                                                                               
CPKXIT   L     R6,AIO              RESTORE KEY BECAUSE A WRITE CLOBBERS         
         USING DOKEY,R6                WHAT IS IN AIO                           
         MVC   DOKEY,PREVKEY                                                    
         B     XIT                                                              
         DROP  R6                                                               
         EJECT                                                                  
*******************************************************************             
GETMKT   NTR1                                                                   
         XC    BMKT,BMKT                  CLEAR MARKET                          
         XC    FAKEFLDH,FAKEFLDH          CLEAR FIELD HEADER                    
         MVC   FAKEFLD,SPACES             AND FIELD                             
         GOTO1 MSUNPK,DMCB,BMKTSTA,FULL,FAKEFLD                                 
         LA    R2,FAKEFLDH                                                      
         MVI   FAKEFLDH+5,4               SET FIELD LENGTH                      
         CLI   FAKEFLD+4,C' '             TEST BAND                             
         BNH   *+8                                                              
         MVI   FAKEFLDH+5,5               FIX LENGTH FOR BAND                   
         GOTO1 VALISTA                                                          
         B     XIT                                                              
*******************************************************************             
*  READ FLIGHT RECORD                                                           
*******************************************************************             
READFLT  NTR1                                                                   
         LA    R4,KEY                                                           
         USING DFLRECD,R4                                                       
         XC    KEY,KEY                                                          
         MVC   DFLKTYP(2),=X'0D38'                                              
         MVC   DFLKAGMD,BAGYMD                                                  
         MVC   DFLKCLT,BCLT                                                     
         MVC   DFLKPRD,=C'POL'                                                  
         MVC   DFLKEST,BEST                                                     
         GOTO1 HIGH                FIRST READ FOR PRD POL                       
         CLC   KEY(9),KEYSAVE                                                   
         BE    RDFLT10                                                          
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(9),KEYSAVE      IF NO FLIGHT RECORD FOUND FOR                
         MVC   DFLKPRD,QPRD        POL THEN READ FOR BRAND                      
         GOTO1 HIGH                                                             
         CLC   KEY(9),KEYSAVE                                                   
         BNE   NO                                                               
*                                                                               
RDFLT10  MVC   AIO,AIO2                                                         
         GOTO1 GETREC                                                           
         LA    R4,DFLEL                                                         
RDFLT20  CLI   0(R4),X'00'                                                      
         BNE   *+6                                                              
         DC    H'00'                                                            
         CLI   0(R4),X'05'                                                      
         BE    RDFLT30                                                          
RDFLT25  ZIC   RE,1(R4)                                                         
         AR    R4,RE                                                            
         B     RDFLT20                                                          
         USING DFFLTEL,R4                                                       
RDFLT30  CLC   DFFLTNUM,SVBFLT     FLIGHT NUMBER                                
         BNE   RDFLT25                                                          
         MVC   SVFLSDT,DFFLTSTR    SAVE FLIGHT START DATE                       
         MVC   SVFLEDT,DFFLTEND    SAVE FLIGHT END DATE                         
RDFLTX   B     YES                                                              
         DROP  R4                                                               
***********************************************************************         
* CHECK IF AN AUTHORIZATION STATION RECORD EXISTS                     *         
***********************************************************************         
         SPACE 1                                                                
CHKAUTH  NTR1                                                                   
         LA    R6,KEY                                                           
         USING CLTHDRD,R6                                                       
         XC    KEY,KEY                                                          
         MVI   CKEYTYPE,0                                                       
         MVC   CKEYAM,BAGYMD                                                    
         MVC   CKEYCLT,BCLT                                                     
         GOTO1 HIGH                                                             
         CLC   KEY(L'CKEY),KEYSAVE                                              
         BE    *+6                                                              
         DC    H'00'                                                            
*                                                                               
         MVC   AIO,AIO3                                                         
         GOTO1 GETREC                                                           
         L     R6,AIO3                                                          
*                                                                               
         LA    R4,CLIST            GET THE EBCDIC PRODUCT CODES                 
CAUT01   CLI   0(R4),0                                                          
         BE    CAUT02              RETURN 'NO' TO CALLER IF EOT                 
         CLC   BPRD,3(R4)                                                       
         BNE   *+10                                                             
         MVC   QPRD,0(R4)          GOT THE EBCDIC PRODUCT CODE                  
         CLC   BPRD2,3(R4)                                                      
         BNE   *+10                                                             
         MVC   SVQPRD2,0(R4)       GOT THE EBCDIC PRODUCT CODE                  
         LA    R4,4(R4)                                                         
         B     CAUT01                                                           
*                                                                               
CAUT02   LA    R6,KEY              READ ESTIMATE RECORD TO CHECK IF             
         USING ESTHDR,R6            THIS IS A SDESK ESTIMATE                    
         XC    KEY,KEY                                                          
         MVI   EKEYTYPE,0                                                       
         MVC   EKEYAM,BAGYMD                                                    
         MVC   EKEYCLT,BCLT                                                     
         MVC   EKEYPRD,QPRD                                                     
         MVC   EKEYEST,BEST                                                     
         GOTO1 HIGH                                                             
         CLC   KEY(L'EKEY),KEYSAVE                                              
         BE    *+6                                                              
         DC    H'00'                                                            
*                                                                               
         L     R6,AIO3                                                          
         GOTO1 GETREC                                                           
         TM    EFLAG1,EF1SDE       SDESK AUTH OPEN FOR ESTIMATE?                
         BNO   CHKAUTHX            NO                                           
         MVC   ESTSTRT,ESTART      SET ESTIMATE PERIOD                          
         MVC   ESTEND,EEND                                                      
         DROP  R6                                                               
*                                                                               
         MVC   KEY,SAVEKEY                                                      
         MVI   RDUPDATE,C'Y'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(L'DOKEY),KEYSAVE                                             
         BNE   CHKAUTHX                                                         
         GOTO1 GETREC                                                           
*                                                                               
         MVI   ELCODE,DOXMTELQ                                                  
         BAS   RE,GETEL                                                         
         BNE   CHKAUTHX                                                         
         B     CAUT02B                                                          
CAUT02A  BAS   RE,NEXTEL                                                        
         BNE   CHKAUTHX                                                         
*                                                                               
         USING DOXMTELD,R6                                                      
CAUT02B  CLI   DOXMTSTA,QBYRCNFM   GET BUYER CONFIRMATION ELEMENT               
         BNE   CAUT02A                                                          
CAUT03   GOTO1 DATCON,DMCB,(8,DOXMTYMD),(2,SVSENTDT)                            
*                                                                               
         PUSH  USING                                                            
CAUT04   XC    ELEM,ELEM           CALL SPAUTH                                  
         USING SPAUTHD,ELEM                                                     
         MVC   SPACOM,ACOMFACS                                                  
         L     RF,AIO3                                                          
         ST    RF,SPAIO                                                         
         MVC   SPAKAM,BAGYMD                                                    
         MVC   SPAKCLT,BCLT                                                     
         MVC   SPAKPRD,BPRD                                                     
         MVC   SPAKPRD2,BPRD2                                                   
         MVC   SPAKEST,BEST                                                     
*                                                                               
         CLI   BPRD2,0             ANY PIGGYBACKS?                              
         BE    CAUT06                                                           
         CLC   QPRD,SVQPRD2        IF PRODUCTS NOT IN ALPHABETICAL              
         BL    CAUT06              ORDER, SWAP THEM                             
         XC    SPAKPRD2,SPAKPRD                                                 
         XC    SPAKPRD,SPAKPRD2                                                 
         XC    SPAKPRD2,SPAKPRD                                                 
*                                                                               
CAUT06   GOTO1 DATCON,DMCB,ESTSTRT,(2,SPASDTE)                                  
         GOTO1 (RF),DMCB,ESTEND,(2,SPAEDTE)                                     
         CLI   SVBFLT,0            IF FLIGHT NUMBER = 0, USE EST DATES          
         BE    CAUT08                                                           
         BAS   RE,READFLT                                                       
         BNE   CAUT08                                                           
         GOTO1 (RF),DMCB,(3,SVFLSDT),(2,SPASDTE)                                
         GOTO1 (RF),DMCB,(3,SVFLEDT),(2,SPAEDTE)                                
*                                                                               
CAUT08   DS    0H                                                               
         MVC   BSTA,SVSTA                                                       
         BAS   RE,GETMKT                                                        
         MVC   SPAKMKT(5),BMKTSTA                                               
         MVC   SPADRSDT,SVSENTDT                                                
         MVI   SPAUPDT,SPAUPBCN    UPDATE BUYER CONFIRMATION DATE               
         GOTO1 SPAUTH,ELEM                                                      
         CLI   SPAERR,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         POP   USING                                                            
*                                                                               
CHKAUTHX B     XIT                                                              
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* UPDATE THE EXTRA RECORD FOR OM DESKTOP                                        
*                                                                               
*  ON ENTRY: DARE RECORD IN AIO1                                                
*                                                                               
***********************************************************************         
UPDEXTRA NTR1                                                                   
         L     R6,AIO                                                           
         XC    KEY,KEY                                                          
         MVC   KEY(L'DOKEY),0(R6)                                               
         MVI   KEY+12,X'05'        UPDATE EXTRA X'05' RECORD                    
*                                                                               
         MVI   ELCODE,DOSTELQ                                                   
         BAS   RE,GETEL                                                         
         USING DOSTELD,R6                                                       
         MVC   DOSTELEM(DOSTLNQ),0(R6)                                          
         L     R6,AIO                                                           
         DROP  R6                                                               
*                                                                               
         MVC   AIO,AIO2                                                         
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(L'DOKEY),KEYSAVE                                             
         BNE   UEX20                                                            
         MVI   RDUPDATE,C'Y'                                                    
         GOTO1 GETREC                                                           
         L     R4,AIO                                                           
         B     UEX40                                                            
*                                                                               
UEX20    OI    MISCFLG1,MF1XTRNF                                                
         L     RE,AIO                                                           
         LA    RF,2000                                                          
         XCEFL                                                                  
                                                                                
         L     R4,AIO                        NOTE: EXTRA REC                    
         USING DOKEY,R4                                                         
         MVC   DOKEY,KEYSAVE                                                    
         MVC   DORLEN,=Y(DORFRST-DOKEY)                                         
         MVC   DORAGY,AGENCY                                                    
         DROP  R4                                                               
*                                                                               
UEX40    LA    R4,DORFRST-DOKEY(R4)                                             
         LA    R3,DOSTELEM                                                      
         USING DOSTELD,R3                                                       
         MVI   DOSTLEN,DOSTLNQ4                                                 
         XC    DOSTPID(DOSTLNQ4-DOSTLNQ),DOSTPID                                
*                                                                               
         GOTO1 GETFACT,DMCB,0                                                   
         L     R1,0(R1)                                                         
         USING FACTSD,R1                                                        
         MVC   DOSTPID,FAPASSWD                                                 
         DROP  R1                                                               
*                                                                               
         MVI   ELCODE,DOSPELQ                                                   
         BAS   RE,GETEL                                                         
         USING DOSPELD,R6                                                       
         MVC   DOSTDEST,DOSPDEST   SAVE DESTINATION                             
         MVC   DOSTMTHD,DOSPMTHD   SAVE METHOD                                  
         MVC   DOSTSPTS,DOSPSPTS   SAVE SPOTS                                   
         MVC   DOSTTOTL,DOSPTOTL   SAVE TOTAL DOLLARS                           
         MVC   DOSTREVN,DOSPREVN   SAVE REVISION                                
         MVI   ELCODE,DOWIGELQ                                                  
         BAS   RE,NEXTEL           DO WE HAVE A FAX?                            
         BNE   *+10                NO                                           
         USING DOWIGELD,R6                                                      
         MVC   DOSTFXN,DOWIGFXN    SAVE FAX                                     
         DROP  R6                                                               
*                                                                               
         GOTO1 RECUP,DMCB,(C'S',AIO),DOSTELEM,(R4)                              
*                                                                               
         TM    MISCFLG1,MF1XTRNF   ADDING OR WRITING?                           
         BNZ   UEXWRT              ADDING                                       
UEXPUT   GOTO1 PUTREC              WRITING                                      
         B     UEXX                                                             
*                                                                               
UEXWRT   XC    KEY,KEY             ADDING                                       
         L     R4,AIO                                                           
         MVC   KEY(L'DOKEY),0(R4)                                               
         GOTO1 ADDREC              DIRECTORY KEY AUTOMATICALLY ADDED            
*                                                                               
         NI    MISCFLG1,X'FF'-MF1XTRNF                                          
         MVC   AIO,AIO1                                                         
UEXX     J     XIT                                                              
         LTORG                                                                  
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
*                                                                               
CANTCFM  MVI   GERROR1,CNTMNCFM    CAN ONLY MANUAL CONFIRM DELFAX ORDER         
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
         GETEL R6,DATADISP,ELCODE  USED FOR THE GETEL OPERATIONS                
         EJECT                                                                  
YES      SR    RC,RC                                                            
NO       LTR   RC,RC                                                            
XIT      XIT1                                                                   
         LTORG                                                                  
         SPACE 2                                                                
RELO     DS    A                                                                
         SPACE 2                                                                
*                                                                               
PFTABLE  DS    0C                                                               
*                                                                               
* RETURN TO CALLER                                                              
         DC    AL1(PF12X-*,12,PFTRPROG,0,0,0)                                   
         DC    CL3' ',CL8' ',CL8' '                                             
PF12X    EQU   *                                                                
*                                                                               
         DC    X'FF'                                                            
         EJECT                                                                  
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
       ++INCLUDE SPOMSFBD          (OUR DISPLAY SCREEN)                         
         EJECT                                                                  
*DDGENTWA                                                                       
       ++INCLUDE DDGENTWA                                                       
         EJECT                                                                  
*SPOMSWORKD                                                                     
       ++INCLUDE SPOMSWORKD        (SYSTEM AREAS)                               
         EJECT                                                                  
*SPGENDRORD                                                                     
       ++INCLUDE SPGENDRORD        (RECORD DSECTS)                              
         EJECT                                                                  
*SPGENDRFLT                                                                     
       ++INCLUDE SPGENDRFLT        (DARE FLIGHT RECORD)                         
         EJECT                                                                  
*SPAUTHD                                                                        
       ++INCLUDE SPAUTHD                                                        
         EJECT                                                                  
CLTHDRD  DSECT                                                                  
       ++INCLUDE SPGENCLT                                                       
         EJECT                                                                  
PRDHDRD  DSECT                                                                  
       ++INCLUDE SPGENPRD                                                       
         EJECT                                                                  
ESTHDRD  DSECT                                                                  
       ++INCLUDE SPGENEST                                                       
         EJECT                                                                  
         EJECT                                                                  
***********************************************************************         
* MY STORAGE AREA                                                               
***********************************************************************         
MYAREAD  DSECT                                                                  
ELEMDISP DS    H                   ELEMENT DISPLACEMENT (USED FOR DIS)          
*                                  HOB = X'80' = AT THE DELNOT                  
*                                  HOB = X'40' = AT THE STATUS                  
*                                                                               
PREVREP  DS    XL2                 PREVIOUS REP ID NUMBER                       
PREVCON  DS    CL8                 PREVIOUS REP CONTRACT NUMBER                 
MISCFLG1 DS    XL1                 MISCELLANEOUS FLAGS                          
MF1KYCHG EQU   X'80'                 A KEY FIELD HAS BEEN CHANGED               
MF1RECNF EQU   X'40'                 RECORD NOT FOUND                           
MF1CNFCM EQU   X'20'                 CONFIRMED WITH COMMENTS                    
MF1XTRNF EQU   X'10'                 EXTRA RECORD NOT FOUND                     
*                                                                               
REVISION DS    XL1                 REVISION NUMBER                              
*                                                                               
BINORDER DS    0XL4                BINARY ORDER NUMBER                          
BINORDDT DS    XL2                     DATE PORTION                             
BINORDSQ DS    XL2                     SEQUENCE PORTION                         
*                                                                               
PREVJUDT DS    XL3                 PREVIOUS JULIAN DATE (PWOS)                  
*                                                                               
PACKOF4B DS    PL4                 PACKED OF 4 BYTES                            
*                                                                               
SVBUYER  DS    CL3                                                              
SVSTA    DS    XL3                                                              
SVQPRD2  DS    CL3                 EBCIDIC PRODUCT                              
SVBFLT   DS    XL1                 FLIGHT NUMBER                                
SVFLSDT  DS    XL3                 FLIGHT START DATE                            
SVFLEDT  DS    XL3                 FLIGHT END DATE                              
SVSENTDT DS    XL2                 ORDER SENT DATE                              
*                                                                               
PREVKEY  DS    XL(L'DOKEY)         SAVED KEYS                                   
SAVEKEY  DS    XL(L'DOKEY)                                                      
DISKADDR DS    XL4                 DISK ADDRESS                                 
*                                                                               
CFFDATE  DS    XL2                 TODAY'S DATE COMPRESSED X'FF' COMPL.         
DOSTELEM DS    CL(L'DOSTELD)       HISTORY ELEMENT                              
COLOELEM DS    CL(COLLENQ)         NEW COLOR STATUS ELEMENT                     
                                                                                
FAKEFLDH DS    CL8                 FAKE FIELD HEADER AND FIELD DATA             
FAKEFLD  DS    CL60                                                             
MAXREVN  EQU   8                   MAXIMUM NUMBER OF REVISIONS STORED           
*                                                                               
PROFOM   DS    CL16                OM PROFILE                                   
POMUSEOM EQU   PROFOM               - USES ORDER MANAGER?                       
POMAUCFM EQU   PROFOM+2             - PARITAL CONFIRM WORKFLOW                  
POMMGREP EQU   PROFOM+15            - CREATE MKGD TRANSACTION REPORT?           
*                                                                               
***********************************************************************         
* DISPLAY LINE DSECT                                                            
***********************************************************************         
DISLINED DSECT                                                                  
DISCOM   DS    CL(L'ORCCOM1)                                                    
DISNEXTL DS    0C                                                               
       ++INCLUDE SPOMSDARED                                                     
         EJECT                                                                  
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
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'219SPOMS0B   01/03/07'                                      
         END                                                                    
