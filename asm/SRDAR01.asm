*          DATA SET SRDAR01    AT LEVEL 135 AS OF 11/03/20                      
*PHASE T16101A                                                                  
         PRINT NOGEN                                                            
         TITLE '$DMG (T16101) -- DARE MAKEGOOD'                                 
**********************************************************************          
* THIS IS AN ONLINE PROGRAM TO RETRIEVE AND FORMAT ONTO SPOT FILE               
*        DARE MAKEGOOD ORDERS                                                   
**********************************************************************          
SRDAR01  CSECT                                                                  
         NMOD1 WORKX-WORKD,$D01*,R8,RR=R3                                       
         USING WORKD,RC                                                         
         ST    R3,RELO                                                          
         ST    RD,SAVERD                                                        
         ST    R1,SAVER1                                                        
         MVI   RETCODE,0           RESET ERROR RETURN BYTE                      
*                                                                               
*     INITIALIZATION                                                            
*                                                                               
         LR    RF,RC                                                            
         L     RE,=A(PQREC-WORKD)                                               
         AR    RF,RE                                                            
         ST    RF,APQREC                                                        
         LR    RF,RC                                                            
         AHI   RF,IO1-WORKD                                                     
         ST    RF,AIO1                                                          
         LR    RF,RC                                                            
         AHI   RF,IO2-WORKD                                                     
         ST    RF,AIO2                                                          
         LR    RF,RC                                                            
         AHI   RF,IO3-WORKD                                                     
         ST    RF,AIO3                                                          
         LR    RF,RC                                                            
         AHI   RF,IO4-WORKD                                                     
         ST    RF,AIO4                                                          
         LR    RF,RC                                                            
         AHI   RF,IO5-WORKD                                                     
         ST    RF,AIO5                                                          
         LR    RF,RC                                                            
         AHI   RF,SPULAREA-WORKD                                                
         ST    RF,ASPLAREA                                                      
*                                                                               
         L     RE,0(R1)                                                         
         MVC   SRPARS,0(RE)        SAVE S/R PARAMETER LIST                      
*                                                                               
SRPARMSD USING SRPARMD,SRPARS                                                   
*                                                                               
         L     R9,SRPARMSD.SRQASYSF                                             
         USING SYSFACD,R9          R9 = A(SYSTEM FACILITIES)                    
         L     RF,VSYSFAC2         GET THE SPOT SYSFAC                          
         USING SPSYSFAC,RF                                                      
         MVC   VRECUP,SRECUP                                                    
         DROP  RF                                                               
*                                                                               
         L     R9,SRPAR1                                                        
         USING SYSFACD,R9          A(SYSFACS)                                   
*                                                                               
         MVC   ATIA,SRPAR6         A(TIA)                                       
         MVC   AUTL,SRPAR3         A(UTL)                                       
         L     RA,ATIA                                                          
         USING T161FED,RA          A(TWA)                                       
         L     R6,SRPAR4                                                        
         USING COMFACSD,R6         A(COMFACS)                                   
         ST    R6,ACOMFACS                                                      
         MVC   VHEXIN,CHEXIN                                                    
         MVC   VHEXOUT,CHEXOUT                                                  
         MVC   VGETFACT,CGETFACT                                                
         MVC   VSWITCH,CSWITCH                                                  
         MVC   VHELLO,CHELLO                                                    
         MVC   VDATCON,CDATCON                                                  
         MVC   VDATVAL,CDATVAL                                                  
         MVC   VGETDAY,CGETDAY                                                  
         MVC   VCALLOVL,CCALLOV                                                 
         MVC   VLOCKET,CLOCKET                                                  
         MVC   VCASHVAL,CCASHVAL                                                
         MVC   VADDAY,CADDAY                                                    
         MVC   VDEMOCON,CDEMOCON                                                
         DROP  R6                                                               
***************                                                                 
* CORERES STUFF                                                                 
***************                                                                 
         XC    DMCB(12),DMCB                                                    
         MVC   DMCB+4,=X'D9000A0C'    SPOOL                                     
         GOTOR VCALLOVL,DMCB                                                    
         MVC   ASPOOL,DMCB                                                      
*                                                                               
         MVI   DMCB+7,X'7A'           STAPACK                                   
         GOTOR VCALLOVL,DMCB                                                    
         MVC   ASTAPACK,DMCB                                                    
*                                                                               
         XC    POWERCDE,POWERCDE                                                
*                                                                               
         L     RE,VSSB                                                          
         USING SSBD,RE                                                          
         NI    SSBDARFL,X'FF'-SSBDRSPQ   SO TASKER DOESN'T CALL AGAIN           
         L     R2,SSBDARPQ         R2 = A(DARE PQ TABLE)                        
         DROP  RE                                                               
******                                                                          
* R7 IS STILL POINTING TO THE EDICT ENTRY FROM SRDAR00                          
******                                                                          
         USING PMKGHDRD,R7                                                      
         XC    WORK,WORK                                                        
         GOTOR VHEXIN,DMCB,PMKGUSER,WORK,4    HEX ID # OF USER                  
         OC    WORK(2),WORK                                                     
         BZ    EXIT                                                             
         MVC   WORK+2(3),PMKGSUBI                                               
         GOTOR VHEXIN,DMCB,PMKGREPN,WORK+5,4                                    
         DROP  R7                                                               
*                                                                               
         LA    R2,WORK                                                          
         GOTOR CTRLSET,DMCB,(R2)   CHECK/SWITCH BET CTRL + SPOT                 
         BNE   SRDMG900            RETURN WITH RETURN CODE                      
*                                                                               
         GOTOR RETRVREP,DMCB,(R2)  GET THE REPORT FROM PRINT QUEUE              
         BNE   SRDMG900            REPORT NOT FOUND, EXIT                       
*                                                                               
         MVI   MISCFLG1,0          RESET THE MISCELLANEOUS FLAGS                
         MVI   ERRFLG1,0           RESET ERROR FLAG                             
         BRAS  RE,UPDATDAR         PROCESS THE REPORT                           
*                                                                               
         GOTOR VDATAMGR,DMCB,(0,=C'DMUNLK'),0                                   
*                                                                               
SRDMG900 DS    0H                                                               
         L     R1,SAVER1                                                        
         MVC   4(1,R1),RETCODE     SET RETCODE IN CALLER'S P2(1)                
         B     EXIT                                                             
*                                                                               
*RETCODE VALUES:                                                                
*01-CAN'T OPEN CTRL SYS, OR TARGET SE SYSTEM, NOT OPENED FOR WRITE              
*                                                                               
         EJECT                                                                  
***********************************************************************         
* RETRIEVES LINES OF REPORT AND CALLS THE APPROPRIATE ROUTINE FOR THAT          
* SPECIFIC LINE OF THE REPORT                                                   
***********************************************************************         
UPDATDAR NTR1                                                                   
         LA    R4,PQINDEX          SET INDEX FOR REPORT                         
         USING UKRECD,R4                                                        
         XC    LASTREC,LASTREC     SET OUTPUT FLAGS                             
         MVI   LINENUM,0           NO LINES READ YET                            
         MVI   RECFLAG,0           RESET THIS CAUSE OF REPEATING ERROR          
         MVI   RECFLAG2,0                                                       
*                                                                               
UPDA0010 XC    R,R                                                              
         L     RE,APQREC                                                        
         ST    RE,DMCB+16                                                       
         GOTOR VDATAMGR,DMCB,(0,=C'READ'),UKUSRINF,0,R,,0                       
         CLI   DMCB+8,0            END OF PQ CONTROL INTERVAL?                  
         BNE   UPDA0900            YES - FINISHED - CHECK OUTPUT                
*                                                                               
         LARL  R3,RECTABLE         A(CONVERSION TABLE)                          
UPDA0080 CLI   0(R3),0             END OF TABLE REACHED?                        
         BE    UPDA0010            YES - SKIP RECORD - READ NEXT                
*                                                                               
         CLC   0(6,R3),R+1         NO  - LOOK FOR RECORD TYPE                   
         BE    *+12                FOUND - PROCESS IT                           
         LA    R3,LRECTAB(R3)      BUMP TO NEXT ENTRY                           
         B     UPDA0080            CHECK NEXT TYPE IN TABLE                     
*                                                                               
         ZIC   R1,LINENUM          LINE NUMBER IN REPORT                        
         LA    R1,1(R1)                                                         
         STC   R1,LINENUM                                                       
*                                                                               
         L     RF,8(R3)            LOAD ROUTINE ADDRESS                         
         A     RF,RELO                                                          
         BASR  RE,RF               BRANCH TO ROUTINE                            
         BE    UPDA0010            GO BACK FOR NEXT RECORD                      
         TM    MISCFLG1,MF1SKIP    SKIP REPORT UNTIL NEXT TIMER POP?            
         BNZ   EXIT                YES                                          
*                                                                               
UPDA0900 L     RE,APQREC                                                        
         ST    RE,DMCB+16                                                       
         GOTOR VDATAMGR,DMCB,(0,=C'PRINTED'),UKUSRINF,PQINDEX,R6                
         CLI   DMCB+8,0            MARKED AS 'PRINTED'?                         
         BE    EXIT                YES                                          
         DC    H'0'                NO                                           
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
*   MKGTST: MAKEGOOD TEST                                                       
***********************************************************************         
MKGTST   NTR1                                                                   
         L     R1,MKGCOUNT         INCREMENT RECORD COUNT                       
         LA    R1,1(R1)                                                         
         ST    R1,MKGCOUNT                                                      
         B     YES                                                              
***********************************************************************         
*   MKGDS1: MAKEGOOD OFFER COMMENT                                              
***********************************************************************         
MKGDS1   NTR1                                                                   
         TM    RECFLAG,INPROG      MAKEGOOD RECORDS IN PROGRESS?                
         JNO   BADSQMGO                                                         
*                                                                               
         CLI   LASTREC,DS1         OFFER COMMENT                                
         BE    MKGDS110                                                         
*                                                                               
         CLI   LASTREC,HDR         MAKEGOOD HEADER                              
         JNE   BADSQMGO                                                         
         MVI   LASTREC,DS1         SET LAST RECORD TYPE                         
         MVI   DS1COUNT,0          INITIALIZE MKGDS1 COUNT                      
*                                                                               
MKGDS110 L     R1,MKGCOUNT         INCREMENT RECORD COUNT                       
         LA    R1,1(R1)                                                         
         ST    R1,MKGCOUNT                                                      
*                                                                               
         LA    R4,R+1              SET INPUT RECORD DSECT                       
         USING MOFRDS1D,R4                                                      
*                                                                               
         CLC   ORDNO,MOD1ORDR      ORDER #                                      
         JNE   INCNDATA                                                         
*                                                                               
         XC    ELTAREA,ELTAREA                                                  
         LA    R2,ELTAREA                                                       
         USING MOCMELD,R2                                                       
*                                                                               
         MVI   MOCMEL,MOCMELQ      ELEMENT CODE                                 
         MVI   MOCMLEN,MOCMOVRH    OVERHEAD LENGTH                              
*                                                                               
         ZIC   R1,DS1COUNT         INCREMENT MKGDS1 COUNT                       
         LA    R1,1(R1)                                                         
         STC   R1,DS1COUNT                                                      
         STC   R1,MOCMLINE         STORE LINE #                                 
*                                                                               
         LA    R1,MOD1TEXT+L'MOD1TEXT-1  FIND LAST NON-BLANK CHARACTER          
         LA    RF,MOD1TEXT               IN THE COMMENT                         
MKGDS120 CR    RF,R1                                                            
         BE    MKGDS130                                                         
         CLI   0(R1),C' '                                                       
         BH    *+8                                                              
         BCT   R1,MKGDS120                                                      
*                                                                               
         SR    R1,RF               LENGTH OF COMMENT - 1                        
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   MOCMTEXT(0),MOD1TEXT                                             
*                                                                               
         AHI   R1,MOCMOVRH+1       ADD LENGTH OF OVERHEAD+1                     
         STC   R1,MOCMLEN                                                       
         DROP  R2                                                               
*                                                                               
MKGDS130 GOTOR ADDEL,DMCB,(RECOSEQ,AIO1),(OEXISTS,0)                            
         B     YES                                                              
         EJECT                                                                  
*                                                                               
***********************************************************************         
*   MKGDS2: MAKEGOOD DEMO CATEGORIES                                            
***********************************************************************         
MKGDS2   NTR1                                                                   
         TM    RECFLAG,INPROG      MAKEGOOD RECORDS IN PROGRESS?                
         JNO   BADSQMGO                                                         
*                                                                               
         CLI   LASTREC,DS1         OFFER COMMENT                                
         BE    MKGDS210                                                         
         CLI   LASTREC,HDR         MAKEGOOD HEADER                              
         JNE   BADSQMGO                                                         
*                                                                               
MKGDS210 MVI   LASTREC,DS2         SET LAST RECORD TYPE                         
*                                                                               
         L     R1,MKGCOUNT         INCREMENT RECORD COUNT                       
         LA    R1,1(R1)                                                         
         ST    R1,MKGCOUNT                                                      
*                                                                               
         LA    R4,R+1              SET INPUT RECORD DSECT                       
         USING MOFRDS2D,R4                                                      
*                                                                               
         MVC   SVMG2DEC,MOD2TWOD   RATINGS ARE 2 DECIMAL PRECISION?             
*                                                                               
         CLC   ORDNO,MOD2ORDR      ORDER #                                      
         JNE   INCNDATA                                                         
*                                                                               
         XC    SVMODMEL,SVMODMEL                                                
         LA    R2,SVMODMEL         CREATE A SELLER DEMO ELEM                    
         USING MODMELD,R2                                                       
*                                                                               
         MVI   MODMEL,MOSDELQ      X'62' - SELLER DEMO ELEM EQU                 
         MVI   MODMLEN,MODMOVRH+(L'MODMDEMO*4)                                  
*                                                                               
         MVI   RCHEMPTD,C'N'       EMPTY DEMO CATEGORY                          
         LA    R3,4                WE HAVE A MAX OF 4 DEMO CATEGORIES           
MKGDS220 CLI   MOD2TDEM,C'R'       RATING CATEGORY?                             
         BE    MKGDS225                                                         
         CLI   MOD2TDEM,C'I'       IMPRESSION CATEGORY?                         
         BE    MKGDS225                                                         
         CLI   MOD2TDEM,C' '       EMPTY?                                       
         JNE   INVMKGD2                                                         
         MVI   RCHEMPTD,C'Y'       NO, WE HAVE AN EMPTY DEMO CAT NOW            
         B     MKGDS230            NEXT DEMO CATEGORY                           
*                                                                               
MKGDS225 CLI   RCHEMPTD,C'Y'       WE HAD AN EMPTY DEMO CATEGORY B4?            
         JE    INVMKGD2            YES, SEND ERROR                              
*                                                                               
         MVC   MDEMNO+1(1),MOD2TDEM                                             
         LA    RE,MOD2TDEM+1                                                    
         ST    RE,DMCB                                                          
         MVI   DMCB,C'N'                                                        
*****    OI    DMCB,X'80'                                                       
         GOTOR VCASHVAL,DMCB,,3   COST                                          
         CLI   0(R1),0                     CHECK RETURN CODE                    
         JNE   INVMKGD2                                                         
         OC    4(3,R1),4(R1)       GREATER THAN 255?                            
         JNZ   INVMKGD2                                                         
         CLI   7(R1),0             SENT 0?                                      
         JE    INVMKGD2                                                         
         MVC   MDEMNO+2(1),7(R1)              LAST BYTES ONLY                   
         MVI   MDSVI,100              SET HUT VALUE = 100                       
*                                                                               
         LA    R2,L'MODMDEMO(R2)      BUMP TO NEXT DEMO IN SVMODMEL             
*                                                                               
MKGDS230 LA    R4,L'MOD2TDEM(R4)      BUMP TO NEXT DEMO IN MKGDS2               
         BCT   R3,MKGDS220                                                      
*                                                                               
         LA    RF,SVMODMEL                                                      
         CR    R2,RF               DID WE GET DEMOS?                            
         JE    INVMKGD2             NO, SEND ERROR                              
         SR    R2,RF                                                            
         AHI   R2,MODMOVRH                                                      
         STC   R2,SVMODMEL+1                                                    
         DROP  R2                                                               
                                                                                
         B     YES                                                              
         EJECT                                                                  
***********************************************************************         
*   MKGMNW: CABLE NETWORK - PRECEDES GROUP OF MKGMSS FOR THAT NETWORK           
***********************************************************************         
MKGMNW   NTR1                                                                   
         TM    RECFLAG,INPROG      MAKEGOOD RECORDS IN PROGRESS?                
         JNO   BADSQMGO                                                         
*                                                                               
         CLI   QSTATN,C'0'         ONLY CABLE SUPPORTS MKGMNW RECORD            
         JL    NOTCABLE            - SEND ERROR IF NOT CABLE                    
*                                                                               
         CLI   LASTREC,DS1         OFFER COMMENT                                
         BE    MKGMN010                                                         
         CLI   LASTREC,DS2         DEMO CATEGORIES                              
         BE    MKGMN010                                                         
         CLI   LASTREC,CDC         COMSCORE DEMO CATEGORIES                     
         BE    MKGMN010                                                         
         CLI   LASTREC,HDR         MAKEGOOD HEADER                              
         BE    MKGMN010                                                         
*                                                                               
         CLI   LASTREC,DTL         DETAIL                                       
         BE    MKGMN005                                                         
         CLI   LASTREC,COM         COMMENT                                      
         BE    MKGMN005                                                         
         CLI   LASTREC,MSS         MISSED SPOT                                  
         BE    MKGMN005                                                         
         CLI   LASTREC,MAAU        MISSED UUID                                  
         BE    MKGMN005                                                         
         CLI   LASTREC,ORB         ORBIT RECORD?                                
         JNE   BADSQMGO                                                         
*                                                                               
MKGMN005 TM    RECFLAG2,MNWINPRG+ONWINPRG   MNW OR ONW IN PROGRESS?             
         BZ    MKGMN010                                                         
         GOTOR RECWRT,DMCB,(RECCSEQ,AIO4),(CEXISTS,0)                           
         NI    RECFLAG2,X'FF'-MNWINPRG-ONWINPRG                                 
*                                                                               
MKGMN010 DS    0H                                                               
***NOP   MVI   LASTREC,MNW                                                      
*                                                                               
         L     R1,MKGCOUNT         INCREMENT RECORD COUNT                       
         LA    R1,1(R1)                                                         
         ST    R1,MKGCOUNT                                                      
*                                                                               
         LA    R4,R+1              SET INPUT RECORD DSECT                       
         USING MOFRMNWD,R4                                                      
*                                                                               
         CLC   ORDNO,MOMNORDR      ORDER #                                      
         JNE   INCNDATA                                                         
*                                                                               
*********                                                                       
*    CHECK TO INSURE DARE ORDER'S NETWORK EXIST                                 
*********                                                                       
         USING STAPACKD,R6                                                      
         LA    R6,WORK                                                          
         XC    0(32,R6),0(R6)                                                   
         MVI   STAPACT,C'Y'        TRANSLATE NETWORK NIELSEN 4 TO DDS 3         
         MVI   STAPCTRY,C'U'       UNITED STATES                                
         MVC   STAPACOM,ACOMFACS                                                
         MVC   STAPAGY,QAGY                                                     
         MVI   STAPMED,C'T'                                                     
         XC    STAPQMKT,STAPQMKT                                                
         MVC   STAPQSTA(4),MOMNNTWK                                             
*                                                                               
         GOTOR ASTAPACK,WORK       IF NO ERROR, STAPQNET IS SET                 
         CLC   STAPQNET,SPACE        STAPQNET SET?                              
         JE    INVCBLST              NO, ERROR                                  
*                                                                               
         MVC   STAPQMKT,FOXZEROS                                                
         MVI   STAPACT,C'P'        MSPACK                                       
         MVC   STAPQSTA,QSTATN                                                  
*                                                                               
         GOTOR ASTAPACK,WORK                                                    
         CLI   STAPERR,0                                                        
         JNE   INVCBLST                                                         
                                                                                
         MVI   CEXISTS,0                                                        
*                                                                               
         LA    R2,KEY              MAKEGOOD NOTICE RECORD                       
         USING DAREMGND,R2                                                      
         MVC   KEY,NOTCKEY                                                      
         MVC   MNXKSTTN,STAPSTA                                                 
         MVI   MNXKSEQ,1                                                        
*                                                                               
         MVI   RECCSEQ,0                                                        
         MVI   DMINBTS,X'08'       RDHI FOR DELETED                             
         BRAS  RE,XHIGH                                                         
*                                                                               
MKGMN020 CLC   KEYSAVE(MNXKSEQ-MNXKEY),KEY   MATCH ON NOTICE & NETWORK?         
         BNE   MKGMN040                                                         
*                                                                               
         MVC   RECCSEQ,MNXKSEQ                                                  
         MVI   DMINBTS,X'08'       READ SEQ FOR DELETED                         
         BRAS  RE,XSEQ                                                          
         B     MKGMN020                                                         
*                                                                               
MKGMN040 MVC   KEY(L'NOTCKEY),NOTCKEY                                           
         MVC   MNXKSTTN,STAPSTA                                                 
         MVI   DMINBTS,X'08'                                                    
         BRAS  RE,XHIGH                                                         
*                                                                               
         CLC   KEYSAVE(L'MNXKEY),KEY                                            
         JNE   MKGMN050                                                         
*                                                                               
         MVI   DMINBTS,X'88'                                                    
         L     R2,AIO4                                                          
         USING DAREMGND,R2                                                      
         ST    R2,AIO                                                           
         BRAS  RE,XGET                                                          
*                                                                               
         OI    CEXISTS,EXONFILE    NOTICE RECORD EXISTS                         
         TM    MNXRSTAT,X'80'      SHOULD BE DELETED?                           
         JZ    DUPMKMNW             - SEND ERROR                                
         OI    CEXISTS,EXDEL        YES                                         
*                                                                               
MKGMN050 L     R2,AIO4             MAKEGOOD NOTICE RECORD                       
         LR    RE,R2                                                            
         LA    RF,4000                                                          
         XCEFL                                                                  
         MVC   0(L'MNXKEY,R2),NOTCKEY                                           
         MVC   MNXKSTTN,STAPSTA                                                 
         LA    R1,MNXFRST-MNXKEY  LENGTH OF RECORD W/O ELEMENTS                 
         STCM  R1,3,MNXRLEN                                                     
         MVC   MNXRAGY,POWERCDE    ALPHA AGENCY CODE                            
         XC    ELTAREA,ELTAREA                                                  
         DROP  R2                                                               
*                                                                               
         OI    RECFLAG2,MNWINPRG   MAKEGOOD MNW IN PROGRESS                     
*                                                                               
MKGMNX   B     YES                                                              
***********************************************************************         
*   MKGMSS: MAKEGOOD OFFER MISSED SPOTS                                         
***********************************************************************         
MKGMSS   NTR1                                                                   
         TM    RECFLAG,INPROG      MAKEGOOD RECORDS IN PROGRESS?                
         JNO   BADSQMGO                                                         
*                                                                               
* LOOKS LIKE THERE ARE CASES WHERE MKGBUY ARE RECEIVED BEFORE AN MKGMSS         
* WHICH EXPLAINS THIS INSTRUCTION               -HWON 1/24/2014                 
         CLI   RCHDBUY,C'Y'        PROCESSED A MKGBUY ALREADY?                  
         BE    MKGMSS05             YES                                         
*                                                                               
         CLI   LASTREC,MSS         MISSED SPOT                                  
         BE    MKGMSS20                                                         
         CLI   LASTREC,MAAU        MISSED UUID                                  
         BE    MKGMSS20                                                         
         CLI   LASTREC,COM         COMMENT                                      
         BE    MKGMSS20                                                         
         CLI   LASTREC,ORB         ORBIT RECORD?                                
         BE    MKGMSS20                                                         
*                                                                               
***NOP   CLI   LASTREC,MNW         NETWORK                                      
***NOP   JE    MKGMSS10                                                         
*                                                                               
         CLI   LASTREC,HDR         MAKEGOOD HEADER                              
         BE    MKGMSS10                                                         
         CLI   LASTREC,DS1         OFFER COMMENT                                
         BE    MKGMSS10                                                         
         CLI   LASTREC,DS2         DEMO CATEGORIES                              
         BE    MKGMSS10                                                         
         CLI   LASTREC,CDC         COMSCORE DEMO CATEGORIES                     
         BE    MKGMSS10                                                         
         J     BADSQMGO                                                         
*                                                                               
MKGMSS05 CLI   LASTREC,DTL         DETAIL                                       
         JNE   BADSQMGO                                                         
*                                                                               
MKGMSS10 DS    0H                                                               
***NOP   OI    RECFLAG,MISSDINC    MISSED SPOTS INCLUDED                        
         ZIC   R1,MSOFFNUM         INCREMENT OFFER NUMBER                       
         LA    R1,1(R1)                                                         
         STC   R1,MSOFFNUM                                                      
         MVC   OFFERNUM,MSOFFNUM                                                
*                                                                               
MKGMSS20 MVI   LASTREC,MSS         SET LAST RECORD TYPE TO CURRENT ONE          
         MVI   RCHDBUY,C'N'                                                     
         L     R1,MKGCOUNT         INCREMENT RECORD COUNT                       
         LA    R1,1(R1)                                                         
         ST    R1,MKGCOUNT                                                      
*                                                                               
         LA    R4,R+1              SET INPUT RECORD DSECT                       
         USING MOFRMISD,R4                                                      
*                                                                               
         CLC   ORDNO,MOMSORDR      ORDER #                                      
         JNE   INCNDATA                                                         
*                                                                               
         XC    ELTAREA,ELTAREA                                                  
         LA    R2,ELTAREA                                                       
         USING MNMSELD,R2                                                       
*                                                                               
         MVI   MNMSEL,MNMSELQ      ELEMENT CODE                                 
         MVI   MNMSLEN,MNMSOVRH    OVERHEAD LENGTH                              
*                                                                               
         ZIC   R0,MISSSEQ          EACH MISSED SPOT IS DISTINCT                 
         AHI   R0,1                                                             
         STC   R0,MISSSEQ                                                       
         MVC   MNMSSEQN,MISSSEQ                                                 
*                                                                               
         CLC   MOMSAGYL,SPACE      NO BUYLINE???                                
         BH    MKGMSS23                                                         
         MVC   MOMSAGYL,=C'001'    NONE, DEFAULT IT TO 001                      
*                                                                               
MKGMSS23 PACK  DUB,MOMSAGYL        BUYLINE NUMBER                               
         CVB   R5,DUB                                                           
         STC   R5,MNMSBLIN                                                      
         STCM  R5,3,MNMSBLN2       TAKES OVER MNMSBLIN FOR 2 BYTE BYLNS         
*                                                                               
         LA    RE,MOMSDATE          MAKE SURE VALID NUMERIC                     
         LA    R1,L'MOMSDATE                                                    
MKGMSS25 CLI   0(RE),C'0'                                                       
         JL    INVNMFLD                                                         
         CLI   0(RE),C'9'                                                       
         JH    INVNMFLD                                                         
         LA    RE,1(RE)                                                         
         BCT   R1,MKGMSS25                                                      
*                                                                               
         GOTOR VDATCON,DMCB,(0,MOMSDATE),(19,MNMSBDAT)     SPOT DATE            
*                                                                               
         PACK  DUB,MOMSNSPT        NUMBER OF MISSED SPOTS                       
         CVB   R5,DUB                                                           
         STC   R5,MNMSNSPT                                                      
*                                                                               
         LA    R1,MOMSROTN+6       LAST DAY OF ROTATION                         
         LA    R5,7                LOOP FOR 7 DAYS                              
         MVI   BYTE,X'01'          01 = SUN 02 = SAT 04 = FRI..40 = MON         
*                                                                               
MKGMSS30 CLI   0(R1),C' '          CHECK IF A SPACE                             
         BE    *+10                                                             
         OC    MNMSDAYS,BYTE       NO, SO OR ON THE DAY SPOT WILL RUN           
         ZIC   RE,BYTE             SHIFT BIT FOR NEXT DAY                       
         SLL   RE,1                                                             
         STC   RE,BYTE                                                          
         BCTR  R1,0                                                             
         BCT   R5,MKGMSS30                                                      
*                                                                               
         PACK  DUB,MOMSRSDT        ROTATION START DATE                          
         CVB   R5,DUB                                                           
         STC   R5,MNMSOROT                                                      
*                                                                               
         LA    R5,MOMSSTIM         FIELD OF INPUT START TIME                    
         LA    R6,MNMSSTIM         FIELD IN ELEMENT TO STORE START TIME         
         BRAS  RE,VALTIME          GO VALIDATE TIME                             
*                                                                               
         CLC   MOMSETIM,MOMSSTIM   END TIME SAME AS START TIME?                 
         BE    MKGMSS35            YES, LEAVE END TIME AS NULLS                 
         CLC   MOMSETIM,SPACE                                                   
         BE    MKGMSS35                                                         
         LA    R5,MOMSETIM         FIELD OF INPUT END TIME                      
         LA    R6,MNMSETIM         FIELD IN ELEMENT TO STORE END TIME           
         BRAS  RE,VALTIME          GO VALIDATE TIME                             
*                                                                               
MKGMSS35 PACK  DUB,MOMSTSLN        TOTAL SPOT LENGTH                            
         CVB   R5,DUB                                                           
         STC   R5,MNMSTSLN                                                      
*                                                                               
         CLI   MOMSLUNT,C'M'       MINUTES?                                     
         BE    MKGMSS40                                                         
         CLI   MOMSLUNT,C'S'       SECONDS?                                     
         BE    MKGMSS40                                                         
         CLI   MOMSLUNT,C' '                                                    
         JNE   INVLENUN                                                         
         MVI   MNMSUNIT,C'S'       DEFAULT IS SECONDS                           
         B     *+10                                                             
*                                                                               
MKGMSS40 MVC   MNMSUNIT,MOMSLUNT   LENGTH UNITS                                 
*                                                                               
         LA    RE,MOMSCOST                                                      
         ST    RE,DMCB                                                          
         MVI   DMCB,C'N'                                                        
         OI    DMCB,X'80'                                                       
         GOTOR VCASHVAL,DMCB,,9   COST                                          
         CLI   0(R1),0                     CHECK RETURN CODE                    
         JNE   INVNMFLD                                                         
         MVC   MNMSCOST,5(R1)              LAST 3 BYTES ONLY                    
*                                                                               
         LA    R1,MOMSPGNM+L'MOMSPGNM-1  LAST POSSIBLE CHARACTER IN THE         
         LA    RF,MOMSPGNM               PROGRAM NAME                           
MKGMSS50 CR    RF,R1                                                            
         BE    MKGMSS60                                                         
         CLI   0(R1),C' '                                                       
         BH    *+8                                                              
         BCT   R1,MKGMSS50                                                      
*                                                                               
         SR    R1,RF               LENGTH OF TEXT - 1                           
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   MNMSTEXT(0),MOMSPGNM                                             
*                                                                               
         AHI   R1,MNMSOVRH+1       ADD LENGTH OF OVERHEAD+1                     
         STC   R1,MNMSLEN                                                       
*                                                                               
MKGMSS60 GOTOR ,DMCB,(RECNSEQ,AIO2),(NEXISTS,0)                                 
         CLI   QSTATN,C'0'         CABLE?                                       
         BL    MKGMSS65                                                         
         TM    RECFLAG2,MNWINPRG   MAKEGOOD MNW IN PROGRESS                     
         JZ    MSCBLMNW             -ERROR, MISSING CABLE MKGMNW                
         GOTOR ,DMCB,(RECCSEQ,AIO4),(CEXISTS,0)                                 
*                                                                               
MKGMSS65 GOTOR ADDEL,DMCB                                                       
         BNE   NO                                                               
MKGMSSX  B     YES                                                              
         DROP  R2,R4                                                            
         EJECT                                                                  
***********************************************************************         
*   MKGONW: CABLE NETWORK - PRECEDES GROUP OF MKGBUY FOR THAT NETWORK           
***********************************************************************         
MKGONW   NTR1                                                                   
         TM    RECFLAG,INPROG      MAKEGOOD RECORDS IN PROGRESS?                
         JNO   BADSQMGO                                                         
*                                                                               
         CLI   QSTATN,C'0'         ONLY CABLE SUPPORTS MKGONW RECORD            
         JL    NOTCABLE            - SEND ERROR IF NOT CABLE                    
*                                                                               
         CLI   LASTREC,HDR         MAKEGOOD HEADER                              
         BE    MKGON010                                                         
         CLI   LASTREC,DS1         OFFER COMMENT                                
         BE    MKGON010                                                         
         CLI   LASTREC,DS2         OFFER DEMO CATEGORIES                        
         BE    MKGON010                                                         
*                                                                               
         CLI   LASTREC,CDC         COMSCORE DEMO CATEGORIES                     
         BE    MKGON010                                                         
*                                                                               
         CLI   LASTREC,DTL         MAKEGOOD DETAIL                              
         BE    MKGON005                                                         
*                                                                               
         CLI   LASTREC,MSS         MISSED SPOT                                  
         BE    MKGON005                                                         
         CLI   LASTREC,MAAU        MAKEGOOD UUID?                               
         BE    MKGON005                                                         
         CLI   LASTREC,COM         COMMENT                                      
         BE    MKGON005                                                         
         CLI   LASTREC,ORB         ORBIT                                        
         JNE   BADSQMGO                                                         
*                                                                               
MKGON005 TM    RECFLAG2,MNWINPRG+ONWINPRG   MNW OR ONW IN PROGRESS?             
         BZ    MKGON010                                                         
         GOTOR RECWRT,DMCB,(RECCSEQ,AIO4),(CEXISTS,0)                           
         NI    RECFLAG2,X'FF'-MNWINPRG-ONWINPRG                                 
*                                                                               
         TM    RECFLAG2,DEMINPRG   MAKEGOOD SELLER DEMO IN PROGRESS             
         BZ    MKGON010                                                         
         OC    SVMODMEL,SVMODMEL   DID SELLER SEND MKGDS2?                      
         BNZ   MKGON007                                                         
         OC    SVNTDCEL,SVNTDCEL             OR    MKGCDC?                      
         BZ    MKGON010                                                         
MKGON007 GOTOR RECWRT,DMCB,('MOXKSDMQ',AIO5),(CODEXIST,0)                       
*                                                                               
MKGON010 DS    0H                                                               
***NOP   MVI   LASTREC,ONW                                                      
*                                                                               
         L     R1,MKGCOUNT         INCREMENT RECORD COUNT                       
         LA    R1,1(R1)                                                         
         ST    R1,MKGCOUNT                                                      
*                                                                               
         LA    R4,R+1              SET INPUT RECORD DSECT                       
         USING MOFRONWD,R4                                                      
*                                                                               
         CLC   ORDNO,MOONORDR      ORDER #                                      
         JNE   INCNDATA                                                         
*********                                                                       
*    CHECK TO INSURE DARE ORDER'S NETWORK EXIST                                 
*********                                                                       
         USING STAPACKD,R6                                                      
         LA    R6,WORK                                                          
         XC    0(32,R6),0(R6)                                                   
         MVI   STAPACT,C'Y'        TRANSLATE NETWORK NIELSEN 4 TO DDS 3         
         MVI   STAPCTRY,C'U'       UNITED STATES                                
         MVC   STAPACOM,ACOMFACS                                                
         MVC   STAPAGY,QAGY                                                     
         MVI   STAPMED,C'T'                                                     
         XC    STAPQMKT,STAPQMKT                                                
         MVC   STAPQSTA(4),MOONNTWK                                             
*                                                                               
         GOTOR ASTAPACK,WORK       IF NO ERROR, STAPQNET IS SET                 
         CLC   STAPQNET,SPACE        STAPQNET SET?                              
         JE    INVCBLST              NO, ERROR                                  
*                                                                               
         MVC   STAPQMKT,FOXZEROS                                                
         MVI   STAPACT,C'P'        MSPACK                                       
         MVC   STAPQSTA,QSTATN                                                  
*                                                                               
         GOTOR ASTAPACK,WORK                                                    
         CLI   STAPERR,0                                                        
         JNE   INVCBLST                                                         
                                                                                
         MVI   CEXISTS,0                                                        
*                                                                               
         LA    R2,KEY              MAKEGOOD OFFER RECORD                        
         USING DAREMGOD,R2                                                      
         MVC   KEY,OFFRKEY                                                      
         MVC   MOXKSTTN,STAPSTA                                                 
         MVI   MOXKSEQ,1                                                        
*                                                                               
         MVI   RECCSEQ,0                                                        
         MVI   DMINBTS,X'08'       RDHI FOR DELETED                             
         BRAS  RE,XHIGH                                                         
*                                                                               
MKGON020 CLC   KEYSAVE(MOXKSEQ-MOXKEY),KEY   MATCH ON NOTICE & NETWORK?         
         BNE   MKGON040                                                         
*                                                                               
         MVC   RECCSEQ,MOXKSEQ                                                  
         MVI   DMINBTS,X'08'       READ SEQ FOR DELETED                         
         BRAS  RE,XSEQ                                                          
         B     MKGON020                                                         
*                                                                               
MKGON040 MVC   KEY(L'OFFRKEY),OFFRKEY                                           
         MVC   MOXKSTTN,STAPSTA                                                 
         MVI   DMINBTS,X'08'                                                    
         BRAS  RE,XHIGH                                                         
*                                                                               
         CLC   KEYSAVE(L'MOXKEY),KEY                                            
         JNE   MKGON050                                                         
*                                                                               
         MVI   DMINBTS,X'88'                                                    
         L     R2,AIO4                                                          
         USING DAREMGOD,R2                                                      
         ST    R2,AIO                                                           
         BRAS  RE,XGET                                                          
*                                                                               
         OI    CEXISTS,EXONFILE    OFFER RECORD EXISTS                          
         TM    MOXRSTAT,X'80'      SHOULD BE DELETED?                           
         JZ    DUPMKONW             -NO, SEND ERROR                             
         OI    CEXISTS,EXDEL        YES                                         
*                                                                               
MKGON050 L     R2,AIO4             MAKEGOOD OFFER RECORD                        
         LR    RE,R2                                                            
         LA    RF,4000                                                          
         XCEFL                                                                  
         MVC   0(L'MOXKEY,R2),OFFRKEY                                           
         MVC   MOXKSTTN,STAPSTA                                                 
         LA    R1,MOXFRST-MOXKEY  LENGTH OF RECORD W/O ELEMENTS                 
         STCM  R1,3,MOXRLEN                                                     
         MVC   MOXRAGY,POWERCDE    ALPHA AGENCY CODE                            
         XC    ELTAREA,ELTAREA                                                  
         DROP  R2                                                               
*                                                                               
         OI    RECFLAG2,ONWINPRG   MAKEGOOD ONW IN PROGRESS                     
*********                                                                       
*    READ/SETUP MAKEGOOD OFFER SELLER DEMO X'FE' RECORD IN AIO5                 
*********                                                                       
         MVI   CODEXIST,0                                                       
*                                                                               
         LA    R2,KEY              MAKEGOOD OFFER RECORD                        
         USING DAREMGOD,R2                                                      
         MVC   KEY,OFFRKEY                                                      
         MVC   MOXKSTTN,STAPSTA                                                 
         MVI   MOXKDTYP,MOXKSDMQ   X'FE'                                        
*                                                                               
         MVI   DMINBTS,X'08'       RDHI FOR DELETED                             
         BRAS  RE,XHIGH                                                         
         CLC   KEYSAVE(L'MOXKEY),KEY                                            
         JNE   MKGON060                                                         
*                                                                               
         MVI   DMINBTS,X'88'                                                    
         L     R2,AIO5                                                          
         USING DAREMGOD,R2                                                      
         ST    R2,AIO                                                           
         BRAS  RE,XGET                                                          
*                                                                               
         OI    CODEXIST,EXONFILE   MKGD OFER SELLER DEMO EXISTS                 
         TM    MOXRSTAT,X'80'      SHOULD BE DELETED?                           
         JZ    DUPMKONW            -NO, SEND ERROR                              
         OI    CODEXIST,EXDEL      -YES'                                        
*                                                                               
MKGON060 L     R2,AIO5             MAKEGOOD OFFER RECORD                        
         LR    RE,R2                                                            
         LA    RF,4000                                                          
         XCEFL                                                                  
         MVC   0(L'MOXKEY,R2),OFFRKEY                                           
         MVC   MOXKSTTN,STAPSTA                                                 
         MVI   MOXKDTYP,MOXKSDMQ   X'FE'                                        
         LA    R1,MOXFRST-MOXKEY   LENGTH OF RECORD W/O ELEMENTS                
         STCM  R1,3,MOXRLEN                                                     
         MVC   MOXRAGY,POWERCDE    ALPHA AGENCY CODE                            
         XC    ELTAREA,ELTAREA                                                  
         DROP  R2                                                               
MKGONX   B     YES                                                              
***********************************************************************         
*   MKGBUY: MAKEGOOD BUY HEADER                                                 
***********************************************************************         
MKGBUY   NTR1                                                                   
         TM    RECFLAG,INPROG      MAKEGOOD RECORDS IN PROGRESS?                
         JNO   BADSQMGO                                                         
*                                                                               
         LA    R4,R+1              SET INPUT RECORD DSECT                       
         USING MOFRBUYD,R4                                                      
*                                                                               
         CLI   LASTREC,DTL         DETAIL LINE                                  
         BE    MKGBUY10             YES, PREVIOUS WAS A MKGBUY GROUP            
*                                                                               
         CLI   LASTREC,HDR         MAKEGOOD HEADER                              
         BE    MKGBUY12             -YES, MUST BE A BONUS                       
         CLI   LASTREC,DS1         OFFER COMMENT                                
         BE    MKGBUY12             -YES, MUST BE A BONUS                       
         CLI   LASTREC,DS2         OFFER DEMO CATEGORIES                        
         BE    MKGBUY12             -YES, MUST BE A BONUS                       
         CLI   LASTREC,CDC         COMSCORE DEMO CATEGORIES                     
         BE    MKGBUY12             -YES, MUST BE A BONUS                       
*                                                                               
***NOP   CLI   LASTREC,ONW         CABLE OFFER NETWORK                          
***NOP   BE    MKGBUY12                                                         
*                                                                               
         CLI   RCHDBUY,C'N'        HAVE WE REACHED A BUY YET?                   
         JNE   BADSQMGO                                                         
         CLI   LASTREC,MSS         MISSED SPOT                                  
         BE    MKGBUY12                                                         
         CLI   LASTREC,MAAU        MISSED AUTOMATED AVAIL UUID                  
         BE    MKGBUY12                                                         
         CLI   LASTREC,COM         MISSED COMMENT                               
         BE    MKGBUY12                                                         
         CLI   LASTREC,ORB         MISSED ORBIT                                 
         BE    MKGBUY12                                                         
         J     BADSQMGO                                                         
*                                                                               
MKGBUY10 DS    0H                                                               
         CLC   MOBYSEQN,SVOFFNUM   NEW OFFER? OR OFFER NUMBER CHANGE?           
         BE    MKGBUY15             NO, SAME MKGBUY OFFER NUMBER                
*                                                                               
MKGBUY12 MVC   SVOFFNUM,MOBYSEQN   SAVE MKGBUY OFFER NUMBER                     
         MVI   BUYCOUNT,0          INIT BUY COUNT BACK TO 0                     
         ZIC   R1,MGOFFNUM         INCREMENT OFFER NUMBER                       
         LA    R1,1(R1)                                                         
         STC   R1,MGOFFNUM                                                      
         MVC   OFFERNUM,MGOFFNUM                                                
*                                                                               
MKGBUY15 DS    0H                                                               
         XC    TBMKSPTS,TBMKSPTS   INIT TOTAL MG SPOTS PER MKGBUY               
         MVI   LASTREC,BUY         SET LAST RECORD TYPE                         
         MVI   RCHDBUY,C'Y'                                                     
*                                                                               
         ZIC   R1,BUYCOUNT         INCREMENT BUY COUNT                          
         LA    R1,1(R1)                                                         
         STC   R1,BUYCOUNT                                                      
*                                                                               
         L     R1,MKGCOUNT         INCREMENT RECORD COUNT                       
         LA    R1,1(R1)                                                         
         ST    R1,MKGCOUNT                                                      
*                                                                               
         CLC   ORDNO,MOBYORDR      ORDER #                                      
         JNE   INCNDATA                                                         
*                                                                               
         XC    ELTAREA,ELTAREA                                                  
         LA    R2,ELTAREA                                                       
         USING MOMBELD,R2                                                       
         MVI   MOMBEL,MOMBELQ      ELEMENT CODE                                 
         MVI   MOMBLEN,MOMBOVRH    OVERHEAD LENGTH                              
*                                                                               
         MVC   MOMBOFFR,OFFERNUM                                                
         MVC   MOMBREC,BUYCOUNT    RECORD NUMBER                                
*                                                                               
         LA    R1,MOBYROTN+6       LAST DAY OF ROTATION                         
         LA    R5,7                LOOP FOR 7 DAYS                              
         MVI   BYTE,X'01'          01 = SUN 02 = SAT 04 = FRI..40 = MON         
*                                                                               
MKGBUY20 CLI   0(R1),C' '          CHECK IF A SPACE                             
         BE    *+10                                                             
         OC    MOMBDAYS,BYTE       NO, SO OR ON THE DAY SPOT WILL RUN           
         ZIC   RE,BYTE             SHIFT BIT FOR NEXT DAY                       
         SLL   RE,1                                                             
         STC   RE,BYTE                                                          
         BCTR  R1,0                                                             
         BCT   R5,MKGBUY20                                                      
*                                                                               
         CLI   MOMBDAYS,0          IS DAYS VALID?                               
         JE    INVDAYS                                                          
*                                                                               
         PACK  DUB,MOBYRSDT        ROTATION START DATE                          
         CVB   R5,DUB                                                           
         STC   R5,MOMBOROT                                                      
         STC   R5,DAYOFWEK         START DAY OF WEEK                            
*                                                                               
         LA    R5,MOBYSTIM         FIELD OF INPUT START TIME                    
         LA    R6,MOMBSTIM         FIELD IN ELEMENT TO STORE START TIME         
         BRAS  RE,VALTIME          GO VALIDATE TIME                             
*                                                                               
         CLC   MOBYETIM,SPACE                                                   
         BE    MKGBUY29                                                         
         LA    R5,MOBYETIM         FIELD OF INPUT END TIME                      
         LA    R6,MOMBETIM         FIELD IN ELEMENT TO STORE END TIME           
         BRAS  RE,VALTIME          GO VALIDATE TIME                             
*                                                                               
         MVC   FULL,MOMBSTIM       MODIFY TIMES SO WE CAN VALIDATE              
         XR    RE,RE                                                            
         ICM   RE,3,FULL                                                        
         CHI   RE,2400                                                          
         BNH   MKGBUY22                                                         
         SHI   RE,2400                                                          
         STCM  RE,3,FULL                                                        
*                                                                               
MKGBUY22 XR    RE,RE                                                            
         ICM   RE,3,FULL+2                                                      
         CHI   RE,2400                                                          
         BNH   MKGBUY24                                                         
         SHI   RE,2400                                                          
         STCM  RE,3,FULL+2                                                      
*                                                                               
MKGBUY24 CLC   FULL(2),FULL+2      START TIME > END TIME?                       
         BL    MKGBUY29            NO, TYPICAL CASE WHERE END TIME IS >         
         BE    MKGBUY26            END TIME IS SAME                             
* DARE DOES NOT LIKE THIS TIME PERIOD                                           
         CHI   RE,600              YES, THEN END TIME HAS TO BE < 6AM           
         BL    MKGBUY29                                                         
         J     INVTMPRD                                                         
*                                                                               
MKGBUY26 XC    MOMBETIM,MOMBETIM   YES, DON'T NEED END TIME THEN                
         B     MKGBUY29                                                         
*                                                                               
MKGBUY29 PACK  DUB,MOBYTSLN        TOTAL SPOT LENGTH                            
         CVB   R5,DUB                                                           
         STC   R5,MOMBTSLN                                                      
*                                                                               
         CLI   MOBYLUNT,C'M'       MINUTES?                                     
         BE    MKGBUY30                                                         
         CLI   MOBYLUNT,C'S'       SECONDS?                                     
         BE    MKGBUY30                                                         
         CLI   MOBYLUNT,C' '                                                    
         JNE   INVLENUN                                                         
         MVI   MOMBUNIT,C'S'       DEFAULT IS SECONDS                           
         B     *+10                                                             
*                                                                               
MKGBUY30 MVC   MOMBUNIT,MOBYLUNT   LENGTH UNITS                                 
*                                                                               
         LA    RE,MOBYCOST                                                      
         ST    RE,DMCB                                                          
         MVI   DMCB,C'N'           OUTPUT IN BINARY                             
         GOTOR VCASHVAL,DMCB,,9    COST                                         
         CLI   0(R1),0                     CHECK RETURN CODE                    
         JNE   INVNMFLD                                                         
         MVC   MOMBCOST,5(R1)              LAST 3 BYTES ONLY                    
*                                                                               
         CLC   MOBYTMSH,SPACE                                                   
         BE    MKGBUY35                                                         
         LA    RE,MOBYTMSH                                                      
         ST    RE,DMCB                                                          
         MVI   DMCB,C'N'           OUTPUT IN BINARY                             
         GOTOR VCASHVAL,DMCB,,3    TIME SHARE                                   
         CLI   0(R1),0                     CHECK RETURN CODE                    
         JNE   INVNMFLD                                                         
         MVC   MOMBPTIM,7(R1)              LAST 1 BYTE ONLY                     
*&&DO                                                                           
         PACK  DUB,MOBYTMSH        PRODUCT 1 TIME SHARE                         
         CVB   R5,DUB                                                           
         STC   R5,MOMBPTIM                                                      
*&&                                                                             
*                                                                               
MKGBUY35 CLI   MOBYSTYP,C'D'       DAILY SCHEDULE?                              
         BE    MKGBUY40                                                         
         CLI   MOBYSTYP,C'W'       WEEKLY?                                      
         BE    MKGBUY40                                                         
         CLI   MOBYSTYP,C' '                                                    
         JNE   INVLENUN                                                         
         MVI   MOMBSTYP,C'W'       DEFAULT IS WEEKLY                            
         B     *+10                                                             
*                                                                               
MKGBUY40 MVC   MOMBSTYP,MOBYSTYP           SCHEDULE TYPE                        
*                                                                               
         LA    R1,MOBYPGNM+L'MOBYPGNM-1    LAST POSSIBLE CHARACTER IN           
         LA    RF,MOBYPGNM                 THE PROGRAM NAME                     
MKGBUY50 CR    RF,R1                                                            
         BE    MKGBUY60                                                         
         CLI   0(R1),C' '                                                       
         BH    *+8                                                              
         BCT   R1,MKGBUY50                                                      
*                                                                               
         SR    R1,RF                                                            
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   MOMBPROG(0),MOBYPGNM                                             
*                                                                               
         AHI   R1,MOMBOVRH+1       ADD LENGTH OF OVERHEAD+1                     
         STC   R1,MOMBLEN                                                       
         DROP  R2                                                               
*                                                                               
MKGBUY60 GOTOR ,DMCB,(RECOSEQ,AIO1),(OEXISTS,0)                                 
         CLI   QSTATN,C'0'         CABLE?                                       
         BL    MKGBUY65                                                         
         TM    RECFLAG2,ONWINPRG   MAKEGOOD ONW IN PROGRESS                     
         JZ    MSCBLONW             -ERROR, MISSING CABLE MKGONW                
         GOTOR ,DMCB,(RECCSEQ,AIO4),(CEXISTS,0)                                 
*                                                                               
MKGBUY65 GOTOR ADDEL,DMCB                                                       
         BNE   NO                                                               
MKGBUYX  B     YES                                                              
         EJECT                                                                  
***********************************************************************         
*   MKGAAU: MAKEGOOD BUY AUTOMATED AVAIL UUID                                   
***********************************************************************         
MKGAAU   NTR1                                                                   
         TM    RECFLAG,INPROG      MAKEGOOD RECORDS IN PROGRESS?                
         JNO   BADSQMGO                                                         
*                                                                               
         CLI   LASTREC,BUY         BUY HEADER?                                  
         JE    MKGAAU00                                                         
         CLI   LASTREC,MSS         OR MISSED LINE?                              
         JNE   BADSQMGO                                                         
*                                                                               
MKGAAU00 L     R1,MKGCOUNT         INCREMENT RECORD COUNT                       
         LA    R1,1(R1)                                                         
         ST    R1,MKGCOUNT                                                      
*                                                                               
         LA    R4,R+1              SET INPUT RECORD DSECT                       
         USING MOFRAAUD,R4                                                      
*                                                                               
         CLI   LASTREC,MSS         AAU FOR MISSED LINE?                         
         JE    MKGAAU50                                                         
*                                                                               
         MVI   LASTREC,OAAU        OFFERED LINE AAU                             
         XC    ELTAREA,ELTAREA                                                  
         LA    R2,ELTAREA                                                       
         USING MOMBAAUD,R2                                                      
         MVI   MOMBAEL,MOMBAELQ    X'22'                                        
         MVC   MOMBAOFR,OFFERNUM   OFFER NUMBER                                 
         MVC   MOMBAREC,BUYCOUNT   RECORD NUMBER (BUY HEADER COUNT)             
         MVC   MOMBAAU(L'MAAUTEXT),MAAUTEXT   VARIABLE LENGTH TEXT              
         LA    RE,MOMBAAU+L'MAAUTEXT-1                                          
MKGAAU10 CLI   0(RE),C' '          LAST CHARACTER IN TEXT > ' '?                
         JH    MKGAAU15            YES, WE CAN CALC L(ELEMENT)                  
         BCTR  RE,0                                                             
         CR    RE,R2                                                            
         JNH   *+2                 DIE IF WE'RE BEFORE BEG(ELEMENT)             
         J     MKGAAU10                                                         
*                                                                               
MKGAAU15 LA    RE,1(RE)                                                         
         SR    RE,R2               NO WE KNOW THE TRUE LENGTH                   
         STC   RE,MOMBALEN                                                      
*                                                                               
MKGAAU20 GOTOR ,DMCB,(RECOSEQ,AIO1),(OEXISTS,0)                                 
         CLI   QSTATN,C'0'         CABLE?                                       
         BL    MKGAAUX                                                          
         TM    RECFLAG2,ONWINPRG   MAKEGOOD ONW IN PROGRESS                     
         JZ    MSCBLONW             -ERROR, MISSING CABLE MKGONW                
         GOTOR ,DMCB,(RECCSEQ,AIO4),(CEXISTS,0)                                 
         J     MKGAAUX                                                          
*                                                                               
MKGAAU50 MVI   LASTREC,MAAU        MISSED LINE AAU                              
         XC    ELTAREA,ELTAREA                                                  
         LA    R2,ELTAREA                                                       
         USING MNMSAAUD,R2                                                      
         MVI   MNMSAEL,MNMSAELQ    X'12'                                        
         MVC   MNMSAOFR,OFFERNUM   TO MIMIC COMMENTS                            
         MVC   MNMSAREC,MISSSEQ    EACH MISSED IS DISTINCT                      
         MVC   MNMSAAU(L'MAAUTEXT),MAAUTEXT   VARIABLE LENGTH TEXT              
         LA    RE,MNMSAAU+L'MAAUTEXT-1                                          
MKGAAU55 CLI   0(RE),C' '          LAST CHARACTER IN TEXT > ' '?                
         JH    MKGAAU60            YES, WE CAN CALC L(ELEMENT)                  
         BCTR  RE,0                                                             
         CR    RE,R2                                                            
         JNH   *+2                 DIE IF WE'RE BEFORE BEG(ELEMENT)             
         J     MKGAAU55                                                         
*                                                                               
MKGAAU60 LA    RE,1(RE)                                                         
         SR    RE,R2               NO WE KNOW THE TRUE LENGTH                   
         STC   RE,MNMSALEN                                                      
*                                                                               
MKGAAU70 GOTOR ,DMCB,(RECNSEQ,AIO2),(NEXISTS,0)                                 
         CLI   QSTATN,C'0'         CABLE?                                       
         BL    MKGAAUX                                                          
         TM    RECFLAG2,MNWINPRG   MAKEGOOD MNW IN PROGRESS                     
         JZ    MSCBLMNW             -ERROR, MISSING CABLE MKGMNW                
         GOTOR ,DMCB,(RECCSEQ,AIO4),(CEXISTS,0)                                 
*                                                                               
MKGAAUX  GOTOR ADDEL,DMCB                                                       
         BNE   NO                                                               
         B     YES                                                              
         DROP  R4,R2                                                            
         B     YES                                                              
         EJECT                                                                  
***********************************************************************         
*   MKGDEM: MAKEGOOD BUY DEMOS                                                  
***********************************************************************         
MKGDEM   NTR1                                                                   
         TM    RECFLAG,INPROG      MAKEGOOD RECORDS IN PROGRESS?                
         JNO   BADSQMGO                                                         
*                                                                               
         CLI   LASTREC,BUY         BUY HEADER                                   
         JE    *+12                                                             
         CLI   LASTREC,OAAU        OFFERED AUTOMATED AVAIL UUID                 
         JNE   BADSQMGO                                                         
         MVI   LASTREC,DEM                                                      
*                                                                               
         L     R1,MKGCOUNT         INCREMENT RECORD COUNT                       
         LA    R1,1(R1)                                                         
         ST    R1,MKGCOUNT                                                      
*                                                                               
         LA    R4,R+1              SET INPUT RECORD DSECT                       
         USING MOFRDEMD,R4                                                      
*                                                                               
         OC    SVMODMEL,SVMODMEL   DID SELLER SEND MKGDS2?                      
         JZ    INVMKGDM             NO, SEND ERROR                              
         XC    ELTAREA,ELTAREA                                                  
         LA    R2,ELTAREA                                                       
         USING MODMELD,R2                                                       
         MVC   ELTAREA(L'SVMODMEL),SVMODMEL  SELLER DEMO VALUES ELEM            
         MVC   MODMOFFR,OFFERNUM   OFFER NUMBER                                 
         MVC   MODMREC,BUYCOUNT    RECORD NUMBER (BUY HEADER COUNT)             
*                                                                               
         MVI   RCHEMPTD,C'N'       EMPTY DEMO FLAG                              
         LA    R5,4                                                             
MKGDM020 CLC   MODMVAL,FOXZEROS    HAVE A VALID NUMERIC VALUE?                  
         BNL   MKGDM040                                                         
         CLC   MODMVAL,SPACE       EMPTY FIELD?                                 
         JNE   INVMKGDM             YES                                         
         OC    MDEMNO,MDEMNO       NO VALUE, HAVE CATEGORY?                     
         JNZ   INVMKGDM             YES, SEND INVALID MKGDEM ERROR              
         MVI   RCHEMPTD,C'Y'        TURN ON EMPTY DEMO FLAG                     
         B     MKGDM060                                                         
*                                                                               
MKGDM040 CLI   RCHEMPTD,C'Y'       RECEIVED EMPTY DEMO ALREADY?                 
         JE    INVMKGDM             YES, SEND INVALID MKGDEM ERROR              
         PACK  DUB,MODMVAL         DEMO VALUE                                   
         CVB   RE,DUB                                                           
         ST    RE,MDEMRAW                                                       
         OI    MDEMRAW,MODMOVRD    SET X'80' OVERRIDE BIT                       
         CLI   MDEMNO+1,C'R'       RATING?                                      
         BNE   MKGDM060                                                         
         CLI   SVMG2DEC,C'Y'       2-DEC RATING?                                
         BNE   MKGDM060                                                         
         OI    MDEMRAW,MODM2DEC    SET X'40' 2-DEC VALUE                        
MKGDM060 LA    R2,L'MODMDEMO(R2)                                                
         LA    R4,L'MODMVAL(R4)                                                 
         BCT   R5,MKGDM020                                                      
*                                                                               
         GOTOR ,DMCB,(RECOSEQ,AIO1),(OEXISTS,0)                                 
         CLI   QSTATN,C'0'         CABLE?                                       
         BL    MKGDEMX                                                          
         TM    RECFLAG2,ONWINPRG   MAKEGOOD ONW IN PROGRESS                     
         JZ    MSCBLONW             -ERROR, MISSING CABLE MKGONW                
         GOTOR ,DMCB,('MOXKSDMQ',AIO5),(CODEXIST,0)                             
         OI    RECFLAG2,DEMINPRG   MAKEGOOD SELLER DEMO IN PROGRESS             
*                                                                               
MKGDEMX  GOTOR ADDEL,DMCB                                                       
         BNE   NO                                                               
         B     YES                                                              
         DROP  R4,R2                                                            
         B     YES                                                              
         EJECT                                                                  
***********************************************************************         
*   MKGORB: MAKEGOOD ORDER ORBIT                                                
***********************************************************************         
MKGORB   NTR1                                                                   
         TM    RECFLAG,INPROG      MAKEGOOD RECORDS IN PROGRESS?                
         JNO   BADSQMGO                                                         
*                                                                               
         CLI   LASTREC,ORB         ORDER ORBIT                                  
         BE    MKGORB10                                                         
*                                                                               
         MVI   ORBCOUNT,0          INITIALIZE ORBIT                             
         CLI   LASTREC,BUY         BUY HEADER                                   
         BE    MKGORB10                                                         
         CLI   LASTREC,MAAU        MISSED AUTO-AVAIL UUID?                      
         BE    MKGORB10                                                         
         CLI   LASTREC,OAAU        OFFERED AUTO-AVAIL UUID?                     
         BE    MKGORB10                                                         
         CLI   RCHDBUY,C'N'        REACHED A BUY YET?                           
         JNE   BADSQMGO                                                         
         CLI   LASTREC,MSS         MISSED SPOT                                  
         JNE   BADSQMGO                                                         
*                                                                               
MKGORB10 MVI   LASTREC,ORB         SET LAST RECORD TYPE                         
         LLC   R1,ORBCOUNT         INCREMENT BUY COUNT                          
         LA    R1,1(R1)                                                         
         CHI   R1,8                LIMIT OF 8 ORBITS PER OFFERED LINE           
         JH    ORBLIMIT                                                         
         STC   R1,ORBCOUNT                                                      
*                                                                               
         L     R1,MKGCOUNT         INCREMENT RECORD COUNT                       
         LA    R1,1(R1)                                                         
         ST    R1,MKGCOUNT                                                      
*                                                                               
         LA    R4,R+1              SET INPUT RECORD DSECT                       
         USING MORDORBD,R4                                                      
*                                                                               
         CLC   ORDNO,MORBORDR      ORDER #                                      
         JNE   INCNDATA                                                         
*                                                                               
* ADD ELEMENT THE SAME FOR A MISSED SPOT OR MAKEGOOD ORBIT DESCRIPTION          
         XC    ELTAREA,ELTAREA                                                  
         LA    R2,ELTAREA                                                       
***********************************                                             
* MISSED ORBIT DSECT AND MAKEGOOD ORBIT DSECT ARE VERY SIMILAR                  
*   AT WHERE THE PROGRAM DESCRIPTION WHERE IT DIFFERS SLIGHTLY                  
***********************************                                             
         USING MORNELD,R2                                                       
         CLI   RCHDBUY,C'Y'        REACHED A BUY YET?                           
         BE    MKGORB13            YES, SO THIS IS FOR OFFER RECORD             
         MVI   MORNEL,MNMOELQ      MISSED SPOT ELEMENT CODE                     
         MVI   MORNLEN,MORNDEM-MORNELD  MISSED DOES NOT HAVE DEMO               
         B     MKGORB16                                                         
*                                                                               
MKGORB13 MVI   MORNEL,MORNELQ      MAKEGOOD ELEMENT CODE                        
         MVI   MORNLEN,MORNDEM+L'MORNDEM-MORNELD                                
*                                                                               
MKGORB16 MVC   MORNOFFR,OFFERNUM                                                
         MVC   MORNREC,BUYCOUNT    RECORD NUMBER (BUY HEADER COUNT)             
*                                                                               
         LA    R1,MORBPDAY+6       LAST DAY OF ROTATION                         
         LA    R5,7                LOOP FOR 7 DAYS                              
         MVI   BYTE,X'01'          01 = SUN 02 = SAT 04 = FRI..40 = MON         
*                                                                               
MKGORB20 CLI   0(R1),C' '          CHECK IF A SPACE                             
         BE    *+10                                                             
         OC    MORNDAYS,BYTE       NO, SO OR ON DAY SPOT WILL RUN               
         ZIC   RE,BYTE             SHIFT BIT FOR NEXT DAY                       
         SLL   RE,1                                                             
         STC   RE,BYTE                                                          
         BCTR  R1,0                                                             
         BCT   R5,MKGORB20                                                      
*                                                                               
         PACK  DUB,MORBPSDY        ROTATION START DATE                          
         CVB   R5,DUB                                                           
         STC   R5,MORNSDAY                                                      
*                                                                               
         LA    R5,MORBPSTI         FIELD OF INPUT START TIME                    
         LA    R6,MORNSTIM         FIELD IN ELEMENT TO STORE START TIME         
         BRAS  RE,VALTIME          GO VALIDATE TIME                             
*                                                                               
         CLC   MORBPETI,SPACE                                                   
         BE    MKGORB30                                                         
         LA    R5,MORBPETI         FIELD OF INPUT END TIME                      
         LA    R6,MORNETIM         FIELD IN ELEMENT TO STORE END TIME           
         BRAS  RE,VALTIME          GO VALIDATE TIME                             
***************                                                                 
MKGORB30 LA    R1,MORBPGRM+L'MORBPGRM-1  LAST POSSIBLE CHARACTER IN THE         
         LA    RF,MORBPGRM               PROGRAM NAME                           
MKGORB40 CR    RF,R1                                                            
         BE    MKGORBX                                                          
         CLI   0(R1),C' '                                                       
         BH    *+8                                                              
         BCT   R1,MKGORB40                                                      
*                                                                               
         SR    R1,RF               LENGTH OF PROGRAM NAME - 1                   
         CHI   R1,6                MORE THAN 7 CHARACTERS?                      
         BNH   *+8                                                              
         LHI   R1,6                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   MORNPROG(0),MORBPGRM                                             
         OC    MORNPROG(7),SPACE                                                
         DROP  R2                                                               
*                                                                               
MKGORBX  GOTOR ,DMCB,(RECCSEQ,AIO4),(CEXISTS,0)                                 
         CLI   QSTATN,C'0'         CABLE?                                       
         BL    MKGORBX2                                                         
         TM    RECFLAG2,MNWINPRG+ONWINPRG                                       
         JNZ   MKGORBX5                                                         
         CLI   RCHDBUY,C'Y'                                                     
         JNE   MSCBLMNW                                                         
         J     MSCBLONW                                                         
*                                                                               
MKGORBX2 GOTOR ,DMCB,(RECOSEQ,AIO1),(OEXISTS,0)   MKGD OFFER RECORD             
         CLI   RCHDBUY,C'Y'                                                     
         BE    MKGORBX5                                                         
         GOTOR ,DMCB,(RECNSEQ,AIO2),(NEXISTS,0)   MKGD NOTICE RECORD            
                                                                                
MKGORBX5 GOTOR ADDEL,DMCB                                                       
         BNE   NO                                                               
         B     YES                                                              
         EJECT                                                                  
***********************************************************************         
*   MKGCOM: MAKEGOOD COMMENT                                                    
***********************************************************************         
MKGCOM   NTR1                                                                   
         TM    RECFLAG,INPROG      MAKEGOOD RECORDS IN PROGRESS?                
         JNO   BADSQMGO                                                         
*                                                                               
         CLI   LASTREC,COM         MAKEGOOD COMMENT                             
         BE    MKGCOM20                                                         
*                                                                               
         CLI   LASTREC,ORB         ORDER ORBIT                                  
         BE    MKGCOM10                                                         
         CLI   LASTREC,BUY         BUY HEADER                                   
         BE    MKGCOM10                                                         
         CLI   LASTREC,MAAU        MISSED AUTO-AVAILS UUID?                     
         BE    MKGCOM10                                                         
         CLI   LASTREC,OAAU        OFFERED AUTO-AVAILS UUID?                    
         BE    MKGCOM10                                                         
         CLI   LASTREC,DEM         DEMO                                         
         BE    MKGCOM10                                                         
         CLI   LASTREC,CDV         COMSCORE DEMO VALUES                         
         BE    MKGCOM10                                                         
*                                                                               
         CLI   RCHDBUY,C'N'        REACHED A BUY YET?                           
         JNE   BADSQMGO                                                         
         CLI   LASTREC,MSS         MISSED SPOT                                  
         JNE   BADSQMGO                                                         
*                                                                               
MKGCOM10 CLI   RCHDBUY,C'N'        REACHED A MKGBUY YET?                        
         BE    *+8                 NO, NOT YET                                  
         MVI   COMCOUNT,0          INITIALIZE COMMENT COUNT                     
*                                                                               
         MVI   LASTREC,COM         SET LAST RECORD TYPE                         
*                                                                               
MKGCOM20 L     R1,MKGCOUNT         INCREMENT RECORD COUNT                       
         LA    R1,1(R1)                                                         
         ST    R1,MKGCOUNT                                                      
*                                                                               
         LA    R4,R+1              SET INPUT RECORD DSECT                       
         USING MOFRBCMD,R4                                                      
*                                                                               
         CLC   ORDNO,MOBCORDR      ORDER #                                      
         JNE   INCNDATA                                                         
*                                                                               
* ADD ELEMENT THE SAME FOR A MISSED SPOT OR MAKEGOOD ORBIT COMMENT              
         XC    ELTAREA,ELTAREA                                                  
         LA    R2,ELTAREA                                                       
         USING MOBCELD,R2                                                       
*                                                                               
         CLI   RCHDBUY,C'Y'        REACHED A BUY YET?                           
         BE    MKGCOM22            YES, SO THIS IS FOR OFFER RECORD             
         MVI   MOBCEL,MNMCELQ      MISSED SPOT ELEMENT CODE                     
         MVI   MOBCLEN,MNMCOVRH    OVERHEAD LENGTH                              
         MVC   MOBCOFFR,OFFERNUM                                                
         MVC   MOBCREC,MISSSEQ     MISSED SPOT NUMBER (MKGMSS COUNT)            
         B     MKGCOM27                                                         
*                                                                               
MKGCOM22 MVI   MOBCEL,MOBCELQ      MAKEGOOD ELEMENT CODE                        
         MVI   MOBCLEN,MOBCOVRH    OVERHEAD LENGTH                              
         MVC   MOBCOFFR,OFFERNUM                                                
         MVC   MOBCREC,BUYCOUNT    RECORD NUMBER (BUY HEADER COUNT)             
*                                                                               
MKGCOM27 ZIC   R1,COMCOUNT         INCREMENT COMMENT COUNT                      
         LA    R1,1(R1)                                                         
         STC   R1,COMCOUNT                                                      
         STC   R1,MOBCLINE         STORE COMMENT LINE #                         
*                                                                               
         LA    R1,MOBCCOMT+L'MOBCCOMT-1  FIND LAST NON-BLANK CHARACTER          
         LA    RF,MOBCCOMT               IN THE COMMENT                         
MKGCOM30 CR    R1,RF                                                            
         BE    MKGCOMX                                                          
         CLI   0(R1),C' '                                                       
         BH    *+8                                                              
         BCT   R1,MKGCOM30                                                      
*                                                                               
         SR    R1,RF               LENGTH OF COMMENT - 1                        
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   MOBCTEXT(0),MOBCCOMT                                             
*                                                                               
         AHI   R1,MOBCOVRH+1       ADD LENGTH OF OVERHEAD+1                     
         STC   R1,MOBCLEN                                                       
         DROP  R2                                                               
*                                                                               
MKGCOMX  GOTOR ,DMCB,(RECCSEQ,AIO4),(CEXISTS,0)                                 
         CLI   QSTATN,C'0'         CABLE?                                       
         BL    MKGCOMX2                                                         
         TM    RECFLAG2,MNWINPRG+ONWINPRG                                       
         JNZ   MKGCOMX5                                                         
         CLI   RCHDBUY,C'Y'                                                     
         JNE   MSCBLMNW                                                         
         J     MSCBLONW                                                         
*                                                                               
MKGCOMX2 GOTOR ,DMCB,(RECOSEQ,AIO1),(OEXISTS,0)   MKGD OFFER RECORD             
         CLI   RCHDBUY,C'Y'                                                     
         BE    MKGCOMX5                                                         
         GOTOR ,DMCB,(RECNSEQ,AIO2),(NEXISTS,0)   MKGD NOTICE RECORD            
                                                                                
MKGCOMX5 GOTOR ADDEL,DMCB                                                       
         BNE   NO                                                               
         B     YES                                                              
         EJECT                                                                  
***********************************************************************         
*   MKGDTL: MAKEGOOD DETAIL LINE                                                
***********************************************************************         
MKGDTL   NTR1                                                                   
         TM    RECFLAG,INPROG      MAKEGOOD RECORDS IN PROGRESS?                
         JNO   BADSQMGO                                                         
*                                                                               
         CLI   LASTREC,DTL         DETAIL LINE                                  
         BE    MKGDTL20                                                         
*                                                                               
         CLI   LASTREC,COM         COMMENT                                      
         BE    MKGDTL10                                                         
         CLI   LASTREC,ORB         ORDER ORBIT                                  
         BE    MKGDTL10                                                         
         CLI   LASTREC,DEM         DEMO                                         
         BE    MKGDTL10                                                         
         CLI   LASTREC,CDV         COMSCORE DEMO VALUES                         
         BE    MKGDTL10                                                         
         CLI   LASTREC,OAAU        OFFERED AUTO-AVAILS UUID                     
         BE    MKGDTL10                                                         
         CLI   LASTREC,BUY         BUY HEADER                                   
         JNE   BADSQMGO                                                         
*                                                                               
MKGDTL10 MVI   DTLCOUNT,0                                                       
         MVI   LASTREC,DTL         SET LAST RECORD TYPE                         
*                                                                               
MKGDTL20 L     R1,MKGCOUNT         INCREMENT RECORD COUNT                       
         LA    R1,1(R1)                                                         
         ST    R1,MKGCOUNT                                                      
*                                                                               
         LA    R4,R+1              SET INPUT RECORD DSECT                       
         USING MOFRBDTD,R4                                                      
*                                                                               
         CLC   ORDNO,MOBDORDR      ORDER #                                      
         JNE   INCNDATA                                                         
*                                                                               
         XC    ELTAREA,ELTAREA                                                  
         LA    R2,ELTAREA                                                       
         USING MOBDELD,R2                                                       
         MVI   MOBDEL,MOBDELQ      ELEMENT CODE                                 
         MVI   MOBDLEN,MOBDLNQ     ELEMENT LENGTH                               
*                                                                               
         MVC   MOBDOFFR,OFFERNUM   OFFER NUMBER                                 
         MVC   MOBDREC,BUYCOUNT    RECORD NUMBER (BUY HEADER COUNT)             
*                                                                               
         ZIC   R1,DTLCOUNT         INCREMENT DETAIL COUNT                       
         LA    R1,1(R1)                                                         
         STC   R1,DTLCOUNT                                                      
         STC   R1,MOBDSEQ          STORE COMMENT LINE #                         
*                                                                               
         LA    RE,MOBDSCST                                                      
         ST    RE,DMCB                                                          
         MVI   DMCB,C'N'                                                        
         OI    DMCB,X'80'                                                       
         GOTOR VCASHVAL,DMCB,,9    COST                                         
         CLI   0(R1),0             CHECK RETURN CODE                            
         JNE   INVNMFLD                                                         
*                                                                               
         MVC   MOBDCOST,5(R1)      LAST 3 BYTES ONLY                            
*                                                                               
         LA    RE,MOBDSTDT          MAKE SURE VALID NUMERIC                     
         LA    R1,L'MOBDSTDT                                                    
MKGDTL30 CLI   0(RE),C'0'                                                       
         JL    INVNMFLD                                                         
         CLI   0(RE),C'9'                                                       
         JH    INVNMFLD                                                         
         LA    RE,1(RE)                                                         
         BCT   R1,MKGDTL30                                                      
*                                                                               
         GOTOR VDATCON,DMCB,(0,MOBDSTDT),(19,MOBDBDAT)     SPOT DATE            
         GOTOR VGETDAY,DMCB,MOBDSTDT,FULL         DAY OF WEEK                   
         CLC   DAYOFWEK,0(R1)      SPOT DATE MATCH START DAY OF WEEK?           
         JNE   INVRSTDY            -INVALID ROTATION START DAY                  
*                                                                               
         PACK  DUB,MOBDNOWK        NUMBER OF WEEKS                              
         CVB   R1,DUB                                                           
         STC   R1,MOBDNWKS                                                      
*                                                                               
         CLI   MOBDNWKS,0          ZERO WEEKS?                                  
         JE    INVWEEKS            YES, INVALID WEEKS                           
*                                                                               
         CLI   MOBDSPTS,C'0'       HAVE NUMERIC?                                
         JL    INVNPW               NO, ERROR - INVALID NPW                     
         CLI   MOBDSPTS+1,C'0'     HAVE NUMERIC?                                
         JL    INVNPW               NO, ERROR - INVALID NPW                     
*                                                                               
         PACK  DUB,MOBDSPTS        NUMBER OF SPOTS PER WEEK                     
         CVB   R1,DUB                                                           
         STC   R1,MOBDNSPW                                                      
*                                                                               
         CLI   MOBDNSPW,0          ZERO SPOTS PER WEEK?                         
         JE    INVNPW               YES, ERROR - INVALID NPW                    
*                                                                               
         CLI   MOBDNSPW,75         CAN'T ACCEPT MORE THEN 75 SPT/WEEK           
         JH    INVNMSPW                                                         
*                                                                               
         ZIC   R7,MOBDNWKS         # OF WEEKS                                   
         MR    R6,R1               X # OF SPOTS/WEEK = NUMBER OF SPOTS          
         ZICM  R1,TMKGSPTS,(7)     ADD TO TOTAL SPOTS FOR MKGHDR                
         AR    R1,R7                                                            
         STCM  R1,7,TMKGSPTS                                                    
*                                                                               
         ZICM  R1,TBMKSPTS,(7)     ADD TO TOTAL SPOTS FOR MKGBUY                
         AR    R1,R7                                                            
         CHI   R1,208              OVER 208 SPOTS ON LINE?                      
         JH    INVNMSPB                                                         
         STCM  R1,7,TBMKSPTS                                                    
*                                                                               
         ICM   R1,7,MOBDCOST       SPOT COST                                    
         MR    R6,R1               X NUMBER OF SPOTS = MAKEGOOD COST            
         A     R7,TMKGCOST                                                      
         ST    R7,TMKGCOST                                                      
*                                                                               
* CHECK FOR DUPLICATE WEEK                                                      
*                                                                               
****     CLI   DTLCOUNT,0          MORE THAN 1 MKGDTL?                          
****     B     *+8                  NO, DON'T CHECK                             
****     BAS   RE,CKDTLDUP           -NOOP'D HWON 6/12/14                       
*                                                                               
         GOTOR ,DMCB,(RECOSEQ,AIO1),(OEXISTS,0)                                 
         CLI   QSTATN,C'0'         CABLE?                                       
         BL    MKGDTLX                                                          
         TM    RECFLAG2,ONWINPRG                                                
         JZ    MSCBLONW                                                         
         GOTOR ,DMCB,(RECCSEQ,AIO4),(CEXISTS,0)                                 
*                                                                               
MKGDTLX  GOTOR ADDEL,DMCB                                                       
         BNE   NO                                                               
         B     YES                                                              
         DROP  R4,R2                                                            
*&&DO                                                                           
****************************                                                    
* CHECK FOR NON-CHRONILOGICAL DATES AND DUPLICATES                              
****************************                                                    
E        USING MOBDELD,ELTAREA                                                  
CKDTLDUP DS    0H                                                               
         L     R4,AIO1                                                          
         LA    R4,MORFRST-MOKEY(R4)                                             
         CLI   QSTATN,C'0'         CABLE?                                       
         BL    CDD010                                                           
         L     R4,AIO4                                                          
         LA    R4,MOXFRST-MOXKEY(R4)                                            
         USING MOMBELD,R4                                                       
CDD010   CLI   0(R4),0             END OF REC?                                  
         BER   RE                   EXIT                                        
         CLI   MOMBEL,MOMBELQ      FOUND X'20' BUY ELEMENT?                     
         BNE   CDD020                   NO                                      
         CLC   MOMBOFFR(2),E.MOBDOFFR   YES, MATCH OFFNUM/RECNUM?               
         BE    CDD030                                                           
         BHR   RE                   EXIT IF OFFNUM/RECNUM HIGHER                
CDD020   LLC   R0,1(R4)            BUMP TO NEXT                                 
         AR    R4,R0                                                            
         B     CDD010                                                           
         DROP  R4                                                               
***                                                                             
*  FOUND MATCH MOMBELQ X'20', CHECK FOR NON-CHRONOLOGICAL OR                    
*  DUPLICATE DATES                                                              
***                                                                             
CDD030   LLC   R0,1(R4)            BUMP TO NEXT                                 
         AR    R4,R0                                                            
         CLI   0(R4),0             END OF REC?                                  
         BER   RE                                                               
         CLI   0(R4),MOMBELQ       FOUND NEXT X'20' BUY ELEMENT?                
         BER   RE                   YES, THEN EXIT                              
         CLI   0(R4),MOBDELQ       X'50' DETAIL ELEMENT?                        
         BNE   CDD030                                                           
         USING MOBDELD,R4                                                       
         CLC   MOBDBDAT,E.MOBDBDAT  COMPARE DATES??                             
         BL    CDD030                -DATE IN PAST, CHECK NEXT                  
         JH    INDTLDTS              -INVALID, NON-CHRONOLOGICAL DATE           
         J     DPMKGDTL              -INVALID, DUPLICATE DATE                   
         DROP  R4,E                                                             
*&&                                                                             
         EJECT                                                                  
***********************  ERROR MESSAGES  ******************************         
ORNTCNFD LHI   RF,DMGORNCN         ORDER WAS NOT CONFIRMED                      
         J     SNDERRSU                                                         
*                                                                               
BADSQMGO LHI   RF,DMGBADSQ         MAKEGOOD OFFER NOT IN SEQUENCE               
         J     SNDERROR                                                         
*                                                                               
INVNMFLD LHI   RF,DMGINVNM         INVALID NUMBER FIELD                         
         J     SNDERROR                                                         
*                                                                               
NODARORD LHI   RF,DMGNDROR         DARE ORDER RECORD DOESN'T EXIST              
         J     SNDERRSU                                                         
*                                                                               
INCNDAT1 LHI   RF,DMGINCDT         INCON DATA (ORDER, SEQ#, GROUP ID)           
         J     SNDERRSU                                                         
*                                                                               
INCNDATA LHI   RF,DMGINCDT         INCON DATA (ORDER, SEQ#, GROUP ID)           
         J     SNDERROR                                                         
*                                                                               
INVLENUN LHI   RF,DMGINVUN         INVALID UNIT TYPE                            
         J     SNDERROR                                                         
*                                                                               
INVRECNT LHI   RF,DMGINREC         INVALID RECORD COUNT                         
         J     SNDERROR                                                         
*                                                                               
INVTOTDL LHI   RF,DMGTOTDL         INVALID TOTAL MAKEGOOD DOLLARS               
         J     SNDERROR                                                         
*                                                                               
ORDROKAY LHI   RF,DMGORDOK         OFFER ALREADY OKAYED                         
         J     SNDERROR                                                         
*                                                                               
OFFRCANN LHI   RF,DMGCANCL         OFFER CANCELLED W/NO MORE TO FOLLOW          
         J     SNDERROR                                                         
*                                                                               
OFFRNCAN LHI   RF,DMGNCANC         OFFER WAS NOT CANCELLED W/ MORE              
         J     SNDERROR                                                         
*                                                                               
PDNGRVSN LHI   RF,PNDINGRV         PENDING REVISION, DON'T ALLOW MG             
         J     SNDERRSU                                                         
*                                                                               
ORBLIMIT LHI   RF,LMT8ORBS         LIMIT OF 8 ORBITS PER OFFERED LINE           
         J     SNDERROR                                                         
*                                                                               
INVLBYLN LHI   RF,REFBDABL         INVALID AGENCY BUYLINE NUMBER                
         J     SNDERROR                                                         
*                                                                               
INVNMSPW LHI   RF,DMGTMSPW         INVALID TOTAL SPOTS PER WEEK                 
         J     SNDERROR                                                         
*                                                                               
INVNMSPB LHI   RF,DMGTMSPB         INVALID TOTAL SPOTS PER OFFER LINE           
         J     SNDERROR                                                         
*                                                                               
DPMKGDTL LHI   RF,DMGDPDTL         INVALID DUPLICATE MKGDTL DATES               
         J     SNDERROR                                                         
*                                                                               
INDTLDTS LHI   RF,DMGDTLSQ         INVALID MKGDTL DATES SEQUENCE                
         J     SNDERROR                                                         
*                                                                               
NOCBLMG  LHI   RF,DMGNOCBL         NO CABLE MAKEGOOD SUPPORT YET                
         J     SNDERROR                                                         
*                                                                               
INVCBLST LHI   RF,INVCABLE         INVALID CABLE STATION                        
         J     SNDERROR                                                         
*                                                                               
INVSTTN  LHI   RF,INVSTATN         INVALID STATION                              
         J     SNDERRSU                                                         
*                                                                               
NOTCABLE LHI   RF,DMGNOTCB         NOT A CALBE MAKEGOOD                         
         J     SNDERROR                                                         
*                                                                               
DUPMKMNW LHI   RF,DMGDUPMN         DUPLICATE MKGMNW RECEIVED                    
         J     SNDERROR                                                         
*                                                                               
DUPMKONW LHI   RF,DMGDUPMN         DUPLICATE MKGONW RECEIVED                    
         J     SNDERROR                                                         
*                                                                               
MSCBLMNW LHI   RF,DMGMSMNW         CABLE MKGD MISSING MKGMNW                    
         J     SNDERROR                                                         
*                                                                               
MSCBLONW LHI   RF,DMGMSONW         CABLE MKGD MISSING MKGONW                    
         J     SNDERROR                                                         
*                                                                               
INVMKGD2 LHI   RF,DMGINVD2         INVALID MKGDS2 RECEIVED                      
         J     SNDERROR                                                         
*                                                                               
INVMKCDC LHI   RF,DMGINCDC         INVALID MKGCDC RECEIVED                      
         J     SNDERROR                                                         
*                                                                               
INVMKGDM LHI   RF,DMGINVDM         INVALID MKGDEM RECEIVED                      
         J     SNDERROR                                                         
*                                                                               
INVMKCDV LHI   RF,DMGINCDV         INVALID MKGCDV RECEIVED                      
         J     SNDERROR                                                         
*                                                                               
INVTMPRD LHI   RF,DMGINVTM         INVALID TIME PERIOD                          
         J     SNDERROR                                                         
*                                                                               
INVDAYS  LHI   RF,DMGINVDY         INVALID DAYS                                 
         J     SNDERROR                                                         
*                                                                               
INVWEEKS LHI   RF,DMGINVWK         INVALID WEEKS                                
         J     SNDERROR                                                         
*                                                                               
INVRSTDY LHI   RF,DMGINVRO         INVALID ROTATION START DAY                   
         J     SNDERROR                                                         
*                                                                               
INVNPW   LHI   RF,DMGNVNPW         SPOTS PER WEEK IS INVALID OR ZERO            
         J     SNDERROR                                                         
*                                                                               
INVNOCHG LHI   RF,DMGCCHNG         CAN'T CHANGE STATE OF MAKEGOOD               
         J     SNDERROR                                                         
*                                                                               
TOOMNYMS LHI   RF,929              TOO MANY MISSED SPOTS OR MISSED CMTS         
         J     SNDERROR                                                         
***********************************************************************         
* SENDS AN ERROR NOTIFICATION OUT                                               
*                                                                               
* ON ENTRY:    SAVEHDR             HEADER RECORD                                
***********************************************************************         
SNDERRSU OI    ERRFLG1,EF1SKUP     SKIP RECORD UPDATE                           
         J     SNDERROR                                                         
*                                                                               
SNDERROR DS    0H                                                               
         STCM  RF,3,ERROR                                                       
*                                                                               
         LA    R4,SAVEHDR                                                       
         USING MOFRHDRD,R4                                                      
*                                                                               
         L     RE,ASPLAREA                                                      
         LA    RF,L'SPULAREA                                                    
         XCEFL                                                                  
*                                                                               
         L     R7,ASPLAREA                                                      
         USING SPOOLD,R7                                                        
         MVC   SPOOLID,=C'DAR'                                                  
         MVI   USERLANG,0          ENGLISH                                      
         MVC   SPOOLDM,VDATAMGR                                                 
         MVC   RCDATCON,VDATCON                                                 
         MVC   RCCOMFAC,SRPAR4                                                  
         MVC   SPOOLBUF,ATIA                                                    
*                                                                               
         LA    R2,SPOOLKEY                                                      
         USING PQPLD,R2                                                         
         XC    SPOOLKEY,SPOOLKEY                                                
         MVC   PLSUBID,=C'DAR'                                                  
         MVC   PLUSER,=X'0011'    **** SJR FOR NOW                              
         MVC   PLDESC(6),=C'ERRNOT'                                             
         MVI   PLCLASS,C'G'                                                     
         OI    SPOOLIND,SPUINIT    ALLOWS ME TO SET THE CLASS                   
*                                                                               
         XC    P1,P1               OPEN PRINTQ ENTRY                            
         BRAS  RE,SNDEPRNT                                                      
         J     SNDE10                                                           
*                                                                               
SNDEPRNT LR    R0,RE                                                            
         GOTOR ASPOOL,DMCB,(R7)                                                 
         LR    RE,R0                                                            
         BR    RE                                                               
*                                                                               
SNDE10   MVC   SPOOLRPN,PLREPNO                                                 
         MVI   PLCC,0                                                           
         DROP  R2                                                               
***************                                                                 
* PUT OUT EDICT HEADER RECORD                                                   
***************                                                                 
         MVC   P1+4(5),=C'*HDR*'    HEADER RECORD                               
         MVC   P1+9(14),=CL14'EDICT=*DDSDARA'                                   
         MVI   P1+34,C'W'           WIDE REPORT                                 
         MVI   P1+35,C'P'           /PAGE FOR EASYLINK AND SUPRESS TOP          
*                                                                               
         MVC   P2(14),=CL14'++DDS DAERRTRN'                                     
         BRAS  RE,SNDEPRNT                                                      
*                                                                               
         LA    R2,P                                                             
         USING MDLNNOTD,R2                                                      
         MVC   MDNTTID,=C'ERRNOT'  ERROR NOTIFICATION                           
         MVC   MDNTORDR,6(R4)      ORDER # IS ALWAYS 6 BYTES FROM BEG.          
         MVC   MDNTFRID,MOHDTOID          FROM ID                               
         MVC   MDNTTOID,MOHDFRID          TO ID                                 
*                                                                               
         GOTOR VDATCON,DMCB,(5,0),(X'20',MDNTDATE)                              
         THMS  DDSTIME=YES                                                      
         STCM  R1,15,PACKOF4B                                                   
         STCM  R0,15,PACK4                                                      
         AP    PACKOF4B,PACK4      DDS TIME IS OFFSET FROM 6AM                  
*                                                                               
         CP    PACKOF4B,=P'240000'    PAST MIDNIGHT?                            
         JL    SNDE20                                                           
         SP    PACKOF4B,=P'240000'    YES, BUMP TO NEXT DAY AND ADJUST          
         GOTOR VADDAY,DMCB,MDNTDATE,MDNTDATE,F'1'                               
*                                                                               
SNDE20   ICM   R1,15,PACKOF4B                                                   
         SRL   R1,12               GET RID OF SECONDS AND SIGN                  
         STH   R1,HALF                                                          
         GOTOR VHEXOUT,DMCB,HALF,MDNTTIME,L'HALF,0                              
*                                                                               
         MVC   MDNTRPCN,MOHDRPCN   REP CONTRACT NUMBER                          
         MVC   MDNTRTNS,MOHDRTNS   'RETURN TO SENDER' DATA                      
         MVC   MDNTTDTE,MOHDDATE   RECEIVED DATE/TIME                           
         MVC   MDNTTTIM,MOHDTIME                                                
         EDIT  (B2,ERROR),(3,MDNTEFLG),FILL=0                                   
         MVC   MDNTOFRI,GROUPID    GROUP ID                                     
         EDIT  VER,(2,MDNTSEQN),FILL=0                                          
         EDIT  LINENUM,(3,MDNTLINM),FILL=0                                      
         BRAS  RE,SNDEPRNT                                                      
         DROP  R2                                                               
*                                                                               
         MVC   P(26),=CL26'*** END OF DDS MESSAGE ***'                          
         BRAS  RE,SNDEPRNT                                                      
*                                                                               
         MVI   SPMODE,X'FF'        YES, CLOSE PRINTQ ENTRY                      
         GOTOR ASPOOL,DMCB,(R7)                                                 
*                                                                               
         NI    RECFLAG,X'FF'-INPROG                                             
**********************                                                          
* CHECK RECWRITN IN RECFLAG IF ANY NEW VERSIONS OF THE OFFER RECORDS            
* HAVE BEEN WRITTEN OUT.                                                        
* - IF THEY HAVE, THEN DELETE ALL OFFER AND NOTICE RECORDS                      
* - IF THEY HAVEN'T, THEN UNMARK ALL THE OFFER RECORDS DELETED.                 
*    THAT ARE THE SAME VERSION                                                  
**********************                                                          
         CLI   QSTATN,C'0'         CABLE?                                       
         JNL   SNDE100               YES                                        
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(L'OFFRKEY),OFFRKEY                                           
         MVC   KEYSAVE(L'OFFRKEY),KEY                                           
         LA    R2,KEY                                                           
         USING DAREMGOD,R2                                                      
         CLC   BINORDER,MOKORDER   SAVED OFFRKEY                                
         JNE   SNDEX                                                            
         CLC   GROUPID,MOKMGCD        & GROUP CODE DOESN'T MATCH?               
         JNE   SNDEX               THEN WE CAN'T HAVE DELETES                   
*                                                                               
         TM    RECFLAG,RECWRITN                                                 
         JO    SNDE50              DON'T READ FOR DELETES                       
***                                                                             
* NO OFFER RECORDS WERE WRITTEN OUT, LETS                                       
* UNMARKS ALL OFFER RECORDS DELETED THAT ARE THE SAME VERSION                   
***                                                                             
         MVI   DMINBTS,X'08'                                                    
         BRAS  RE,HIGH                                                          
*                                                                               
         TM    KEY+L'MOKEY,X'80'   FIRST ONE DELETED?                           
         JO    SNDEX               YES, SO MUST BE BAD                          
         L     R2,AIO1                                                          
*                                                                               
SNDE30   CLC   KEYSAVE(MOKSEQ-MOKEY),KEY                                        
         JNE   SNDEX               NO MORE RECORDS OUT THERE                    
*                                                                               
         MVI   DMINBTS,X'88'                                                    
         BRAS  RE,HIGH                                                          
*                                                                               
         MVI   DMINBTS,X'88'                                                    
         L     R6,AIO1                                                          
         ST    R6,AIO                                                           
         BRAS  RE,GET                                                           
*                                                                               
         CLI   KEY+MOKSEQ-MOKEY,MOKSEQDQ    X'FF' DEMO RECORD                   
         JE    SNDE35                                                           
*                                                                               
         MVC   BYTE,MOKSEQ         SAVE SEQ#                                    
         MVI   ELCODE,MOSQELQ                                                   
         MVI   DATADISP+1,24                                                    
         BRAS  RE,GETEL                                                         
         JNE   *+2                                                              
         USING MOSQELD,R6                                                       
         CLI   BYTE,0              FIRST RECORD                                 
         JNE   *+14                                                             
         MVC   VERSION,MOSQNUM     SAVE VERSION NUMBER                          
         J     SNDE40                                                           
*                                                                               
         CLC   VERSION,MOSQNUM     COMPARE VERSION                              
         JNE   SNDEX               NOT SAME, DONE                               
         DROP  R6                                                               
*                                                                               
SNDE35   NI    MORSTAT,X'FF'-X'80'                                              
         BRAS  RE,PUT                                                           
*                                                                               
         NI    KEY+L'MOKEY,X'FF'-X'80'                                          
         MVC   KEYSAVE(L'MOKEY),KEY                                             
         BRAS  RE,WRITE                                                         
*                                                                               
SNDE40   MVI   DMINBTS,X'08'                                                    
         BRAS  RE,SEQ                                                           
         J     SNDE30                                                           
***                                                                             
* OFFER RECORDS WERE WRITTEN OUT, LETS                                          
* DELETE ALL OFFER AND NOTICE RECORDS                                           
***                                                                             
SNDE50   MVI   DMINBTS,X'08'                                                    
         BRAS  RE,HIGH                                                          
*                                                                               
         L     R2,AIO1                                                          
SNDE60   CLC   KEYSAVE(MOKSEQ-MOKEY),KEY                                        
         JNE   SNDE70              NO RECORDS OUT THERE                         
*                                                                               
         MVI   DMINBTS,X'88'                                                    
         BRAS  RE,HIGH                                                          
*                                                                               
         MVI   DMINBTS,X'80'                                                    
         L     R6,AIO1                                                          
         ST    R6,AIO                                                           
         BRAS  RE,GET                                                           
*                                                                               
         OI    MORSTAT,X'80'                                                    
         BRAS  RE,PUT                                                           
*                                                                               
         OI    KEY+L'MOKEY,X'80'                                                
         MVC   KEYSAVE(L'MOKEY),KEY                                             
         BRAS  RE,WRITE                                                         
*                                                                               
         BRAS  RE,SEQ                                                           
         J     SNDE60                                                           
*                                                                               
SNDE70   XC    KEY,KEY                                                          
         MVC   KEY(L'MNKEY),NOTCKEY                                             
         MVC   KEYSAVE(L'MNKEY),KEY                                             
         BRAS  RE,HIGH                                                          
*                                                                               
         CLC   KEYSAVE(L'MNKEY),KEY                                             
         JNE   SNDEX               NO RECORDS OUT THERE                         
*                                                                               
         MVI   DMINBTS,X'80'           RDUPDATE ONLY WHEN NECESSARY             
         BRAS  RE,HIGH                                                          
*                                                                               
         MVI   DMINBTS,X'80'                                                    
         L     R6,AIO1                                                          
         ST    R6,AIO                                                           
         BRAS  RE,GET                                                           
*                                                                               
         OI    MORSTAT,X'80'                                                    
         BRAS  RE,PUT                                                           
*                                                                               
         OI    KEY+L'MNKEY,X'80'                                                
         MVC   KEYSAVE(L'MNKEY),KEY                                             
         BRAS  RE,WRITE                                                         
         J     SNDEX                                                            
         DROP  R2                                                               
*                                                                               
* CABLE RECORD PROCESSING , XSPOT FILE                                          
*                                                                               
SNDE100  DS    0H                                                               
         TM    ERRFLG1,EF1SKUP     SKIP UPDATE?                                 
         JO    SNDEX                                                            
*                                                                               
**       CLI   OEXISTS,0           HAVE OFFER RECORD?                           
**       JE    SNDE103              NO, SKIP                                    
         GOTOR RECWRT,DMCB,(RECOSEQ,AIO1),(OEXISTS,0)                           
         CLI   OEXISTS,0           OFFER EXISTED BEFORE?                        
         JNE   *+8                 YES                                          
         OI    OEXISTS,EXONFILE    NO, NOW IT DOES                              
                                                                                
SNDE103  TM    NEXISTS,EXONFILE    NOTICE RECORD EXISTS?                        
         JZ    SNDE104             NO, DEFINITELY NEW                           
         TM    NEXISTS,EXDEL         WAS IT DELETED?                            
         JZ    SNDE107               NO, DEFINITELY AMENDED                     
*                                                                               
SNDE104  TM    RECFLAG,REJCOMNT    WAS THERE A REJECTION COMMENT                
         JNO   SNDE105                                                          
         XC    ELTAREA,ELTAREA                                                  
         LA    R2,SAVEREJT                                                      
         LA    R3,SAVEREJX                                                      
*                                                                               
SNDE104A CR    R2,R3                  EOL OF REJ COMMENTS?                      
         JNL   SNDE105                 YES                                      
         OC    0(L'SAVEREJT,R2),0(R2) ANY OF MORE REJ COMMENTS?                 
         JZ    SNDE105                 NO                                       
*                                                                               
         MVC   ELTAREA,0(R2)                                                    
         GOTOR ADDEL,DMCB,(RECNSEQ,AIO2),(NEXISTS,0)                            
         LA    R2,L'SAVEREJT(0,R2)                                              
         J     SNDE104A                                                         
*                                                                               
SNDE105  GOTOR RECWRT,DMCB,(RECNSEQ,AIO2),(NEXISTS,0)                           
         OI    NEXISTS,EXONFILE    NOTICE RECORD EXISTS                         
*                                                                               
SNDE107  XC    KEY,KEY                                                          
         MVC   KEY(L'MNXKEY),NOTCKEY                                            
         MVC   KEYSAVE(L'MNXKEY),KEY                                            
         BRAS  RE,XHIGH                                                         
*                                                                               
         CLC   KEYSAVE(L'MNXKEY),KEY                                            
         JNE   SNDEX               NO RECORDS OUT THERE                         
*                                                                               
         MVI   DMINBTS,X'80'           RDUPDATE ONLY WHEN NECESSARY             
         BRAS  RE,XHIGH                                                         
*                                                                               
         MVI   DMINBTS,X'80'                                                    
         L     R6,AIO2                                                          
         ST    R6,AIO                                                           
         BRAS  RE,XGET                                                          
*                                                                               
         LA    R3,ELTAREA                                                       
         USING MNSTELD,R3                                                       
*                                                                               
         MVI   MNSTEL,MNSTELQ      ELEMENT CODE                                 
         MVI   MNSTLEN,MNSTELNQ    OVERHEAD LENGTH                              
*                                                                               
         LA    RE,MOHDDATE         MAKE SURE VALID NUMERIC                      
         LA    R1,L'MOHDDATE                                                    
SNDE110  CLI   0(RE),C'0'                                                       
         JL    INVNMFLD                                                         
         CLI   0(RE),C'9'                                                       
         JH    INVNMFLD                                                         
         LA    RE,1(RE)                                                         
         BCT   R1,SNDE110                                                       
*                                                                               
         GOTOR VDATCON,DMCB,(0,MOHDDATE),(19,MNSTDATE)                          
         PACK  PTIME,MOHDTIME      TIME                                         
         SRP   PTIME,1,0           SHIFT LEFT 1 DIGIT                           
         MVC   MNSTTIME,PTIME      PWOS TIME                                    
*                                                                               
         MVI   MNSTSTAT,MNSTERR    ERROR                                        
         MVC   MNSTERRN,ERROR                                                   
         LA    R6,MNXFRST-MNXKEY(R6)                                            
         GOTOR VRECUP,DMCB,(X'FE',AIO2),ELTAREA,(R6),XSPREC                     
         DROP  R3                                                               
*                                                                               
         GOTOR RECWRT,DMCB,(RECNSEQ,AIO2),(NEXISTS,0)                           
****     BRAS  RE,PUT                                                           
*                                                                               
         DROP  R4                                                               
SNDEX    MVI   ERRFLG1,0                                                        
         J     NO                                                               
         DROP  R7                                                               
*                                                                               
YES      SR    RC,RC                                                            
NO       LTR   RC,RC                                                            
EXIT     XIT1                                                                   
*                                                                               
EXITPRG  L     RD,SAVERD                                                        
         B     EXIT                                                             
*                                                                               
         GETEL R6,DATADISP,ELCODE                                               
         EJECT                                                                  
*                                                                               
*   LOCAL VALUES                                                                
*                                                                               
DMRDHI   DC    CL8'DMRDHI  '                                                    
DMREAD   DC    CL8'DMREAD  '                                                    
DMWRITE  DC    CL8'DMWRT   '                                                    
DMADD    DC    CL8'DMADD   '                                                    
DMRSEQ   DC    CL8'DMRSEQ  '                                                    
GETREC   DC    CL8'GETREC  '                                                    
PUTREC   DC    CL8'PUTREC  '                                                    
ADDREC   DC    CL8'ADDREC  '                                                    
*                                                                               
SPTDIR   DC    CL8'SPTDIR'                                                      
SPTFILE  DC    CL8'SPTFILE'                                                     
CTFILE   DC    CL8'CTFILE'                                                      
XSPDIR   DC    CL8'XSPDIR'                                                      
XSPFIL   DC    CL8'XSPFIL'                                                      
SPACE    DC    20C' '                                                           
FOXZEROS DC    20C'0'                                                           
XSPREC   DC    AL2(42,32,5970)                                                  
*ESTO1   DC    C'MKGORB4340000201*M      114001500PROGRAM'                     
*ESTO2   DC    C'MKGORB4340000201   WTF  316001700'                             
*ESTO3   DC    C'MKGORB4340000201*M      114001500'                             
*ESTO4   DC    C'MKGORB4340000201   WTF  316001700'                             
*ESTO5   DC    C'MKGORB4340000202*M      114001500'                             
*ESTO6   DC    C'MKGORB4340000202   WTF  316001700'                             
*ESTC1   DC    C'MKGCOM4340000201*DETAIL COMMENT LINE 1 OFFER 2/AB....'         
*ESTC2   DC    C'MKGCOM4340000201 DETAIL COMMENT LINE 2 OFFER 2/AB....'         
*ESTC3   DC    C'MKGCOM4340000201*DETAIL COMMENT LINE 11OFFER 2/AB....'         
*ESTC4   DC    C'MKGCOM4340000201 DETAIL COMMENT LINE 22OFFER 2/AB....'         
*ESTC5   DC    C'MKGCOM4340000202*DETAIL COMMENT LINE 1 OFFER 2/AB....'         
*ESTC6   DC    C'MKGCOM4340000202*DETAIL COMMENT LINE 2 OFFER 2/AB....'         
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
GTMOOFTB BASR  R1,RE               GET MEDIAOCEAN OFFICE TABLE                  
       ++INCLUDE DDMOREPTAB                                                     
         EJECT                                                                  
***********************************************************************         
*   MKGHDR: MAKEGOOD OFFER IDENTIFICATION                                       
***********************************************************************         
MKGHDR   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         XC    MNDSKADD,MNDSKADD   CLEAR X'00' NOT REC D/A                      
*                                                                               
         LA    R4,R+1              SET INPUT RECORD DSECT                       
         USING MOFRHDRD,R4                                                      
         MVC   SAVEHDR,0(R4)       SAVE THE HEADER                              
*                                                                               
         TM    RECFLAG,INPROG      MAKEGOOD RECORDS IN PROGRESS?                
         JO    BADSQMGO                                                         
         MVI   RECFLAG,INPROG      INIT AND SET INPROG                          
         MVI   MISSSEQ,0           MISSED BUYLINE SEQ                           
         MVI   MSOFFNUM,0          INIT MISSED OFFER NUMBER                     
         MVI   MGOFFNUM,0          INIT MAKEGOOD OFFER NUMBER                   
         XC    SVOFFNUM,SVOFFNUM   INIT OFFER NUMBER FROM MKGBUY                
         MVI   RCHDBUY,0                                                        
*                                                                               
         MVI   LASTREC,HDR         SET LAST RECORD TYPE                         
         MVC   MKGCOUNT,=F'1'      INITIALIZE TO 1                              
         MVI   DS1COUNT,0          INITIALIZE TO 0                              
         MVI   COMCOUNT,0          INITIALIZE COMMENT COUNT                     
         XC    TMKGCOST,TMKGCOST   INITIALIZE TO 0                              
         XC    SVMODMEL,SVMODMEL   INITIALIZE SELLER DEMO ELEMENT               
         XC    SVNTDCEL,SVNTDCEL   INITIALIZE SELLER NON-TRAD DEMO ELEM         
*                                                                               
         MVC   ORDNO,MOHDORDR      ORDER NUMBER                                 
         LA    RE,MOHDORDR                                                      
         ST    RE,DMCB                                                          
         MVI   DMCB,C'N'                                                        
         OI    DMCB,X'80'                                                       
         GOTOR VCASHVAL,DMCB,,8                                                 
         CLI   0(R1),0                                                          
         JNE   INVNMFLD            INVALID NUMERIC FIELD                        
*                                                                               
         CLI   MOHDORDR+1,C'3'                                                  
         BNH   MKGHD010                                                         
         PACK  DUB,MOHDORDR        NEW STYLE ORDER NUMBER                       
         SP    DUB,=P'04000000'                                                 
         CVB   R1,DUB                                                           
         STCM  R1,15,BINORDER                                                   
         OI    BINORDER,X'80'                                                   
         XC    BINORDER,=4X'FF'                                                 
         B     MKGHD020                                                         
*                                                                               
MKGHD010 GOTOR VDATCON,DMCB,(5,0),(15,JDTTODAY)   GET CURRENT DATE              
         ZAP   FULL,JDTTODAY       CENTURY/YEAR FROM TODAY'S DATE               
         SRP   FULL,61,0                                                        
*                                                                               
         GOTOR VHEXIN,DMCB,ORDNO,HEXORDER,8  SAVE AS IF ENTRY WAS HEX           
         MVC   PACKOF4B,HEXORDER   CONVERT IT TO PACK                           
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
         OC    PACKOF4B+1(2),HEXORDER  STICK IN DAYS IN YEAR                    
         SRP   PACKOF4B,63,0                                                    
*                                                                               
         ZAP   DUB,PACKOF4B        SAVE DATE PORTION                            
         CVB   R0,DUB                                                           
         STCM  R0,3,BINORDDT                                                    
         XC    BINORDDT,=4X'FF'                                                 
*                                                                               
         PACK  DUB,MOHDORDR+4(4)   SAVE SEQUENCE PORTION                        
         CVB   R0,DUB                                                           
         STCM  R0,3,BINORDSQ                                                    
         XC    BINORDSQ,=4X'FF'                                                 
*                                                                               
MKGHD020 MVC   GROUPID,MOHDOFRI    GROUP ID                                     
*                                                                               
         PACK  DUB,MOHDSEQN        SEQUENCE #                                   
         CVB   R5,DUB                                                           
         STC   R5,VER                                                           
*                                                                               
         NI    BITFLAG1,X'FF'-BF1REPCH                                          
         GOTOR GETIDNUM,DMCB,MOHDFRID,MKGREPID                                  
         BNE   MKGHDRNO                                                         
*                                                                               
         LA    R2,MOHDRTNS         RETURN TO SENDER DATA                        
         USING RTN2SNDR,R2                                                      
         MVC   QAGY,RTNPWRCD       SAVE AGENCY POWER CODE                       
         GOTOR VHEXIN,DMCB,RTNAGYMD,AGYMD,2,=C'TOG'   AGENCY/MEDIA              
***************                                                                 
* THIS IS FOR SPANKED AGENCIES.  THE ADV IS OKAY BUT THE AGY/MED MIGHT          
* NOT BE                                                                        
***************                                                                 
         CLC   =C'OM',QAGY         OMNY IS NOW AGENCY X'40' ON ADV2             
         BNE   *+8                                                              
         MVI   AGYMD,X'41'                                                      
         DROP  R2                                                               
*                                                                               
* READ DARE ORDER RECORDS TO GET CLIENT,PRODUCT,BUYER, AND STATION              
         LA    R6,KEY                                                           
         USING DAREORDD,R6                                                      
         XC    KEY,KEY                                                          
         MVI   DOKTYPE,DOKTYPQ     TYPE                                         
         MVI   DOKSUBTY,DOKSTYPQ   SUBTYPE                                      
         MVC   DOKAGMD,AGYMD       A/M                                          
         MVC   DOKORDER,BINORDER   ORDER NUMBER                                 
*                                                                               
         BRAS  RE,HIGH                                                          
         CLC   KEYSAVE(DOKSTA-DOKEY),KEY                                        
         JNE   NODARORD            DARE ORDER RECORD DOESN'T EXIST              
*                                                                               
         CLI   DOKCMT,0                                                         
         JNE   NODARORD            SHOULDN'T BE A COMMENT                       
*                                                                               
         L     R6,AIO1                                                          
         ST    R6,AIO                                                           
         BRAS  RE,GET                                                           
*                                                                               
         LA    R1,DORFRST          FIRST ELEMENT                                
*                                                                               
         CLI   0(R1),X'01'         PRIMARY ID ELEMENT?                          
         JNE   *+2                                                              
*                                                                               
         USING DOIDELD,R1                                                       
         MVC   BUYER,DOIDBYR       BUYER CODE                                   
         MVC   BSTATN,DOISTA       BINARY STATION                               
         MVC   BCLT,DOIDCLT        BINARY CLIENT CODE                           
         MVC   BEST,DOIDEST        BINARY ESTIMATE NUMBER                       
         DROP  R1                                                               
*                                                                               
         LA    R6,DORFRST                                                       
         SR    R0,R0                                                            
MKGHD030 CLI   0(R6),0             END OF RECORD                                
         JE    ORNTCNFD            NO, PRINT ERROR MESSAGE                      
         CLI   0(R6),DOSPELQ                                                    
         BE    MKGHD040                                                         
         CLI   0(R6),DORPELQ                                                    
         BE    MKGHD045                                                         
         CLI   0(R6),DOSTELQ                                                    
         BE    MKGHD050                                                         
MKGHD035 IC    R0,1(R6)                                                         
         AR    R6,R0                                                            
         B     MKGHD030                                                         
*                                                                               
         USING DOSPELD,R6                                                       
MKGHD040 MVC   REVISION,DOSPREVN   SAVE THE REVISION NUMBER IF ANY              
         B     MKGHD035            YES                                          
*                                                                               
         USING DOREPELD,R6                                                      
MKGHD045 CLC   DORPNREP,MKGREPID   SAME ID AS MOST CURRENT?                     
         BE    MKGHD120            YES                                          
         OI    BITFLAG1,BF1REPCH   NO, REP CHANGED                              
         B     MKGHD070              SHOULD BE CONFIRMED ALREADY                
*                                                                               
         USING DOSTELD,R6                                                       
MKGHD050 CLI   DOSTSTAT,DDLVRD     DELIVERED?                                   
         BE    MKGHD035            IF DELIVERED, SKIP TO NEXT STATUS            
*                                                                               
         CLI   DOSTSTAT,QCFMD      CONFIRMED?                                   
         BE    MKGHD060            YES                                          
         CLI   DOSTSTAT,QBYRCNFM   BUYER CONFIRMED?                             
         BE    MKGHD060            YES                                          
         CLI   REVISION,0          GETTING A MKGD FOR A REVSION?                
         JE    ORNTCNFD            NO, MUST BE CONFIRMED FIRST                  
         CLI   DOSTSTAT,QAPP       YES, APPROVED REVISION?                      
         BE    MKGHD060                 YES                                     
         CLI   DOSTSTAT,QRJCT           REJECTED REVISION?                      
         BE    MKGHD060                 YES                                     
         CLI   DOSTSTAT,QSNTXCNF        SENT CANCELLED, CONFIRMED?              
         BE    MKGHD060                 YES                                     
         CLI   DOSTSTAT,QSNTXREJ        SENT CANCELLED, REJECTED?               
         BE    MKGHD060                 YES                                     
         CLI   DOSTSTAT,QERRORED                                                
         BE    MKGHD062                                                         
         CLI   DOSTSTAT,QRCLAPPR        OR RECALLED REVISION?                   
         JL    PDNGRVSN                                                         
         CLI   DOSTSTAT,QRCLWIP                                                 
         JH    PDNGRVSN                 NO, PENDING REVISION                    
*                                                                               
MKGHD060 CLI   DOSTEL,DOSTELQ      MAKE SURE WE HAVE A DELNOT                   
         BNE   MKGHD120             DON'T WANT TO LOOP                          
***      DC    H'0'               *** DON'T WANT TO DIE.                        
         CLI   DOSTSTAT,DDLVRD     CHECK DELNOT FOR SAME ID                     
         BE    MKGHD065                                                         
         IC    R0,1(R6)                                                         
         AR    R6,R0                                                            
         B     MKGHD060                                                         
*                                                                               
MKGHD062 ST    R6,FULL                                                          
         L     R6,AIO1              FIND ERROR ELEMENT                          
         MVI   ELCODE,X'30'                                                     
         MVI   DATADISP+1,24                                                    
         BRAS  RE,GETEL                                                         
         JNE   PDNGRVSN                                                         
         CLC   3(3,R6),=C'113'     ALLOW MAKEGOOD IF ACTIVE MKGD ERROR          
         JNE   PDNGRVSN                                                         
         L     R6,FULL                                                          
         B     MKGHD060                                                         
*                                                                               
MKGHD065 CLC   DOSTIDNM,MKGREPID   SAME ID AS MOST CURRENT?                     
         BE    MKGHD120            YES                                          
         OI    BITFLAG1,BF1REPCH   NO, REP CHANGED                              
         DROP  R6                                                               
*                                                                               
MKGHD070 DS    0H                                                               
         CLC   =C'HRP',MOHDFRID    JDS REP SENDING THIS MAKEGOOD?               
         BE    MKGHD075                                                         
         CLC   =C'TEL',MOHDFRID                                                 
         BNE   MKGHD120                                                         
*********                                                                       
*    DO ADDITIONAL CHECK TO INSURE DARE ORDER'S STATION IS REPPED BY            
*       THE REP SENDING THE MAKEGOOD                                            
*********                                                                       
MKGHD075 BRAS  RE,GETQSTA                                                       
         JNE   INVSTTN             INVALID STATION                              
*                                                                               
         MVI   DMCB,X'0A'          SWITCH TO THE CONTROL SYSTEM                 
         BRAS  RE,SWTCHSYS                                                      
         BNE   EXITPRG                                                          
*                                                                               
         XC    KEY,KEY             GET DSTA RECORD TO FIND CURRENT REP          
         LA    RE,KEY              USE RE FOR NOW                               
         USING STAKEY,RE                                                        
         MVI   STAKTYP,STAKTYPQ                                                 
         MVI   STAKMEDA,C'T'                                                    
         MVC   STAKSTIN,QSTATN                                                  
         DROP  RE                                                               
*                                                                               
         MVC   KEYSAVE,KEY                                                      
         GOTOR VDATAMGR,DMCB,DMRDHI,=C'GENDIR',KEYSAVE,KEY                      
**       CLC   KEYSAVE(L'STAKEY),KEY                                            
**       BNE                                                                    
         GOTOR VDATAMGR,DMCB,GETREC,=C'GENFIL',KEY+36,AIO2,IOWORK               
         L     R6,AIO2                                                          
         USING STAKEY,R6                                                        
         LA    R6,STAFSTEL         R6 = A(1ST ELEM)                             
         SR    R0,R0                                                            
MKGHD080 CLI   0(R6),0                                                          
         JE    *+2                                                              
         CLI   0(R6),X'10'         INFO ELEMENT?                                
         BE    MKGHD090                                                         
         IC    R0,1(R6)                                                         
         AR    R6,R0                                                            
         B     MKGHD080                                                         
*                                                                               
         USING STAREPD,R6                                                       
MKGHD090 GOTOR VDATCON,DMCB,(5,0),(15,FULL)                                     
         LA    R1,STAREPCR         POINT TO THE CURRENT REP                     
         CLC   FULL,STAREPED       ON OR AFTER EFFECTIVE DATE?                  
         BNL   *+8                 YES, WE'LL USE THE CURRENT REP               
         LA    R1,STAREPPR         POINT TO PREVIOUS REP                        
*                                                                               
         XC    DMCB,DMCB                                                        
         MVC   DMCB+4,=X'D9000A3E'    GETDARE                                   
         GOTOR VCALLOVL,DMCB                                                    
         L     RF,DMCB                                                          
         GOTOR (RF),DMCB,(C'U',0),MOHDFRID,AIO3,F'6000',VDATAMGR                
         JNE   MKGHD100                                                         
*                                                                               
         L     RE,AIO3                                                          
         USING DAREPTD,RE                                                       
         LLC   RF,DAPTLPFX         GET LENGTH OF USERID PREFIX                  
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         CLC   DAPTRPPF(0),MOHDFRID   DOES THE REP MATCH THE SENDER?            
         BE    MKGHD110                                                         
         DROP  RE                                                               
*                                                                               
MKGHD100 MVC   DMCB(1),SPTSENUM    SWITCH BACK TO SPOT SYSTEM                   
         BRAS  RE,SWTCHSYS                                                      
         J     INCNDAT1            INCONSISTENT DATA                            
*                                                                               
MKGHD110 MVC   DMCB(1),SPTSENUM    SWITCH BACK TO SPOT SYSTEM                   
         BRAS  RE,SWTCHSYS                                                      
*                                                                               
MKGHD120 BRAS  RE,GETQSTA                                                       
         JNE   INVSTTN             INVALID STATION                              
*                                                                               
         CLI   QSTATN,C'0'         CABLE?                                       
         BNL   MKGHD130             YES                                         
         BAS   RE,RDUPDT                                                        
*                                                                               
         L     R2,AIO2             MAKEGOOD NOTICE RECORD                       
         USING DAREMGND,R2                                                      
         LR    RE,R2                                                            
         LA    RF,4000                                                          
         XCEFL                                                                  
         MVC   0(L'MNKEY,R2),NOTCKEY                                            
         LA    R1,MNRFRST-MNKEY    LENGTH OF RECORD W/O ELEMENTS                
         STCM  R1,3,MNRLEN                                                      
         MVC   MNRAGY,POWERCDE     ALPHA AGENCY CODE                            
         B     MKGHD140                                                         
*                                                                               
MKGHD130 BAS   RE,RDXPDT                                                        
*                                                                               
         L     R2,AIO2             MAKEGOOD NOTICE RECORD                       
         USING DAREMGND,R2                                                      
         LR    RE,R2                                                            
         LA    RF,4000                                                          
         XCEFL                                                                  
         MVC   0(L'MNXKEY,R2),NOTCKEY                                           
         LA    R1,MNXFRST-MNXKEY  LENGTH OF RECORD W/O ELEMENTS                 
         STCM  R1,3,MNXRLEN                                                     
         MVC   MNXRAGY,POWERCDE    ALPHA AGENCY CODE                            
         XC    ELTAREA,ELTAREA                                                  
*                                                                               
MKGHD140 LA    R6,ELTAREA                                                       
         USING MNSTELD,R6                                                       
*                                                                               
         MVI   MNSTEL,MNSTELQ      ELEMENT CODE                                 
         MVI   MNSTLEN,MNSTLENQ    OVERHEAD LENGTH                              
*                                                                               
         LA    RE,MOHDDATE         MAKE SURE VALID NUMERIC                      
         LA    R1,L'MOHDDATE                                                    
MKGHD170 CLI   0(RE),C'0'                                                       
         JL    INVNMFLD                                                         
         CLI   0(RE),C'9'                                                       
         JH    INVNMFLD                                                         
         LA    RE,1(RE)                                                         
         BCT   R1,MKGHD170                                                      
*                                                                               
         GOTOR VDATCON,DMCB,(0,MOHDDATE),(19,MNSTDATE)                          
         PACK  PTIME,MOHDTIME      TIME                                         
         SRP   PTIME,1,0           SHIFT LEFT 1 DIGIT                           
         MVC   MNSTTIME,PTIME      PWOS TIME                                    
*                                                                               
         MVI   MNSTSTAT,MNSTNEW    STATUS NEW                                   
         TM    NEXISTS,EXONFILE    NOTICE RECORD EXISTS?                        
         BZ    MKGHD180            NO, DEFINITELY NEW                           
         TM    NEXISTS,EXDEL         WAS IT DELETED?                            
         BZ    MKGHD190              NO, DEFINITELY AMENDED                     
MKGHD180 GOTOR ADDEL,DMCB,(RECNSEQ,AIO2),(NEXISTS,0)                            
         B     MKGHD230                                                         
*                                                                               
MKGHD190 MVI   MNSTSTAT,MNSTAMND   STATUS AMMENDED                              
         GOTOR ADDEL,DMCB,(RECNSEQ,AIO2),(NEXISTS,0)                            
*                                                                               
         XC    FULL,FULL                                                        
         L     R6,AIO1             COPY THE '05' ELEMENTS                       
         MVI   ELCODE,5                                                         
         MVI   DATADISP+1,24                                                    
         CLI   QSTATN,C'0'         CABLE?                                       
         BL    *+8                                                              
         MVI   DATADISP+1,42                                                    
         BRAS  RE,GETEL                                                         
MKGHD200 BNE   MKGHD220                                                         
*                                                                               
         CLI   MNSTSTAT,MNSTOKAY                                                
         JE    ORDROKAY            OFFER ALREAY OKAYED                          
         CLI   MNSTSTAT,MNSTCAN                                                 
         JE    OFFRCANN            OFFER CANCELLED W/ NO MORE TO FOLLOW         
*                                                                               
         CLI   MNSTSTAT,MNSTDELV   DELIVERY NOTIFICATION                        
         BNE   *+16                                                             
         ST    R6,FULL                                                          
         BRAS  RE,NEXTEL           YES, SKIP TO NEXT STATUS                     
         B     MKGHD200                                                         
*                                                                               
         CLI   MNSTSTAT,MNSTNEW    NEW IS OKAY                                  
         BE    MKGHD210                                                         
*                                                                               
         CLI   MNSTSTAT,MNSTAMND   AMENDED IS OKAY ALSO                         
         BE    MKGHD210                                                         
*                                                                               
         CLI   MNSTSTAT,MNSTREJ    REJECT IS OKAY                               
         BE    MKGHD210                                                         
*                                                                               
         CLI   MNSTSTAT,MNSTCANM   CANCELLED W/ MORE TO FOLLOW?                 
         BE    MKGHD210                                                         
*                                                                               
         CLI   MNSTSTAT,MNSTERR    ERROR                                        
         JNE   INVNOCHG             NO, ERROR CAN'T CHANGE MKGD STATUS          
         CLI   MNSTLEN,MNSTLENQ     YES, ERROR WITHOUT CODE?                    
         JNE   INVNOCHG              NO, ERROR CAN'T CHANGE MKGD STATUS         
*                                                                               
MKGHD210 OC    FULL,FULL           DID WE SKIP A DELNOT?                        
         BZ    MKGHD215            NO                                           
         L     R6,FULL             YES, DON'T FORGET TO COPY DELNOT             
MKGHD215 XC    ELTAREA,ELTAREA                                                  
         ZIC   R1,MNSTLEN          ELEMENT LENGTH                               
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   ELTAREA(0),0(R6)    MOVE ELEMENT TO ELTAREA                      
         GOTOR ADDEL,DMCB,(RECNSEQ,AIO2),(NEXISTS,0)                            
         BRAS  RE,NEXTEL                                                        
         BE    MKGHD215                                                         
         DROP  R2,R6                                                            
*                                                                               
MKGHD220 DS    0H                                                               
         LA    R2,SAVEREJT                                                      
         LR    RE,R2                                                            
         LA    RF,SAVEREJX-SAVEREJT                                             
         XCEFL                                                                  
*                                                                               
         L     R6,AIO1             COPY THE REJECTION COMMENT                   
         USING MNMRJCD,R6                                                       
         MVI   ELCODE,MNMRJCQ                                                   
         LA    RF,24               SPOT REC HAS 24 BYTE HEADER                  
         CLI   QSTATN,C'0'         CABLE?                                       
         BL    *+8                                                              
         LA    RF,42               XSPT REC HAS 42 BYTE HEADER                  
         LA    R6,0(RF,R6)         PT TO 1ST ELEMENT                            
*                                                                               
         LA    R3,SAVEREJX         R3 = END OF REJ COMMENTS                     
MKGHD225 BRAS  RE,NEXTEL                                                        
         BNE   MKGHD226                                                         
         LLC   R1,MNMRLEN                                                       
         BCTR  R1,0                                                             
         MVC   0(0,R2),MNMRJC                                                   
         EX    R1,*-6                                                           
*                                                                               
         CR    R2,R3                                                            
         JNL   *+2                 ONLY SUPPORT 5 REJ COMMENTS                  
         LA    R2,L'SAVEREJT(R2)                                                
         B     MKGHD225                                                         
*                                                                               
MKGHD226 OI    RECFLAG,REJCOMNT                                                 
         DROP  R6                                                               
*                                                                               
MKGHD230 L     R2,AIO1             MAKEGOOD OFFER RECORD                        
         USING DAREMGOD,R2                                                      
         LR    RE,R2                                                            
         LA    RF,4000                                                          
         XCEFL                                                                  
*                                                                               
         CLI   QSTATN,C'0'         CABLE?                                       
         BNL   MKGHD240             YES                                         
*                                                                               
         MVC   0(L'MOKEY,R2),OFFRKEY                                            
         LA    R1,MORFRST-MOKEY    LENGTH OF RECORD W/O ELEMENTS                
         STCM  R1,3,MORLEN                                                      
         MVC   MORAGY,POWERCDE     ALPHA AGENCY CODE                            
         B     MKGHD250                                                         
*                                                                               
MKGHD240 MVC   0(L'MOXKEY,R2),OFFRKEY                                           
         LA    R1,MOXFRST-MOXKEY  LENGTH OF RECORD W/O ELEMENTS                 
         STCM  R1,3,MOXRLEN                                                     
         MVC   MOXRAGY,POWERCDE    ALPHA AGENCY CODE                            
         XC    ELTAREA,ELTAREA                                                  
         DROP  R2                                                               
*                                                                               
MKGHD250 XC    ELTAREA,ELTAREA                                                  
         LA    R2,ELTAREA                                                       
         USING MOSQELD,R2                                                       
         MVI   MOSQEL,MOSQELQ      ELEMENT CODE                                 
         PACK  DUB,MOHDSEQN        SEQUENCE/VERSION #                           
         CVB   R5,DUB                                                           
         STC   R5,MOSQNUM                                                       
         MVI   MOSQLEN,MOSQLNQ     ELEMENT LENGTH                               
         DROP  R2                                                               
*                                                                               
         L     R2,AIO1             ADD SEQUENCE ELEMENT TO OFFER RECORD         
         GOTOR ADDEL,DMCB,(RECOSEQ,AIO1),(OEXISTS,0)                            
*                                                                               
MKGHDRYS J     YES                                                              
*                                                                               
MKGHDRNO J     NO                                                               
         EJECT                                                                  
***********************************************************************         
* GET QSTATN                                                                    
***********************************************************************         
         USING STAPACKD,R6                                                      
GETQSTA  NTR1  LABEL=*                                                          
         OC    QSTATN,QSTATN       ALREADY GOT QSTATION?                        
         JNZ   YES                                                              
*                                                                               
         LA    R6,WORK                                                          
         XC    0(32,R6),0(R6)                                                   
         MVI   STAPACT,C'U'        MSUNPK                                       
         MVI   STAPCTRY,C'U'       UNITED STATES                                
         MVC   STAPACOM,ACOMFACS                                                
         MVC   STAPAGY,QAGY                                                     
         MVI   STAPMED,C'T'                                                     
         XC    STAPMKT,STAPMKT                                                  
         MVC   STAPSTA,BSTATN                                                   
*                                                                               
         GOTOR ASTAPACK,WORK                                                    
         CLI   STAPERR,0                                                        
         JNE   NO                  TEMP, NEEDS TO BE CHANGED  -HWON             
*                                                                               
         MVC   QSTATN,STAPQSTA                                                  
         CLI   QSTATN+4,C' '                                                    
         BNE   *+8                                                              
         MVI   QSTATN+4,C'T'                                                    
         J     YES                                                              
***********************************************************************         
*   RDUPDT:    CHECKS FOR EXISTING RECORDS WITH SAME KEYS                       
*                                                                               
* ON EXIT:     OFFRKEY HAS THE KEY OF THE SPOT OFFER RECORD                     
*              NOTCKEY HAS THE KEY OF THE SPOT NOTICE RECORD                    
***********************************************************************         
RDUPDT   NTR1                                                                   
*                                                                               
* READ SPOT OFFER RECORDS                                                       
*                                                                               
         XC    OFFRKEY,OFFRKEY                                                  
         LA    R2,OFFRKEY          MAKEGOOD OFFER RECORD                        
         USING DAREMGOD,R2                                                      
                                                                                
         MVI   MOKTYPE,MOKTYPQ     TYPE                                         
         MVI   MOKSUBTY,MOKSTYPQ   SUBTYPE                                      
         MVC   MOKAGMD,AGYMD       A/M                                          
         MVC   MOKORDER,BINORDER   BINARY ORDER NUMBER                          
         MVC   MOKMGCD,MOHDOFRI    GROUP CODE                                   
         DROP  R2                                                               
*                                                                               
         MVI   OEXISTS,0                                                        
*                                                                               
         MVC   KEY(L'OFFRKEY),OFFRKEY                                           
         LA    R2,KEY                                                           
         USING DAREMGOD,R2                                                      
         MVI   RECOSEQ,0                                                        
         MVI   MOKSEQ,1            DON'T NEED TO DELETE THE 0 RECORD            
*                                                                               
         MVI   DMINBTS,X'08'       READ FOR DELETED, BUT NOT UPDATE             
         BRAS  RE,HIGH                                      DON'T LOCK          
*                                                                               
         L     R2,AIO1                                                          
         ST    R2,AIO                                                           
RDUPDT10 CLC   KEYSAVE(MOKSEQ-MOKEY),KEY                                        
         BNE   RDUPDT40                                                         
*                                                                               
         MVI   DMINBTS,X'88'       READ FOR DELETED AND UPDATE (LOCK)           
         BRAS  RE,HIGH                                                          
*                                                                               
         MVI   DMINBTS,X'88'                                                    
         BRAS  RE,GET                                                           
*                                                                               
         CLI   MOKSEQ,MOKSEQDQ     X'FF' - SEQ FOR A DEMO RECORD?               
         BNE   RDUPDT30                                                         
         MVC   MORLEN,=X'0018'     REMOVE ALL THE ELEMENTS                      
         XC    24(255,R2),24(R2)                                                
         B     RDUPDT34            BUT DON'T SET DELETE BIT                     
*******                                                                         
RDUPDT30 MVC   RECOSEQ,MOKSEQ                                                   
         OI    MORSTAT,X'80'                                                    
RDUPDT34 BRAS  RE,PUT                                                           
*                                                                               
         CLI   MOKSEQ,MOKSEQDQ     X'FF' - SEQ FOR A DEMO RECORD?               
         BE    RDUPDT38                                                         
         OI    KEY+L'MOKEY,X'80'                                                
         BRAS  RE,WRITE                                                         
*                                                                               
RDUPDT38 MVI   DMINBTS,X'08'       SEQ FOR DELETED, BUT NOT UPDATE              
         BRAS  RE,SEQ                                       DON'T LOCK          
         B     RDUPDT10                                                         
*                                                                               
RDUPDT40 MVC   KEY,OFFRKEY                                                      
         MVI   DMINBTS,X'08'                                                    
         BRAS  RE,HIGH                                                          
*                                                                               
         CLC   OFFRKEY(L'MOKEY),KEY                                             
         BNE   RDUPDT50                                                         
*                                                                               
         OI    OEXISTS,EXONFILE    OFFER RECORD EXISTS                          
         TM    KEY+13,X'80'        DELETED?                                     
         BZ    *+8                                                              
         OI    OEXISTS,EXDEL       YES                                          
         DROP  R2                                                               
*                                                                               
* READ SPOT NOTICE RECORDS                                                      
*                                                                               
RDUPDT50 XC    NOTCKEY,NOTCKEY                                                  
         LA    R2,NOTCKEY          MAKEGOOD NOTICE RECORD                       
         USING DAREMGND,R2                                                      
*                                                                               
         MVI   MNKTYPE,MNKTYPQ     TYPE                                         
         MVI   MNKSUBTY,MNKSTYPQ   SUBTYPE                                      
         MVC   MNKAGMD,AGYMD       A/M                                          
         MVC   MNKBYR,BUYER        BUYER                                        
         MVC   MNKORDER,BINORDER   BINARY ORDER NUMBER                          
         MVC   MNKGROUP,GROUPID    GROUP CODE                                   
         MVI   MNKTYPE,MNKTYPQ     TYPE                                         
*                                                                               
         MVC   KEY(L'NOTCKEY),NOTCKEY                                           
         MVI   NEXISTS,0                                                        
*                                                                               
         MVI   DMINBTS,X'08'                                                    
         BRAS  RE,HIGH                                                          
*                                                                               
         CLC   KEYSAVE(L'MNKEY),KEY                                             
         BNE   RDUPDTX                                                          
*                                                                               
         MVI   DMINBTS,X'88'                                                    
         MVC   AIO,AIO1                                                         
         BRAS  RE,GET                                                           
*                                                                               
         OI    NEXISTS,EXONFILE    NOTICE RECORD EXISTS                         
         TM    IO1+15,X'80'        DELETED?                                     
         BZ    *+8                                                              
         OI    NEXISTS,EXDEL       YES                                          
* WHY???? , WE OVERWRITE THE RECORD ENTIRELY LATER  -HWON 1/9/2014              
****     NI    IO1+15,X'FF'-X'80'            UNDELETE                           
****     BRAS  RE,PUT                                                           
* WHY???? , WE OVERWRITE THE RECORD ENTIRELY LATER                              
RDUPDTX  J     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
*   RDXPDT:    CHECKS FOR EXISTING RECORDS WITH SAME KEYS                       
*                                                                               
*  ON EXIT:    OFFRKEY HAS THE KEY OF THE XSPOT OFFER RECORD                    
*              NOTCKEY HAS THE KEY OF THE XSPOT NOTICE RECORD                   
***********************************************************************         
RDXPDT   NTR1                                                                   
         XC    OFFRKEY,OFFRKEY                                                  
         LA    R2,OFFRKEY          MAKEGOOD OFFER RECORD                        
         USING DAREMGOD,R2                                                      
         MVI   MOXKTYPE,MOXKTYPQ   TYPE                                         
         MVI   MOXKSBTY,MOXKSBTQ   SUBTYPE                                      
         MVC   MOXKAGMD,AGYMD      A/M                                          
         MVC   MOXKORDR,BINORDER   BINARY ORDER NUMBER                          
         MVC   MOXKMGCD,MOHDOFRI   GROUP CODE                                   
         DROP  R2                                                               
*                                                                               
*****                                                                           
* READ OFFER RECORDS AND CLEAR AND DELETE                                       
*****                                                                           
         MVC   KEY(L'OFFRKEY),OFFRKEY                                           
         MVI   OEXISTS,0                                                        
*                                                                               
         LA    R2,KEY                                                           
         USING DAREMGOD,R2                                                      
         MVI   RECOSEQ,0                                                        
         MVI   MOXKSTTN,1          DON'T NEED TO DELETE THE 0 RECORD            
         MVI   DMINBTS,X'08'       READ XHIGH FOR DELETED                       
         BRAS  RE,XHIGH                                                         
*                                                                               
         L     R2,AIO1                                                          
         ST    R2,AIO                                                           
RDXPDT10 CLC   KEYSAVE(MOXKSTTN-MOXKEY),KEY                                     
         BNE   RDXPDT40                                                         
*                                                                               
         MVI   DMINBTS,X'88'       READ XHIGH FOR DELETED                       
         BRAS  RE,XHIGH                                                         
*                                                                               
         MVI   DMINBTS,X'88'       READ XGET FOR DELETED                        
         BRAS  RE,XGET                                                          
*                                                                               
*** REMOVE ALL ELEMENTS FOR ALL RECORDS, -HWON 6/19/2014                        
***      CLI   MOXKDTYP,MOXKDDMQ    X'FF' - DEFAULT DEMO RECORD?                
***      BNE   RDXPDT20                                                         
***                                                                             
         MVC   MOXRLEN,=X'002A'     REMOVE ALL THE ELEMENTS                     
         XC    42(255,R2),42(R2)                                                
*** AND DELETE ALL RECORDS, -HWON 6/19/2014                                     
***      B     RDXPDT25            BUT DON'T SET DELETE BIT                     
*                                                                               
RDXPDT20 OI    MOXRSTAT,X'80'                                                   
         OC    MOXKSTTN,MOXKSTTN                                                
         BNZ   RDXPDT25                                                         
         MVC   RECOSEQ,MOXKSEQ     STORE LAST RECORD SEQUENCE #                 
RDXPDT25 BRAS  RE,XPUT                                                          
*                                                                               
*** AND DELETE ALL RECORDS, -HWON 6/19/2014                                     
***      CLI   MOXKDTYP,MOXKDDMQ    X'FF' - DEFAULT DEMO RECORD?                
***      BE    RDXPDT30                                                         
         OI    KEY+L'MOXKEY,X'80'                                               
         BRAS  RE,XWRITE                                                        
*                                                                               
RDXPDT30 MVI   DMINBTS,X'08'                                                    
         BRAS  RE,XSEQ                                                          
         B     RDXPDT10                                                         
*                                                                               
RDXPDT40 MVC   KEY,OFFRKEY                                                      
         MVI   DMINBTS,X'08'                                                    
         BRAS  RE,XHIGH                                                         
*                                                                               
         CLC   OFFRKEY(L'MOXKEY),KEY                                            
         BNE   RDXPDT50                                                         
*                                                                               
         OI    OEXISTS,EXONFILE    OFFER RECORD EXISTS                          
         TM    IO1+MOXRSTAT-MOXKEY,X'80'   DELETED?                             
         BZ    *+8                                                              
         OI    OEXISTS,EXDEL       YES                                          
         DROP  R2                                                               
******                                                                          
* READ NOTICE RECORDS                                                           
******                                                                          
RDXPDT50 XC    NOTCKEY,NOTCKEY                                                  
         LA    R2,NOTCKEY          MAKEGOOD NOTICE RECORD                       
         USING DAREMGND,R2                                                      
         MVI   MNXKTYPE,MNXKTYPQ   TYPE                                         
         MVI   MNXKSBTY,MNXKSBTQ   SUBTYPE                                      
         MVC   MNXKAGMD,AGYMD      A/M                                          
         MVC   MNXKORDR,BINORDER   BINARY ORDER NUMBER                          
         MVC   MNXKGRP,GROUPID     GROUP CODE                                   
         DROP  R2                                                               
                                                                                
         MVC   KEY(L'OFFRKEY),NOTCKEY                                           
         MVI   NEXISTS,0                                                        
*                                                                               
         LA    R2,KEY                                                           
         USING DAREMGND,R2                                                      
         MVI   RECNSEQ,0                                                        
         MVI   MNXKSEQ,1           DON'T NEED TO DELETE THE 0 RECORD            
         MVI   DMINBTS,X'08'                                                    
         BRAS  RE,XHIGH                                                         
*                                                                               
         L     R2,AIO1                                                          
         ST    R2,AIO                                                           
RDXPDT60 CLC   KEYSAVE(MNXKSTTN-MNXKEY),KEY                                     
         BNE   RDXPDT90                                                         
*                                                                               
         MVI   DMINBTS,X'88'       RDHI FOR DELETED                             
         BRAS  RE,XHIGH                                                         
*                                                                               
         MVI   DMINBTS,X'88'                                                    
         BRAS  RE,XGET             GET FOR DELETED                              
*                                                                               
         OC    MNXKSTTN,MNXKSTTN                                                
         BNZ   RDXPDT70                                                         
         MVC   RECNSEQ,MNXKSEQ     STORE LAST RECORD SEQUENCE #                 
*                                                                               
RDXPDT70 MVC   MNXRLEN,=X'002A'     REMOVE ALL THE ELEMENTS                     
         XC    42(255,R2),42(R2)                                                
         OI    MNXRSTAT,X'80'      DELETE THE XSPOT RECORD                      
         BRAS  RE,XPUT                                                          
*                                                                               
         OI    KEY+L'MNXKEY,X'80'  DELETE THE XSPOT KEY                         
         BRAS  RE,XWRITE                                                        
*                                                                               
         MVI   DMINBTS,X'08'       READ SEQ FOR DELETED                         
         BRAS  RE,XSEQ                                                          
         B     RDXPDT60                                                         
*                                                                               
RDXPDT90 MVC   KEY(L'NOTCKEY),NOTCKEY                                           
         MVC   KEYSAVE(L'NOTCKEY),NOTCKEY                                       
         MVI   DMINBTS,X'08'                                                    
         BRAS  RE,XHIGH                                                         
*                                                                               
         CLC   KEYSAVE(L'MNXKEY),KEY                                            
         BNE   RDXPDTX                                                          
*                                                                               
         MVI   DMINBTS,X'88'                                                    
         MVC   AIO,AIO1                                                         
         BRAS  RE,XGET                                                          
*                                                                               
         OI    NEXISTS,EXONFILE    NOTICE RECORD EXISTS                         
         TM    IO1+MNXRSTAT-MNXKEY,X'80'   DELETED?                             
         BZ    *+8                                                              
         OI    NEXISTS,EXDEL       YES                                          
*                                                                               
         NI    IO1+MNXRSTAT-MNXKEY,X'FF'-X'80'   UNDELETE                       
         BRAS  RE,XPUT                                                          
*                                                                               
RDXPDTX  J     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* GET THE ID NUMBER FOR A SPECIFIC USER ID                                      
*                                                                               
* ON ENTRY:    (P1)                A(USER ID)                                   
*              (P2)                A(ID NUMBER)                                 
***********************************************************************         
GETIDNUM NTR1                                                                   
         LM    R2,R3,DMCB                                                       
         XC    0(2,R3),0(R3)                                                    
*                                                                               
         MVI   DMCB,X'0A'          SWITCH TO CONTROL SYSTEM                     
         BRAS  RE,SWTCHSYS         SUCCESSFUL SWITCH?                           
         BNE   GIDNMNO             NO                                           
*                                                                               
         XC    KEY,KEY             LOOK FOR THE ID RECORD                       
         LA    R4,KEY                                                           
         USING CTIREC,R4                                                        
         MVI   CTIKTYP,CTIKTYPQ                                                 
         MVC   CTIKID,0(R2)        INSERT USER ID                               
         DROP  R4                                                               
*                                                                               
         GOTOR VDATAMGR,DMCB,DMRDHI,CTFILE,KEY,AIO1                             
*                                                                               
         L     R6,AIO1                                                          
         USING CTIREC,R6                                                        
         CLC   CTIKEY,KEY          ERR IF WE CAN'T FIND THE ID REC              
         BNE   GIDNMNO                                                          
*                                                                               
         SR    R0,R0                                                            
         LA    R6,CTIDATA                                                       
         USING CTSYSD,R6                                                        
GIDNM10  CLI   0(R6),0                                                          
         BE    GIDNMNO             ERROR IF NO DESCRIPTION ELEM                 
         CLI   0(R6),2                                                          
         BE    GIDNM20                                                          
         IC    R0,1(R6)                                                         
         AR    R6,R0                                                            
         B     GIDNM10                                                          
*                                                                               
GIDNM20  MVC   0(2,R3),2(R6)                                                    
*                                                                               
         MVC   DMCB(1),SPTSENUM    SWITCH TO REP  SYSTEM                        
         BRAS  RE,SWTCHSYS         SUCCESSFUL SWITCH?                           
         BNE   GIDNMNO                                                          
*                                                                               
GIDNMYES J     YES                                                              
*                                                                               
GIDNMNO  J     NO                                                               
         DROP  R6                                                               
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*   MKGCDC: MAKEGOOD COMSCORE DEMO CATEGORIES                                   
***********************************************************************         
MKGCDC   NTR1  BASE=*,LABEL=*                                                   
         TM    RECFLAG,INPROG      MAKEGOOD RECORDS IN PROGRESS?                
         JNO   BADSQMGO                                                         
*                                                                               
         CLI   LASTREC,DS1         OFFER COMMENT                                
         JE    MKGCDC10                                                         
         CLI   LASTREC,DS2         TRADITIONAL DEMO CATEGORIES?                 
         JE    MKGCDC10                                                         
         CLI   LASTREC,HDR         MAKEGOOD HEADER                              
         JNE   BADSQMGO                                                         
*                                                                               
MKGCDC10 MVI   LASTREC,CDC         SET LAST RECORD TYPE                         
         L     R1,MKGCOUNT         INCREMENT RECORD COUNT                       
         LA    R1,1(R1)                                                         
         ST    R1,MKGCOUNT                                                      
*                                                                               
         LA    R4,R+1              SET INPUT RECORD DSECT                       
         USING MOFRCDCD,R4                                                      
*                                                                               
         CLC   ORDNO,MCDCORDR      ORDER #                                      
         JNE   INCNDATA             MUST MATCH                                  
*                                                                               
         MVI   DMCB,X'0A'          SWITCH TO THE CONTROL SYSTEM                 
         BRAS  RE,SWTCHSYS                                                      
         BNE   EXITPRG                                                          
         BRAS  RE,GETCSLIC                                                      
         MVC   DMCB(1),SPTSENUM    SWITCH BACK TO SPOT SYSTEM                   
         BRAS  RE,SWTCHSYS                                                      
*                                                                               
         OC    SVCSLIC,SVCSLIC     AGENCY AUTHORIZED FOR COMSCORE?              
         JZ    INVMKCDC             NO                                          
*                                                                               
         XC    SVNTDCEL,SVNTDCEL                                                
         LA    R2,SVNTDCEL         SELLER NON-TRAD DEMO VALUES ELEM             
         USING MOXCELD,R2                                                       
         MVI   MOXCEL,MOXCELQ      X'18' - SELLER NON-TRAD OXCODES              
*                                                                               
         MVI   RCHEMPTD,C'N'       EMPTY DEMO CATEGORY FLAG                     
         LA    R3,10               MAX 10 DEMO CATEGORIES FOR MKGCDC            
MKGCDC20 CLI   MCDCDEM1,C'R'       RATING CATEGORY?                             
         JE    MKGCDC30             YES                                         
         CLI   MCDCDEM1,C'I'       IMPRESSION CATEGORY?                         
         JE    MKGCDC30             YES                                         
         CLC   MCDCDEM1,SPACE      HAVE EMPTY DEMO?                             
         JNZ   INVMKCDC             NO, SEND ERROR                              
         MVI   RCHEMPTD,C'Y'        YES, HAVE EMPTY DEMO CAT                    
         J     MKGCDC60            NEXT DEMO CATEGORY                           
*                                                                               
MKGCDC30 CLI   RCHEMPTD,C'Y'       WE HAD AN EMPTY DEMO CATEGORY B4?            
         JE    INVMKCDC            YES, SEND ERROR                              
*                                                                               
         GOTOR VCASHVAL,DMCB,(0,MCDCDEM1+1),(X'40',10)                          
         CLI   0(R1),0             CHECK RETURN CODE                            
         JNE   INVMKCDC             SEND ERROR                                  
*                                                                               
         XC    DBLOCK(256),DBLOCK  SETUP DBLOCK FOR DEMOCON CALL                
         MVC   DBCOMFCS,ACOMFACS                                                
         MVC   DBFILE,=C'TP '                                                   
         MVI   DBSELMED,C'T'                                                    
*                                                                               
         MVC   WORK(12),FOXZEROS   SETUP SEARCH STRING FOR DEMOCON              
         MVC   WORK+2(L'MCDCDEM1-1),MCDCDEM1+1  COPY ONLY NUMERIC PART          
*                                                                               
*                                  FOR CUSTOM COMSCORE DEMO VALIDATION          
         MVC   WORK+20(4),=C'DEXB' SET UNIQUE TABLE ID AND                      
         LA    RF,SVCSLIC                                                       
         ST    RF,WORK+24          PASS A(COMSCORE LICENSE)                     
*                                                                               
         GOTO1 VDEMOCON,DMCB,(C'N',WORK),(20,ELEM),(C'S',DBLOCK),               
               WORK+20,0,0                                                      
         CLI   DMCB,X'FF'          HAVE ERROR?                                  
         JE    INVMKCDC             YES, CAN'T CONTINUE SEND ERROR              
         OC    ELEM(CSDLNQ),ELEM   DID WE GET CSDTABD BACK?                     
         JZ    INVMKCDC             NO, INVALID MAKEGOOD DEMO                   
*                                                                               
CSL      USING CSDTABD,ELEM                                                     
         LA    R1,MOXCDEMO         START BUILDING MOXCDEMO ENTRIES              
         CLI   MCDCDEM1,C'R'       PROC A RATING DEMO?                          
         JNE   MKGCDC40                                                         
         MVI   MOXCDEMO,C'R'       YES - SET RATING IN MOXCELD                  
         LA    R1,1(R1)                                                         
*                                                                               
MKGCDC40 MVI   0(R1),C'X'          SO WE KNOW IT IS AN OXCODE                   
         LA    RE,MOXCDEMO-1-1     -1(FOR X) & -1(FOR EX INSTR)                 
         SR    RE,R1                                                            
         MVC   1(0,R1),CSL.CSDCODE MOVE THE OXCODE IN PLACE                     
         EX    RE,*-6                                                           
*                                                                               
         LA    R2,L'MOXCDEMO(R2)   BUMP TO NEXT DEMO IN SVNTDCEL                
MKGCDC60 LA    R4,L'MCDCDEM1(R4)   BUMP TO NEXT DEMO IN MKGCDC                  
         BCT   R3,MKGCDC20                                                      
*                                                                               
         LA    RF,SVNTDCEL                                                      
         CR    R2,RF               DID WE GET ANY DEMO CATEGORIES?              
         JE    INVMKCDC            NONE, SEND ERROR                             
         LA    R2,MOXCOVRH(R2)     ADJUST FOR ELCODE AND ELLEN                  
         SR    R2,RF                                                            
         STC   R2,SVNTDCEL+1       LENGTH OF THE ELEMENT                        
         DROP  R2                                                               
*                                                                               
         BRAS  RE,CKESTREC         CONFIRM COMSCORE DEMOS ON ESTIMATE           
         JNE   INVMKCDC            INVALID MAKEGOOD DEM                         
         B     YES                                                              
***********************************************************************         
* CHECK NON-TRAD DEMO CATEGORIES AGAINST THE ESTIMATE RECORD                    
*                                                                               
* ON ENTRY:    AIO1                MAKEGOOD OFFER RECORD                        
*                                                                               
* ON EXIT:     CC                  EQ - NON-TRAD DEMO CATEGORIES OK             
*              AIO3                WILL BE USED FOR THE ESTIMATE REC            
***********************************************************************         
CKESTREC NTR1                                                                   
         XC    KEY,KEY                LOOK AT THE POL ESTIMATE FOR THE          
         LA    R6,KEY                   NON-TRAD DEMO CATEGORY ELEM             
         USING EKEY,R6                                                          
         MVC   EKEYAM,AGYMD                                                     
         MVC   EKEYCLT,BCLT                                                     
         MVC   EKEYPRD,=C'POL'                                                  
         MVC   EKEYEST,BEST                                                     
*                                                                               
         BRAS  RE,HIGH                                                          
         CLC   KEYSAVE(EKCNTRL-EKEY),KEY                                        
         JNE   CKESTRCN               ESTIMATE DOES NOT EXIST? NO WAY           
         L     R6,AIO3                READ ESIMATE INTO AIO3                    
         ST    R6,AIO                                                           
         BRAS  RE,GET                                                           
*                                                                               
         LA    R2,SVNTDCEL            A(INCOMING DEMO CATEGORY NAMES)           
         USING MOXCELD,R2                                                       
         LA    RE,SVNTDMEL            A(SELLER NON-TRAD DMO VALUES)             
         USING MONTELD,RE                                                       
         MVI   MONTEL,MOSNTELQ        SELLER DEMO ELEM CODE                     
         LLC   RF,MOXCLEN                                                       
         SHI   RF,MOXCOVRH                                                      
*                                                                               
CESTRC10 LA    R3,1                   BINARY NON-TRAD DEMO # FROM EST           
         LA    R4,ENONTDMS            A(EST'S NON-TRAD CATEGORY NAMES)          
CESTRC15 CLC   MOXCDEMO,0(R4)         FIND THE NON-TRAD DEMO CATEGORY?          
         BE    CESTRC20               YES                                       
         LA    R4,L'ENONTDMS(R4)                                                
         LA    R3,1(R3)                                                         
         CHI   R3,50                  MAX OF 50 NON-TRAD DEMOS                  
         BNH   CESTRC15                                                         
         B     CKESTRCN               BEYOND THE MAX, INVALID                   
*                                                                               
CESTRC20 XC    MONTDEMO,MONTDEMO                                                
         STC   R3,MONTBDMO+1          BINARY NON-TRAD DEMO #                    
         MVI   MONTHUTA,X'64'         SET DEFAULT HUT ADJ OF 100                
*                                                                               
         LA    R2,L'MOXCDEMO(R2)                                                
         LA    RE,L'MONTDEMO(RE)                                                
         SHI   RF,L'MOXCDEMO          THIS TELL US HOW MANY TO DO               
         BP    CESTRC10                                                         
         DROP  R2,RE                                                            
*                                                                               
         LA    R0,SVNTDMEL                                                      
         SR    RE,R0                                                            
         AHI   RE,MONTOVRH                                                      
         STC   RE,SVNTDMEL+1          LENGTH OF THE DEMO VALUES ELEM            
*                                                                               
CKESTRCY B     YES                                                              
CKESTRCN B     NO                                                               
         DROP  R6                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* GET THE COMSCORE SECURITY LICSENSE FROM TOKEN RECORD                          
*                                                                               
* NOTE:  AIO3 WILL BE CLOBBERED TO READ THE TOKEN RECORD                        
***********************************************************************         
GETCSLIC NTR1  LABEL=*                                                          
*                                                                               
         MVC   SVCSLIC,SPACE       INIT TO SPACES                               
         MVC   HALF,QAGY           AGENCY CODE                                  
                                                                                
         XC    KEY,KEY             CLEAR THE KEY                                
         LA    R4,KEY              R4 = KEY                                     
         USING CT5KEY,R4           SYSTEM ACCESS RECORD DSECT                   
         MVI   CT5KTYP,CT5KTYPQ    X'05'                                        
         MVC   CT5KALPH,QAGY       AGENCY ALPHA                                 
         GOTO1 VDATAMGR,DMCB,(0,=C'DMREAD '),=C'CTFILE ',KEY,AIO3               
         L     R4,AIO3             A(SYSTEM ACCESS RECORD)                      
         LA    R5,CT5DATA          X'01' ACTIVITY ELEMENT                       
         USING CTSEAD,R5           SECURITY AGENCY APLHA ID ELEMENT             
         MVI   ELCODE,CTSEAELQ     X'B8'                                        
         BRAS  RE,FIRSTEL          HAVE X'B8' ELEMENT?                          
         BNE   *+10                NO                                           
         MVC   HALF,CTSEAAID       YES - SET SECURITY AGENCY                    
         DROP  R5                  DROP R5                                      
*                                                                               
         LA    R4,KEY                                                           
         USING TOKKEY,R4                                                        
         XC    KEY,KEY             X'00'                                        
         MVI   TOKKMIN,TOKKMINQ    C'K'                                         
         MVI   TOKKTYP,TOKKRTRK    X'01' - RENTRAK RECORD                       
         MVC   TOKKSAGY,HALF       SECURITY AGENCY CODE                         
         MVC   TOKKAAGY,QAGY       AGENCY ALPHA CODE                            
         MVI   TOKKSYS,X'02'       SPOT SYSTEM                                  
*                                                                               
         MVC   KEYSAVE,KEY                                                      
         GOTO1 VDATAMGR,DMCB,(0,=C'DMRDHI '),=C'GENDIR ',KEYSAVE,KEY            
                                                                                
         CLC   TOKKEY,KEYSAVE      FOUND A MATCH?                               
         JNE   GTCSLNO             NO - DONE                                    
                                                                                
         GOTO1 VDATAMGR,DMCB,=C'GETREC',=C'GENFIL ',KEY+36,AIO3,WORK            
*                                                                               
         L     R4,AIO3                                                          
         LA    R4,TOKFIRST(R4)     R4=A(1ST ELEMENT)                            
GTCSL10  CLI   0(R4),0             ANY ELEMENTS?                                
         JE    GTCSLNO                                                          
         CLI   0(R4),RTAUTELQ      X'0A' - RENTRAK AUTHOR ELEM?                 
         JE    GTCSL30                                                          
         LLC   R0,1(R4)            CHECK THE NEXT ELEMENT                       
         AR    R4,R0                                                            
         J     GTCSL10                                                          
*                                                                               
         USING RTAUTHD,R4                                                       
GTCSL30  CLC   RTAUTID,SPACE       LICENSE ID BETTER BE > SPACES                
         JNH   GTCSLNO                                                          
         MVC   SVCSLIC,RTAUTID     LICENSE ID                                   
         DROP  R4                                                               
*                                                                               
GTCSLYES J     YES                                                              
GTCSLNO  J     NO                                                               
*                                                                               
***********************************************************************         
*   MKGCDV: MAKEGOOD BUY COMSCORE DEMOS                                         
***********************************************************************         
MKGCDV   NTR1  BASE=*,LABEL=*                                                   
         TM    RECFLAG,INPROG      MAKEGOOD RECORDS IN PROGRESS?                
         JNO   BADSQMGO                                                         
*                                                                               
         CLI   LASTREC,BUY         MAKEGOOD OFFER BUYLINE?                      
         JE    MKGCDV10                                                         
         CLI   LASTREC,OAAU        OFFERED AUTO-AVAILS UUID?                    
         JE    MKGCDV10                                                         
         CLI   LASTREC,DEM         OR MAKEGOOD DEMO  BEFORE THIS?               
         JNE   BADSQMGO                                                         
*                                                                               
MKGCDV10 MVI   LASTREC,CDV         YES, THEN WE'RE OKAY                         
         L     R1,MKGCOUNT         INCREMENT RECORD COUNT                       
         LA    R1,1(R1)                                                         
         ST    R1,MKGCOUNT                                                      
*                                                                               
         LA    R4,R+1              SET INPUT RECORD DSECT                       
         USING MOFRCDVD,R4                                                      
*                                                                               
         OC    SVNTDCEL,SVNTDCEL   DID SELLER SEND MKGCDC?                      
         JZ    INVMKCDV             NO, SEND ERROR                              
*                                                                               
         LA    R3,SVNTDCEL         WE NEED THIS SO WE KNOW IF THE DEMO          
         USING MOXCELD,R3            CATEGORY IS RATINGS OR IMPRESSIONS         
         LLC   R0,MOXCLEN                                                       
         AR    R0,R3               BYTE AFTER ALL THE DEMOS                     
         XC    ELTAREA,ELTAREA                                                  
         LA    R2,ELTAREA                                                       
         USING MONTELD,R2                                                       
         MVC   ELTAREA(L'SVNTDMEL),SVNTDMEL  SELLER DEMO VALUES ELEM            
         MVC   MONTOFFR,OFFERNUM   OFFER NUMBER                                 
         MVC   MONTREC,BUYCOUNT    RECORD NUMBER (BUY HEADER COUNT)             
*                                                                               
         MVI   RCHEMPTD,C'N'       EMPTY DEMO FLAG                              
         LA    R5,10               MAX OF 10 VALUES FOR MKGCDV                  
MKGCDV20 CLC   MCDVVAL,SPACE       HAVE A VALUE?                                
         BH    MKGCDV30            YES                                          
         LA    RE,MOXCDEMO         NO VALUE, DO WE HAVE A CATEGORY?             
         CR    RE,R0                                                            
         JL    INVMKCDV            YES, SEND INVALID MKGCDV ERROR               
         MVI   RCHEMPTD,C'Y'         TURN ON EMPTY DEMO FLAG                    
         B     MKGCDV60                                                         
*                                                                               
MKGCDV30 CLI   RCHEMPTD,C'Y'       RECEIVED EMPTY DEMO ALREADY?                 
         JE    INVMKCDV            YES, SEND INVALID MKGCDV ERROR               
*                                                                               
         XC    DMCB+4(4),DMCB+4                                                 
         MVI   DMCB+4,1            ONE DECIMAL FOR IMPRESSIONS                  
         CLI   MOXCDEMO,C'R'       RATING?                                      
         JNE   *+8                                                              
         MVI   DMCB+4,2            YES, 2 DECIMAL PLACES FOR RATINGS            
*                                                                               
         GOTO1 DECIVAL,DMCB,(L'MCDVVAL,MCDVVAL),,0                              
         MVC   MONTDVAL,DMCB+4                                                  
*                                                                               
         OI    MONTDVAL,MONTOVRD    SET X'80' OVERRIDE BIT                      
         CLI   MOXCDEMO,C'R'        RATING?                                     
         BNE   *+8                                                              
         OI    MONTDVAL,MONT2DEC    SET X'40' 2-DEC VALUE                       
*                                                                               
MKGCDV60 LA    R2,L'MONTDEMO(R2)                                                
         LA    R3,L'MOXCDEMO(R3)                                                
         LA    R4,L'MCDVVAL(R4)                                                 
         BCT   R5,MKGCDV20                                                      
*                                                                               
         GOTOR ,DMCB,(RECOSEQ,AIO1),(OEXISTS,0)                                 
         CLI   QSTATN,C'0'         CABLE?                                       
         BL    MKGCDVX                                                          
         TM    RECFLAG2,ONWINPRG   MAKEGOOD ONW IN PROGRESS                     
         JZ    MSCBLONW             -ERROR, MISSING CABLE MKGONW                
         GOTOR ,DMCB,('MOXKSDMQ',AIO5),(CODEXIST,0)                             
         OI    RECFLAG2,DEMINPRG   MAKEGOOD SELLER DEMO IN PROGRESS             
*                                                                               
MKGCDVX  GOTOR ADDEL,DMCB                                                       
         BNE   NO                                                               
         B     YES                                                              
         DROP  R4,R2                                                            
         EJECT                                                                  
***********************************************************************         
* THIS ROUTINE WILL CONSUME A DECIMAL STRING AND ITS VALUE WILL BE              
* STORED IN THE RETURN VALUE * 10 RAISED TO THE NUMBER OF DECIMALS TO           
* KEEP.                                                                         
*                                                                               
* I.E. 23.56456 KEEPING 2 DECIMALS WOULD BE STORED AS   2356                    
*      23.56456 KEEPING 4 DECIMALS WOULD BE STORED AS   235646                  
*                                                                               
* ON ENTRY:    PARAM 1 - BYTE 0    L'FIELD                                      
*                        BYTES 1-3 A(FIELD)                                     
*                                                                               
*              PARAM 2 - BYTE 0    # OF DECIMAL PLACES TO KEEP                  
*                                                                               
* ON EXIT:     CC                  EQ=VALID  NEQ=INVALID                        
*              PARAM 2             RETURN VALUE - FULL WORD                     
***********************************************************************         
DECIVAL  NTR1                                                                   
         LLC   R2,DMCB+4           R2 = # OF DECIMAL PLACES TO KEEP             
         LLC   R1,DMCB             R1 = L'FIELD                                 
         XR    RE,RE               RE = A(FIELD)                                
         ICM   RE,7,DMCB+1                                                      
         LA    RF,0(R1,RE)         RF = LAST CHARACTER IN FIELD                 
         BCTR  RF,0                                                             
*                                                                               
         XR    R0,R0               A(DECIMAL POINT)                             
         XR    R4,R4               R4 = # OF DECIMAL PLACES IN STRING           
DCVL20   CLI   0(RF),C'.'          WE FOUND A DECIMAL POINT                     
         JE    DCVL26              YES                                          
         CLI   0(RF),C'0'          IF NOT '.', THEN IT BETTER BE                
         JL    DECIVALN              A DECIMAL DIGIT                            
         CLI   0(RF),C'9'                                                       
         JH    DECIVALN                                                         
DCVL23   BCTR  RF,0                                                             
         CR    RF,RE               EXHAUSTED ALL CHARACTERS?                    
         JNL   DCVL20                                                           
         J     DCVL30                                                           
*                                                                               
DCVL26   LTR   R0,R0               R0 HOLDS A(DECIMAL POINT)                    
         JNZ   DECIVALN            ONLY 1 DECIMAL POINT ALLOWED IN STR          
*                                                                               
         LR    R4,R1               R4 = # OF DECIMAL PLACES IN STRING           
         SR    R4,RF                    LENGTH-RF+RE-1                          
         AR    R4,RE                                                            
         BCTR  R4,0                                                             
*                                                                               
         LR    R0,RF               WE NOW HAVE A DECIMAL POINT                  
         J     DCVL23              CHECK DIGITS IN REST OF STRING               
***************                                                                 
* STRING IS VALIDATED                                                           
***************                                                                 
DCVL30   LR    R3,R1               R3 = LENGTH OF INPUT                         
         LTR   R0,R0               DID WE FIND A DECIMAL POINT?                 
         JZ    DCVL40              NO, STRAIGHT TO PACK                         
*                                                                               
         LTR   R4,R4               ANY DECIMAL PLACES?                          
         JZ    DCVL35              NONE, DECIMAL IS AT LAST CHARACTER           
*                                                                               
         LR    RF,R0               POINT RF BACK TO THE DECIMAL POINT           
         LR    R3,R4                                                            
         BCTR  R3,0                                                             
         EX    R3,*+8              GET RID OF THE DECIMAL POINT                 
         J     *+10                                                             
         MVC   0(0,RF),1(RF)                                                    
         LR    R3,R1               R3 = ADJUSTED LENGTH OF INPUT                
DCVL35   BCTR  R3,0                  AS WE REMOVED THE DECIMAL POINT            
*                                                                               
DCVL40   BCTR  R3,0                DECR FOR THE EX INSTR                        
         EX    R3,*+8                                                           
         J     *+10                                                             
         PACK  DUB,0(0,RE)         NOW WE CAN PACK THE DIGITS                   
*                                                                               
         LR    R3,R2                                                            
         SR    R3,R4               HOW MUCH TO SHIFT                            
         JNM   *+8                 LEFT SHIFT, MULTIPLYING                      
         AHI   R3,64               RIGHT SHIFT, DIVIDING                        
         SRP   DUB,0(R3),5                                                      
*                                                                               
         CP    DUB,=P'2147483647'  MAX VALUE THAT CAN BE  CVB'D                 
         JH    DECIVALN                                                         
*                                                                               
         CVB   R4,DUB                                                           
         ST    R4,DMCB+4           SAVE OFF THE FULLWORD VALUE                  
DECIVALY J     YES                                                              
*                                                                               
DECIVALN J     NO                                                               
*                                                                               
***********************************************************************         
*   MKGTLR: MAKEGOOD OFFER TRAILER                                              
***********************************************************************         
MKGTLR   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         TM    RECFLAG,INPROG      MAKEGOOD RECORDS IN PROGRESS?                
         JNO   BADSQMGO                                                         
         NI    RECFLAG,X'FF'-INPROG                                             
*                                                                               
         CLI   LASTREC,MSS         MISSED SPOT                                  
         BE    MKGTLR00                                                         
         CLI   LASTREC,MAAU        MISSED AAU                                   
         BE    MKGTLR00                                                         
         CLI   LASTREC,ORB         ORBIT MISSED SPOT                            
         BE    MKGTLR00                                                         
         CLI   LASTREC,COM         COMMENT LINE                                 
         BE    MKGTLR00                                                         
         CLI   LASTREC,DTL         DETAIL LINE                                  
         JNE   BADSQMGO                                                         
*                                                                               
MKGTLR00 TM    RECFLAG2,MNWINPRG+ONWINPRG   MNW OR ONW IN PROGRESS?             
         BZ    MKGTLR05                                                         
         GOTOR RECWRT,DMCB,(RECCSEQ,AIO4),(CEXISTS,0)                           
         NI    RECFLAG2,X'FF'-MNWINPRG-ONWINPRG                                 
*                                                                               
         TM    RECFLAG2,DEMINPRG   MAKEGOOD SELLER DEMO IN PROGRESS             
         BZ    MKGTLR05                                                         
         OC    SVMODMEL,SVMODMEL   DID SELLER SEND MKGDS2?                      
         BNZ   MKGTLR03                                                         
         OC    SVNTDCEL,SVNTDCEL             OR    MKGCDC?                      
         BZ    MKGTLR05                                                         
MKGTLR03 GOTOR RECWRT,DMCB,('MOXKSDMQ',AIO5),(CODEXIST,0)                       
*                                                                               
MKGTLR05 L     R1,MKGCOUNT         INCREMENT RECORD COUNT                       
         LA    R1,1(R1)                                                         
         ST    R1,MKGCOUNT                                                      
*                                                                               
         LA    R4,R+1              SET INPUT RECORD DSECT                       
         USING MOFRTLRD,R4                                                      
*                                                                               
         CLC   ORDNO,MOTLORDR      ORDER #                                      
         JNE   INCNDATA                                                         
*                                                                               
         CLC   GROUPID,MOTLOFID    GROUP OFFER ID                               
         JNE   INCNDATA                                                         
*                                                                               
         PACK  DUB,MOTLSEQN        SEQUENCE NUMBER                              
         CVB   R1,DUB                                                           
         STC   R1,BYTE                                                          
         CLC   VER,BYTE                                                         
         JNE   INCNDATA                                                         
*                                                                               
         PACK  DUB,MOTLRCCT        RECORD COUNT                                 
         CVB   R1,DUB                                                           
         C     R1,MKGCOUNT         MAKE SURE ALL RECORDS WERE COUNTED           
         JNE   INVRECNT                                                         
*                                                                               
         LA    RE,MOTLTDOL                                                      
         ST    RE,DMCB                                                          
         MVI   DMCB,C'N'                                                        
         OI    DMCB,X'80'                                                       
         GOTOR VCASHVAL,DMCB,,10  TOTAL MAKEGOOD DOLLARS                        
         CLI   0(R1),0                     CHECK RETURN CODE                    
         JNE   INVNMFLD                                                         
         CLC   TMKGCOST,4(R1)      COMPARE WITH TOTAL COST                      
         JNE   INVTOTDL                                                         
*                                                                               
         GOTOR RECWRT,DMCB,(RECOSEQ,AIO1),(OEXISTS,0)                           
         CLI   OEXISTS,0           OFFER EXISTED BEFORE?                        
         BNE   *+8                 YES                                          
         OI    OEXISTS,EXONFILE    NO, NOW IT DOES                              
*                                                                               
         TM    RECFLAG,REJCOMNT    WAS THERE A REJECTION COMMENT                
         BNO   MKGTLR10                                                         
*                                                                               
         XC    ELTAREA,ELTAREA                                                  
         LA    R2,SAVEREJT                                                      
         LA    R3,SAVEREJX                                                      
*                                                                               
MKGTLR07 CR    R2,R3                  EOL OF REJ COMMENTS?                      
         JNL   MKGTLR10                YES                                      
         OC    0(L'SAVEREJT,R2),0(R2) ANY OF MORE REJ COMMENTS?                 
         JZ    MKGTLR10                NO                                       
*                                                                               
         MVC   ELTAREA,0(R2)                                                    
         GOTOR ADDEL,DMCB,(RECNSEQ,AIO2),(NEXISTS,0)                            
         LA    R2,L'SAVEREJT(0,R2)                                              
         J     MKGTLR07                                                         
*                                                                               
MKGTLR10 GOTOR RECWRT,DMCB,(RECNSEQ,AIO2),(NEXISTS,0)                           
         MVC   MNDSKADD,KEY+14     SAVE NOTICE DISK ADDRESS                     
         CLI   QSTATN,C'0'         CABLE?                                       
         BL    *+10                                                             
         MVC   MNDSKADD,KEY+36     SAVE NOTICE DISK ADDRESS                     
*                                                                               
* READ DARE ORDER RECORD TO ADD MAKEGOOD COLOR ELEMENT                          
         LA    R6,KEY                                                           
         USING DAREORDD,R6                                                      
         XC    KEY,KEY                                                          
         MVI   DOKTYPE,DOKTYPQ     TYPE                                         
         MVI   DOKSUBTY,DOKSTYPQ   SUBTYPE                                      
         MVC   DOKAGMD,AGYMD       A/M                                          
         MVC   DOKORDER,BINORDER   ORDER NUMBER                                 
*                                                                               
         MVI   DMINBTS,X'08'                                                    
         BRAS  RE,HIGH                                                          
*                                                                               
         CLC   KEYSAVE(DOKSTA-DOKEY),KEY                                        
         JNE   NODARORD            DARE ORDER RECORD DOESN'T EXIST              
         MVI   DMINBTS,X'88'                                                    
         L     R6,AIO1                                                          
         ST    R6,AIO                                                           
         BRAS  RE,GET                                                           
         MVC   DSKADDR,KEY+14      SAVE DISK ADDRESS                            
*                                                                               
         XC    ELEM,ELEM                                                        
         LA    R1,ELEM                                                          
         USING MGCOLELD,R1                                                      
         MVI   MGCOLEL,MGCOLELQ                                                 
         MVI   MGCOLLEN,MGCOLLNQ                                                
         MVC   MGCOLCOD,MOTLOFID   MAKEGOOD GROUP CODE                          
         MVI   MGCOLCOL,C'G'       COLOR STATUS OF THE MAKEGOOD                 
         DROP  R1                                                               
*                                                                               
         L     R6,AIO1                                                          
         LA    R6,DORFRST          FIRST ELEMENT                                
         SR    R0,R0                                                            
*                                                                               
MKGTLR20 CLI   0(R6),0             MAKEGOOD COLOR ELEMENT?                      
         BE    MKGTLR30                                                         
         CLI   0(R6),MGCOLELQ                                                   
         BL    MKGTLR25                                                         
         BNE   MKGTLR30                                                         
         USING MGCOLELD,R6                                                      
         CLC   MGCOLCOD,MOTLOFID                                                
         BL    MKGTLR25                                                         
         BNE   MKGTLR30                                                         
         MVI   MGCOLCOL,C'G'       COLOR STATUS OF THE MAKEGOOD                 
         B     MKGTLR40                                                         
         DROP  R4                                                               
*                                                                               
MKGTLR25 IC    R0,1(R6)                                                         
         AR    R6,R0                                                            
         B     MKGTLR20                                                         
*                                                                               
MKGTLR30 GOTOR VRECUP,DMCB,(C'S',AIO1),ELEM,(R6),0                              
*                                                                               
MKGTLR40 XC    ELEM,ELEM                                                        
         LA    R4,ELEM                                                          
         USING COLOREL,R4                                                       
         MVI   COLEL,COLELQ                                                     
         MVI   COLELLEN,COLLENQ                                                 
         MVI   COLCOL,C'G'         COLOR STATUS OF THE MAKEGOOD                 
         GOTOR VDATCON,DMCB,(5,0),(2,COLDATE)                                   
         XC    COLDATE,=X'FFFF'                                                 
         DROP  R4                                                               
*                                                                               
         L     R6,AIO1                                                          
         USING DOKEY,R6                                                         
         LA    R6,DORFRST                                                       
*                                                                               
         SR    R0,R0                                                            
MKGTLR50 IC    R0,1(R6)                                                         
         AR    R6,R0                                                            
         CLI   0(R6),0                                                          
         BE    MKGTLR55                                                         
*                                                                               
         CLI   0(R6),COLELQ        COLOR ELEMENT                                
*        BL    MKGTLR50                                                         
         BNE   MKGTLR50            DON'T ASSUME ELEMS IN ORDER                  
         USING COLOREL,R6                                                       
         MVI   COLCOL,C'G'         COLOR STATUS OF THE MAKEGOOD                 
         MVC   COLDATE,ELEM+COLDATE-COLOREL                                     
         B     MKGTLR60            WE CAN CHANGE THE ELEMENT IN RECORD          
*                                                                               
MKGTLR55 GOTOR VRECUP,DMCB,(C'S',AIO1),ELEM,(R6),0                              
         B     MKGTLR70            R6 --> COLOR ELEMENT                         
*                                                                               
         USING COLOREL,R6                                                       
MKGTLR60 XC    KEY,KEY             DELETE THE OLD COLOR PASSIVE                 
MKGTLR6D USING DOKEY,KEY                                                        
         MVI   MKGTLR6D.DSCKTYPE,DSCKTYPQ                                       
         MVI   MKGTLR6D.DSCKSTYP,DSCKSTYQ                                       
         MVC   MKGTLR6D.DSCKAGMD,AGYMD                                          
         MVC   MKGTLR6D.DSCKBYR,BUYER                                           
         MVC   MKGTLR6D.DSCKSTAT,COLCOL                                         
         MVC   MKGTLR6D.DSCKDATE,COLDATE                                        
         MVC   MKGTLR6D.DSCKORDR,BINORDER                                       
*                                                                               
         MVI   DMINBTS,X'08'       READ FOR DELETED                             
         BRAS  RE,HIGH                                                          
*                                                                               
         CLC   KEY(L'DOKEY),KEYSAVE    WE HAVE PASSIVE FOR THIS?                
         BNE   MKGTLR70                NO, NOTHING TO DELETE                    
         GOTOR VDATCON,DMCB,(5,0),(2,HALF)                                      
         XC    HALF,=X'FFFF'                                                    
         CLI   MKGTLR6D.DSCKSTAT,C'G'                                           
         BNE   MKGTLR65                                                         
         CLC   MKGTLR6D.DSCKDATE,HALF                                           
         BNE   MKGTLR65                                                         
* ACTUALLY SAME COLOR & DATE                                                    
MKGTLR63 TM    KEY+13,X'80'            DELETED?                                 
         BZ    MKGTLR75                                                         
         MVI   DMINBTS,X'88'           READ FOR UPDATE                          
         BRAS  RE,HIGH                                                          
         NI    KEY+13,X'FF'-X'80'      YES, UNDELETE AND WRITE                  
         BRAS  RE,WRITE                                                         
         B     MKGTLR75                                                         
*                                                                               
MKGTLR65 MVI   DMINBTS,X'88'           READ FOR UPDATE                          
         BRAS  RE,HIGH                                                          
         OI    KEY+13,X'80'                                                     
         BRAS  RE,WRITE                                                         
*                                                                               
         MVI   COLCOL,C'G'         COLOR STATUS OF THE MAKEGOOD                 
         MVC   COLDATE,HALF                                                     
*                                                                               
MKGTLR70 XC    KEY,KEY                                                          
         MVI   MKGTLR6D.DSCKTYPE,DSCKTYPQ     ADD THE NEW PASSIVE               
         MVI   MKGTLR6D.DSCKSTYP,DSCKSTYQ                                       
         MVC   MKGTLR6D.DSCKAGMD,AGYMD                                          
         MVC   MKGTLR6D.DSCKBYR,BUYER                                           
         MVC   MKGTLR6D.DSCKSTAT,COLCOL                                         
         MVC   MKGTLR6D.DSCKDATE,COLDATE                                        
         MVC   MKGTLR6D.DSCKORDR,BINORDER                                       
         MVC   KEY+14(4),DSKADDR                                                
*                                                                               
         MVI   DMINBTS,X'08'       SEE IF WE HAD ONE OF THESE BEFORE            
         BRAS  RE,HIGH                                                          
         CLC   KEY(L'DOKEY),KEYSAVE    WE HAVE PASSIVE FOR THIS?                
         BNE   MKGTLR73                NO, WE CAN SAFELY ADD IT NOW             
         MVC   KEY+14(4),DSKADDR                                                
         B     MKGTLR63                YES, UNDELETE IF NECESSARY               
*                                                                               
MKGTLR73 MVC   KEY,KEYSAVE                                                      
         BRAS  RE,ADDKEY                                                        
*                                                                               
MKGTLR75 BRAS  RE,CHKFRCHG         ADD CHECK FOR CHANGE KEY                     
*                                                                               
         TM    BITFLAG1,BF1REPCH   DID THE REP CHANGE?                          
         BZ    MKGTLR90            NO, NOTHING LEFT TO DO                       
*                                                                               
         L     R6,AIO1                                                          
         USING DOKEY,R6                                                         
         LA    R6,DORFRST          FIRST ELEMENT                                
         USING DOIDELD,R6                                                       
*                                                                               
         XC    ELEM,ELEM                                                        
         LA    R4,ELEM                                                          
         USING DOREPELD,R4                                                      
         MVI   DORPEL,DORPELQ                                                   
         MVI   DORPLEN,DORPLENQ                                                 
         MVC   DORPNREP,MKGREPID   NEW REP ID NUMBER                            
         MVC   DORPPRCN,DOIDCON    SAVE OLD REP CONTRACT #                      
*                                    AND OVERWRITE OLD ONE WITH NEW ONE         
         MVC   DOIDCON,SAVEHDR+MOHDRPCN-MOFRHDRD                                
         GOTOR VDATCON,DMCB,(0,SAVEHDR+MOHDDATE-MOFRHDRD),(19,DORPDATE)         
         PACK  FULL,SAVEHDR+MOHDTIME-MOFRHDRD(L'MOHDTIME)   (000HHMMC)          
         SRP   FULL,1,0            SHIFT LEFT 1 DIGIT       (00HHMM0C)          
         MVC   DORPTIME,FULL+1                                (HHMM)            
         DROP  R4                                                               
*                                                                               
         SR    R0,R0                                                            
MKGTLR83 IC    R0,1(R6)                                                         
         AR    R6,R0                                                            
         CLI   0(R6),0                                                          
         JE    *+2                                                              
*                                                                               
         CLI   0(R6),DORPELQ       ADD REP CHANGED ELEMENT                      
         BL    MKGTLR83                                                         
         GOTOR VRECUP,DMCB,(C'S',AIO1),ELEM,(R6),0                              
*                                                                               
* ADD THE NEW REP ELEMENT DORPELQ2 X'09'                                        
         L     R6,AIO1                                                          
         USING DOKEY,R6                                                         
         LA    R6,DORFRST          FIRST ELEMENT                                
         USING DOIDELD,R6                                                       
*                                                                               
         LA    R4,ELEM                                                          
         USING DOREPELD,R4                                                      
         MVI   DORPEL,DORPELQ2     X'09'                                        
         DROP  R4                                                               
*                                                                               
         SR    R0,R0                                                            
MKGTLR85 IC    R0,1(R6)                                                         
         AR    R6,R0                                                            
         CLI   0(R6),0                                                          
         JE    *+2                                                              
*                                                                               
         CLI   0(R6),DORPELQ2      ADD REP CHANGED ELEMENT                      
         BE    MKGTLR86                                                         
         CLI   0(R6),DOSTELQ       ADD REP CHANGED ELEMENT                      
         BL    MKGTLR85                                                         
MKGTLR86 GOTOR VRECUP,DMCB,(C'S',AIO1),ELEM,(R6),0                              
*                                                                               
MKGTLR90 BRAS  RE,PUT                                                           
*                                                                               
MKGTLRX  B     YES                                                              
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* ADDEL: ADDS ELEMENT IN WORKSPACE (ELTAREA) TO RECORD                          
*        CHECKS RECORD SIZE TO SEE IF CURRENT RECORD MUST BE                    
*        WRITTEN, AND ANOTHER SET UP                                            
*        DMCB+1(3)  - ADDRESS OF RECORD                                         
*        DMCB(1)    - RECORD SEQUENCE #                                         
*        DMCB+4(1)  - EXISTS FLAG                                               
*                                                                               
* ON EXIT:     (CC)                IMPORTANT FOR NOTICE RECORDS                 
***********************************************************************         
ADDEL    NTR1  BASE=*,LABEL=*                                                   
         SR    R2,R2                                                            
         ICM   R2,7,1(R1)                                                       
         MVC   RECSEQ,0(R1)                                                     
         MVC   EXISTS,4(R1)                                                     
*                                                                               
         CLI   QSTATN,C'0'         CABLE?                                       
         BNL   ADDXEL              YES, GOTO TO ADDXEL INSTEAD                  
*                                                                               
         USING DAREMGOD,R2                                                      
         NI    RECFLAG,X'FF'-NEWRECRD                                           
         ZICM  RF,MORLEN,2         GET RECORD LENGTH                            
         ZIC   RE,ELTAREA+1        GET ELEMENT LENGTH                           
         AR    RF,RE               CALCULATE NEW LENGTH                         
         C     RF,=F'3970'         DOES RECORD EXCEED MAXIMUM?                  
         BL    ADDEL10             NO  - ADD ELEMENT                            
*                                                                               
         CLI   1(R2),MNKSTYPQ      NOTICE RECORD?                               
         JE    TOOMNYMS            TOO MANY MISSED SPOTS OR MISSED CMTS         
*                                                                               
         GOTOR RECWRT,DMCB,(RECSEQ,(R2)),(EXISTS,0)                             
         OI    RECFLAG,NEWRECRD                                                 
*                                                                               
         ZIC   RF,MOKSEQ           BUMP THE RECORD SEQ NUMBER                   
         LA    RF,1(RF)                                                         
         STC   RF,MOKSEQ           PUT IT BACK                                  
*                                                                               
         LA    RE,MORFRST-MOKEY+MOSQLNQ      NEW LENGTH OF RECORD               
         STCM  RE,3,MORLEN                                                      
         LA    RF,2000             CLEAR AFTER FIRST ELEMENT                    
         SR    RF,RE                                                            
         AR    RE,R2                                                            
         XCEF                                                                   
*&&DO                                                                           
ADDEL10  LA    RF,=C'ADD=END'      DEFAULT IS TO ADD TO THE END OF REC          
         CLI   ELTAREA,MNMSELQ     ADDING MISSED BUYLINE ELEM?                  
         BNE   *+6                 DON'T WORRY ABOUT THE SORT ORDER             
         XR    RF,RF               NEED TO SORT MISSED BUYLINE ELEMS            
*                                                                               
         ST    RF,DMCB+12                                                       
         GOTOR VHELLO,DMCB,(C'P',SPTFILE),(R2),ELTAREA,,0                       
         CLI   PAR4,0                                                           
         JNE   *+2                                                              
*&&                                                                             
ADDEL10  LLH   R6,MORLEN                                                        
         LA    R6,0(R6,R2)                                                      
         CLI   1(R2),MOKSTYPQ      OFFER RECORD?                                
         BNE   ADDEL20                                                          
         CLI   ELTAREA,MOMBELQ     LESS THAN X'20', ADD TO END                  
         JNH   ADDEL20                                                          
         LA    R6,MORFRST-MOKEY(R2)                                             
         BRAS  RE,STMKOINS         SET MKO INSERTION ADDRESS                    
                                                                                
ADDEL20  GOTOR VRECUP,DMCB,(C'S',(R2)),ELTAREA,(R6),0                           
*                                                                               
         TM    RECFLAG,NEWRECRD    NEW SEQUENCE OF OFFER RECORD ADDED           
         BNO   ADDELX                                                           
*                                                                               
         NI    RECFLAG,X'FF'-NEWRECRD                                           
         XC    ELTAREA,ELTAREA                                                  
         LA    R3,ELTAREA                                                       
         USING MOSQELD,R3                                                       
         MVI   MOSQEL,MOSQELQ      ELEMENT CODE                                 
         MVI   MOSQLEN,MOSQLNQ     ELEMENT LENGTH                               
         MVC   MOSQNUM,VER                                                      
*                                                                               
         B     ADDEL10                                                          
*                                                                               
ADDELX   J     YES                                                              
         DROP  R2,R3                                                            
         EJECT                                                                  
***********************************************************************         
* ADDXEL: ADDS ELEMENT IN WORKSPACE (ELTAREA) TO XSPOT RECORD                   
*         CHECKS RECORD SIZE TO SEE IF CURRENT RECORD MUST BE                   
*         WRITTEN, AND ANOTHER SET UP                                           
*         R2 HAS ADDRESS OF RECORD                                              
*                                                                               
* ON EXIT:     (CC)                IMPORTANT FOR NOTICE RECORDS                 
***********************************************************************         
ADDXEL   DS    0H                                                               
         USING DAREMGOD,R2                                                      
*                                                                               
         NI    RECFLAG,X'FF'-NEWRECRD                                           
         ZICM  RF,MOXRLEN,2        GET RECORD LENGTH                            
         ZIC   RE,ELTAREA+1        GET ELEMENT LENGTH                           
         AR    RF,RE               CALCULATE NEW LENGTH                         
         C     RF,=F'5970'         DOES RECORD EXCEED MAXIMUM?                  
         BL    ADDXEL10            NO  - ADD ELEMENT                            
*                                                                               
**NOP**  CLI   1(R2),MNKSTYPQ      NOTICE RECORD?                               
**NOP**  JE    TOOMNYMS            TOO MANY MISSED SPOTS OR MISSED CMTS         
         J     TOOMNYMS            NEITHER NOT NOR OFF SUPPORT SPANNING         
*                                                                               
         GOTOR RECWRT,DMCB,(RECSEQ,(R2)),(EXISTS,0)                             
         OI    RECFLAG,NEWRECRD                                                 
*                                                                               
         ZIC   RF,MOXKSEQ           BUMP THE RECORD SEQ NUMBER                  
         LA    RF,1(RF)                                                         
         STC   RF,MOXKSEQ           PUT IT BACK                                 
*                                                                               
         LA    RE,MOXFRST-MOXKEY+MOSQLNQ  NEW LENGTH OF OFFER RECORD            
         CLI   1(R2),MNXKSBTQ              NOTICE RECORD?                       
         BNE   *+8                                                              
         LA    RE,MNXFRST-MNXKEY          NEW LENGTH OF NOTICE RECORD           
         STCM  RE,3,MOXRLEN                                                     
         LA    RF,2000             CLEAR EVERYTHING AFTER                       
         SR    RF,RE                                                            
         AR    RE,R2                                                            
         XCEF                                                                   
*                                                                               
ADDXEL10 DS    0H                                                               
         LLH   R6,MOXRLEN                                                       
         LA    R6,0(R6,R2)                                                      
         CLI   MOXKSBTY,MOXKSBTQ   OFFER RECORD?                                
         BNE   ADDXEL20                                                         
         CLI   MOXKDTYP,MOXKSDMQ   SELLER DEMO RECORD?                          
         BE    ADDXEL20                                                         
         CLI   ELTAREA,MOMBELQ     LESS THAN X'20', ADD TO END                  
         JNH   ADDXEL20                                                         
         LA    R6,MOXFRST-MOXKEY(R2)                                            
         BRAS  RE,STMKOINS         SET MKO INSERTION ADDRESS                    
*                                                                               
* WE CAN'T USE THE C'T' IN THE 1ST PARAM BECAUSE RECUP WAS NOT CHANGED          
*  TO USE THE MAX RECORD SIZE THAT DMFILTAB HAS FOR XSPFIL                      
******** GOTOR VRECUP,DMCB,(C'T',(R2)),ELTAREA,(R6),0                           
ADDXEL20 GOTOR VRECUP,DMCB,(X'FE',(R2)),ELTAREA,(R6),XSPREC                     
*                                                                               
         TM    RECFLAG,NEWRECRD    NEW SEQUENCE OF OFF/NOT REC ADDED?           
         BNO   ADDXELX                                                          
         NI    RECFLAG,X'FF'-NEWRECRD                                           
*                                                                               
         CLI   1(R2),MNXKSBTQ      NOTICE RECORD?                               
         BE    ADDXELX             YES, CAN SKIP THIS                           
*                                                                               
         XC    ELTAREA,ELTAREA                                                  
         LA    R3,ELTAREA                                                       
         USING MOSQELD,R3                                                       
         MVI   MOSQEL,MOSQELQ      ELEMENT CODE                                 
         MVI   MOSQLEN,MOSQLNQ     ELEMENT LENGTH                               
         MVC   MOSQNUM,VER                                                      
*                                                                               
         B     ADDXEL10                                                         
ADDXELX  J     YES                                                              
         DROP  R2,R3                                                            
***********************************************************************         
* RETURNS THE INSERTION ADDRESS IN R6                                           
***********************************************************************         
STMKOINS DS    0H                                                               
SMO010   CLI   0(R6),0             END OF REC?                                  
         JE    SMOX                                                             
         CLI   0(R6),MOMBELQ       FIND X'20'?                                  
         BE    SMO030                                                           
SMO020   LLC   R0,1(R6)                                                         
         AR    R6,R0                                                            
         J     SMO010                                                           
SMO030   CLC   2(2,R6),ELTAREA+2   MATCH ON OFFNUM/RECNUM?                      
         JNE   SMO020                NO, LOOK FOR NEXT X'20'                    
*                                                                               
SMO040   LLC   R0,1(R6)            FOUND MATCH, FIND INSERTION ADDRESS          
         AR    R6,R0                                                            
         CLI   0(R6),X'20'         END OF REC OR NEXT X'20'?                    
         JNH   SMOX                 YES, FOUND INSERTION!                       
         CLC   0(1,R6),ELTAREA     R6 ELEM CODE > ELTAREA CODE?                 
         JNH   SMO040               NO, BUMP TO NEXT                            
SMOX     BR    RE                  FOUND INSERTION ADDRESS                      
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*   VALTIME: VALIDATE TIMES FOR 0600 - 3000 (MILITARY TIME)                     
*            R5: ADDRESS OF EBCDIC INPUT TIME                                   
*            R6: ADDRESS TO PLACE CONVERTED BINARY TIME                         
***********************************************************************         
VALTIME  NTR1  BASE=*,LABEL=*                                                   
         ST    R5,DMCB                                                          
         MVI   DMCB,C'N'                                                        
         OI    DMCB,X'80'                                                       
         GOTOR VCASHVAL,DMCB,,4                                                 
         CLI   0(R1),0                                                          
         JNE   INVNMFLD                                                         
         CLC   4(4,R1),=F'600'     TIME IS 0600-3000 ONLY                       
         JL    INVNMFLD                                                         
         CLC   4(4,R1),=F'3000'                                                 
         JH    INVNMFLD                                                         
         MVC   0(2,R6),6(R1)       TIME IS ONLY LAST 2 BYTES OF PARAM 2         
         J     YES                                                              
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* SETS SPOT SE # BY LOOKING IN CONTROL FILE.                                    
***********************************************************************         
CTRLSET  NTR1  BASE=*,LABEL=*                                                   
         L     R2,0(R1)            SET A(TABLE INPUT)                           
         MVI   DMCB,X'0A'          SWITCH TO THE CONTROL SYSTEM                 
         BRAS  RE,SWTCHSYS                                                      
         JE    CTRL0110                                                         
         MVI   RETCODE,1                                                        
         J     CTRLNO              IF NOT SUCCESSFUL, TRY AGAIN LATER           
***      BNE   EXITPRG             IF NOT SUCCESSFUL, TRY AGAIN LATER           
*****                                                                           
*******  GOTOR VSWITCH,DMCB,(X'0A',X'FFFFFFFF'),0                               
*****                                                                           
CTRL0110 XC    KEY,KEY                                                          
         MVI   KEY,C'I'            FIND CONTROL FILE ID RECORD                  
         MVC   KEY+23(2),0(R2)     INSERT ID NUMBER FROM TABLE!!                
*                                                                               
         GOTOR VDATAMGR,DMCB,DMRDHI,CTFILE,KEY,AIO1                             
*                                                                               
         L     R1,AIO1                                                          
         CLC   KEY(25),0(R1)       CHECK THE KEY                                
         JNE   *+2                 SAME - OKAY                                  
*                                                                               
         LA    R1,28(R1)           FIND SYS AUTHORIZATION ELEMENT               
CTRL0120 CLI   0(R1),X'21'         AUTH ELEMENT?                                
         BNE   *+12                NO                                           
         CLI   2(R1),X'02'         IS IT SPOT SYSTEM?                           
         BE    CTRL0200            YES                                          
*                                                                               
         ZIC   R0,1(R1)            BUMP TO NEXT ELEMENT                         
         AR    R1,R0                                                            
         CLI   0(R1),0             END OF RECORD?                               
         BNE   CTRL0120            NO                                           
         DC    H'0'                NO X'21' - DUMP IT OUT                       
*                                                                               
CTRL0200 ST    R1,FULL             SAVE A(X'21' ELEMENT)                        
         L     R4,FULL             RESET A(X'21' ELEMENT)                       
*                                     WITH SPOT UTL CODE                        
         GOTOR VSWITCH,DMCB,(3(R4),X'FFFFFFFF'),0                               
         CLI   4(R1),0             SWITCHED OKAY?                               
         BE    CTRL0280            YES - NOW FIND POWER CODE                    
         CLI   4(R1),2             SYSTEM NOT OPENED?                           
         BE    *+6                                                              
         DC    H'0'                OTHERWISE DEATH                              
         MVI   RETCODE,1                                                        
         B     CTRLNO              RETURN WITH A 'NE'                           
*                                                                               
CTRL0280 MVC   SPTSENUM,3(R4)                                                   
         L     R1,VSELIST                                                       
         ZICM  RE,0(R1),2          SET UP BXLE USING (RE,RF)                    
         ICM   RF,15,2(R1)                                                      
         LA    R1,6(R1)                                                         
         USING SELISTD,R1                                                       
*                                                                               
CTRL0282 CLC   SESYS,3(R4)                                                      
         BNE   CTRL0284                                                         
         TM    SEIND,SEISETRO+SEIRONLY   SET TO READ-ONLY STATUS?               
         BZ    CTRL0285                                                         
         MVI   RETCODE,1                                                        
         B     CTRLNO                                                           
*                                                                               
CTRL0284 BXLE  R1,RE,CTRL0282      CHECK NEXT ENTRY IN SELIST                   
*                                                                               
CTRL0285 L     R1,AIO1             RESET A(CONTROL RECORD)                      
         LA    R1,28(R1)           FIND X'06' AGENCY ID ELEMENT                 
*                                                                               
CTRL0320 CLI   0(R1),X'06'         AGENCY ID ELEMENT?                           
         BE    CTRL0360            YES                                          
         ZIC   RF,1(R1)            NO  - BUMP TO NEXT ELEMENT                   
         AR    R1,RF                                                            
*                                                                               
         CLI   0(R1),0             END OF RECORD?                               
         BNE   CTRL0320            NO                                           
         DC    H'0'                YES - NOT FOUND????!!!                       
*                                                                               
CTRL0360 MVC   POWERCDE,2(R1)      SAVE POWER CODE                              
*                                                                               
CTRLYES  SR    RC,RC                                                            
CTRLNO   LTR   RC,RC                                                            
         J     EXIT                                                             
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* SWITCHES SYSTEM WHETHER THE PROGRAM HAS BEEN AUTHORIZED TO SWITCH             
* SYSTEMS OR NOT                                                                
*                                                                               
* ON ENTRY:    DMCB    BYTE  0     SYSTEM SENUM TO SWITCH TO                    
***********************************************************************         
SWTCHSYS NTR1  BASE=*,LABEL=*                                                   
         MVC   DMCB+1(3),=3X'FF'   DON'T CARE IF PROGRAM IS AUTHORIZED          
         GOTOR VSWITCH,DMCB,,0                                                  
         CLI   DMCB+4,0            SUCCESSFUL?                                  
         BNE   SWSYSNO             NO                                           
*                                                                               
SWSYSYES J     YES                                                              
*                                                                               
SWSYSNO  J     NO                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* RETRIEVES THE PRINT QUEUE ENTRY                                               
***********************************************************************         
RETRVREP NTR1  BASE=*,LABEL=*                                                   
         L     R2,0(R1)            SET A(TABLE ENTRY)                           
         USING DREPTABL,R2                                                      
*                                                                               
         XC    PQINDEX,PQINDEX                                                  
         LA    R3,PQINDEX          SET INDEX FOR REPORT                         
         USING UKRECD,R3                                                        
         MVC   UKSRCID,DRUSERID    USER ID CODE                                 
         MVC   UKSUBID,DRSUBID     SUBID                                        
         MVC   UKREPNO,DRREPT#     REPORT NUMBER                                
*                                                                               
         L     RE,APQREC                                                        
         ST    RE,DMCB+16                                                       
         GOTOR VDATAMGR,DMCB,(0,=C'GFILE'),=C'PRTQUE',PQINDEX,R                 
         CLI   8(R1),0                                                          
         JNE   *+2                 BAD GFILE CALL                               
*                                                                               
         L     RE,APQREC                                                        
         ST    RE,DMCB+16                                                       
         GOTOR VDATAMGR,DMCB,(0,=C'INDEX'),UKUSRINF,PQINDEX,R                   
         CLI   8(R1),0                                                          
         J     EXIT                                                             
         DROP  R2,R3                                                            
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*   RECWRT:    CHECKS FOR KEYS, DELETED RECORDS, ETC....                        
*              R2 HAS ADDRESS OF RECORD                                         
***********************************************************************         
RECWRT   NTR1  BASE=*,LABEL=*                                                   
         SR    R2,R2                                                            
         ICM   R2,7,1(R1)                                                       
         MVC   RECSEQ,0(R1)                                                     
         MVC   EXISTS,4(R1)                                                     
*                                                                               
         CLI   QSTATN,C'0'         CABLE?                                       
         BNL   RCXWRT00            YES, GOTO TO RCXWRT00 INSTEAD                
*                                                                               
         LA    R3,KEY                                                           
         USING DAREMGOD,R3                                                      
         XC    KEY,KEY                                                          
         XC    KEYSAVE,KEYSAVE                                                  
         MVC   KEY(L'MOKEY),0(R2)       SET KEY FROM NEW RECORD                 
         MVC   KEYSAVE(L'MOKEY),0(R2)   SET KEY FROM NEW RECORD                 
*                                                                               
         TM    EXISTS,EXONFILE     DO WE ADD THE RECORD?                        
         BZ    RECWRT10            YES, IT DOESN'T LIVE ON THE FILE             
*                                                                               
         CLI   KEY+1,MNKSTYPQ      NOTICE RECORD                                
         BE    RECWRT20                                                         
         OI    RECFLAG,RECWRITN                                                 
         CLC   MOKSEQ,RECSEQ       CHECK IF SEQUENCE # EXISTS                   
         BNH   RECWRT20                                                         
         DROP  R3                                                               
*                                                                               
RECWRT10 ST    R2,AIO                                                           
         BRAS  RE,ADD                                                           
         B     RECWRTX                                                          
*                                                                               
RECWRT20 MVI   DMINBTS,X'88'                                                    
         BRAS  RE,HIGH                                                          
*                                                                               
         CLC   KEYSAVE(L'MOKEY),KEY                                             
         JNE   *+2                                                              
*                                                                               
         MVI   DMINBTS,X'88'                                                    
         MVC   AIO,AIO3            READ RECORD INTO AIO3                        
         BRAS  RE,GET                                                           
*                                  BUT SAVE WHAT IS POINTED BY R2               
         ST    R2,AIO                                                           
         BRAS  RE,PUT                                                           
*                                                                               
         NI    KEY+13,X'FF'-X'80'                                               
         MVC   KEYSAVE,KEY                                                      
         BRAS  RE,WRITE                                                         
*                                                                               
RECWRTX  J     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
*   RECXWRT:   CHECKS FOR KEYS, DELETED RECORDS, ETC....                        
*              R2 HAS ADDRESS OF RECORD                                         
***********************************************************************         
RECXWRT  NTR1                                                                   
RCXWRT00 DS    0H                                                               
         LA    R3,KEY                                                           
         USING DAREMGOD,R3                                                      
         XC    KEY,KEY                                                          
         XC    KEYSAVE,KEYSAVE                                                  
         MVC   KEY(L'MOXKEY),0(R2)      SET KEY FROM NEW RECORD                 
         MVC   KEYSAVE(L'MOXKEY),0(R2)  SET KEY FROM NEW RECORD                 
*                                                                               
         TM    EXISTS,EXONFILE     DO WE ADD THE RECORD?                        
         BZ    RCXWRT10            YES, IT DOESN'T LIVE ON THE FILE             
         OI    RECFLAG,RECWRITN                                                 
*                                                                               
         CLC   MOXKSEQ,RECSEQ       CHECK IF SEQUENCE # EXISTS                  
         BNH   RCXWRT20                                                         
         DROP  R3                                                               
*                                                                               
RCXWRT10 ST    R2,AIO                                                           
         BRAS  RE,XADD                                                          
         B     RECXWRTX                                                         
*                                                                               
RCXWRT20 MVI   DMINBTS,X'88'                                                    
         BRAS  RE,XHIGH                                                         
*                                                                               
         CLC   KEYSAVE(L'MOXKEY),KEY                                            
         JNE   *+2                                                              
*                                                                               
         MVI   DMINBTS,X'88'                                                    
         MVC   AIO,AIO3            READ RECORD INTO AIO3                        
         BRAS  RE,XGET                                                          
*                                  BUT SAVE WHAT IS POINTED BY R2               
         ST    R2,AIO                                                           
         BRAS  RE,XPUT                                                          
*                                                                               
         NI    KEY+32,X'FF'-X'80'                                               
         MVC   KEYSAVE,KEY                                                      
         BRAS  RE,XWRITE                                                        
*                                                                               
RECXWRTX J     EXIT                                                             
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* ADD XPOT KEY FOR CHECK FOR CHANGE SUPPORT                                     
* ON ENTRY : IO1       DARE ORDER RECORD                                        
*            AGYMD     AGENCY/MEDIA                                             
*            BUYER     BUYER CODE                                               
*            BINORDER  BINARY ORDER#                                            
*            GROUPID   GROUP CODE                                               
*            DSKADDR   ORDER RECORD D/A                                         
*            MNDSKADD  NOTICE RECORD D/A                                        
*                                                                               
*  ON EXIT : XSPOT KEYS ADDED                                                   
***********************************************************************         
CHKFRCHG NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         XC    KEY,KEY                                                          
CFCD     USING MNKEY,KEY                                                        
         MVI   CFCD.MNDKTYPE,MNDKTYPQ       X'0D'                               
         MVI   CFCD.MNDKSTYP,MNDKSTYQ       X'BC'                               
         MVC   CFCD.MNDKAGMD,AGYMD          AGENCY MEDIA                        
         MVC   CFCD.MNDKBYR,BUYER           BUYER                               
*                                                                               
         L     R6,AIO1                                                          
         USING DOKEY,R6                                                         
         LA    R6,DORFRST          FIRST ELEMENT                                
         USING DOIDELD,R6                                                       
         MVC   CFCD.MNDKCLT,DOIDCLT         CLIENT                              
         MVC   CFCD.MNDKPRD,DOIDPRD         PRODUCT                             
         MVC   CFCD.MNDKEST,DOIDEST         ESTIMATE                            
*                                                                               
         MVI   ELCODE,DOSPELQ                                                   
         MVI   DATADISP+1,24                                                    
         BRAS  RE,NEXTEL                                                        
         USING DOSPELD,R6                                                       
         MVC   CFCD.MNDKMKT,DOSPMKT         MARKET                              
         DROP  R6                                                               
*                                                                               
         MVC   CFCD.MNDKORDR,BINORDER       ORDER NUMBER                        
         GOTOR VDATCON,DMCB,(5,0),(2,CFCD.MNDKDATE)                             
         THMS  DDSTIME=YES                                                      
         STCM  R0,15,PACKOF4B                                                   
         ST    R1,FULL                                                          
         AP    PACKOF4B,FULL                                                    
*                                                                               
         CP    PACKOF4B,=P'240000'    PAST MIDNIGHT?                            
         BL    CFC010                                                           
         SP    PACKOF4B,=P'240000'    YES, BUMP TO NEXT DAY AND ADJUST          
         GOTOR VDATCON,DMCB,(5,0),(0,DUB)   THE TIME                            
         GOTOR VADDAY,DMCB,DUB,DUB,F'1'                                         
         GOTOR VDATCON,DMCB,(0,DUB),(2,CFCD.MNDKDATE)                           
*                                                                               
CFC010   TIME  TU                  GET THE TIME IN 38400TH OF A SEC             
         STCM  R0,15,CFCD.MNDKTIME                                              
         XC    CFCD.MNDKTIME,=X'FFFFFFFF'                                       
*                                                                               
         MVC   KEY+36(4),DSKADDR   DISK ADDRESS                                 
         BRAS  RE,ADDXKEY                                                       
*                                                                               
         MVC   CFCD.MNDKGPCD,GROUPID        GROUP CODE                          
         MVC   KEY+36(4),MNDSKADD  DISK ADDRESS                                 
         BRAS  RE,ADDXKEY                                                       
         J     EXIT                                                             
         LTORG                                                                  
         EJECT                                                                  
*========================================================                       
* INDEX SEQUENTIAL FILE COMMANDS                                                
*========================================================                       
         SPACE 1                                                                
HIGH     MVC   KEYSAVE,KEY                                                      
         BRAS  R1,GODIR                                                         
         DC    AL4(DMRDHI)                                                      
         DC    AL4(SPTDIR)                                                      
*                                                                               
SEQ      BRAS  R1,GODIR                                                         
         DC    AL4(DMRSEQ)                                                      
         DC    AL4(SPTDIR)                                                      
*                                                                               
WRITE    BRAS  R1,GODIR                                                         
         DC    AL4(DMWRITE)                                                     
         DC    AL4(SPTDIR)                                                      
*                                                                               
ADDKEY   BRAS  R1,GODIR                                                         
         DC    AL4(DMADD)                                                       
         DC    AL4(SPTDIR)                                                      
*                                                                               
XHIGH    MVC   KEYSAVE,KEY                                                      
         BRAS  R1,GODIR                                                         
         DC    AL4(DMRDHI)                                                      
         DC    AL4(XSPDIR)                                                      
*                                                                               
XSEQ     BRAS  R1,GODIR                                                         
         DC    AL4(DMRSEQ)                                                      
         DC    AL4(XSPDIR)                                                      
*                                                                               
XWRITE   BRAS  R1,GODIR                                                         
         DC    AL4(DMWRITE)                                                     
         DC    AL4(XSPDIR)                                                      
*                                                                               
ADDXKEY  BRAS  R1,GODIR                                                         
         DC    AL4(DMADD)                                                       
         DC    AL4(XSPDIR)                                                      
*                                                                               
GODIR    NTR1  BASE=*,LABEL=*                                                   
         ICM   RE,15,0(R1)                                                      
         A     RE,RELO                                                          
         ST    RE,DMCB                                                          
         MVC   DMCB(1),DMINBTS                                                  
*                                                                               
         ICM   RE,15,4(R1)                                                      
         A     RE,RELO                                                          
         ST    RE,DMCB+4                                                        
*                                                                               
         LA    RE,KEYSAVE                                                       
         ST    RE,DMCB+8                                                        
*                                                                               
         LA    RE,KEY                                                           
         ST    RE,DMCB+12                                                       
         GOTOR VDATAMGR,DMCB                                                    
         TM    8(R1),X'FD'                                                      
         JNZ   *+2                                                              
         MVI   DMINBTS,0           RESET                                        
         XIT1                                                                   
         LTORG                                                                  
         SPACE 1                                                                
*========================================================                       
* FILE COMMANDS                                                                 
*========================================================                       
         SPACE 1                                                                
GET      BRAS  R1,GOFILE                                                        
         DC    AL4(GETREC)                                                      
         DC    AL4(SPTFILE)                                                     
         DC    H'14'               DSPL TO DISK ADDRESS                         
*                                                                               
PUT      BRAS  R1,GOFILE                                                        
         DC    AL4(PUTREC)                                                      
         DC    AL4(SPTFILE)                                                     
         DC    H'14'                                                            
*                                                                               
ADD      BRAS  R1,GOFILE                                                        
         DC    AL4(ADDREC)                                                      
         DC    AL4(SPTFILE)                                                     
         DC    H'14'                                                            
*                                                                               
XGET     BRAS  R1,GOFILE                                                        
         DC    AL4(GETREC)                                                      
         DC    AL4(XSPFIL)                                                      
         DC    H'36'               DSPL TO DISK ADDRESS                         
*                                                                               
XPUT     BRAS  R1,GOFILE                                                        
         DC    AL4(PUTREC)                                                      
         DC    AL4(XSPFIL)                                                      
         DC    H'36'                                                            
*                                                                               
XADD     BRAS  R1,GOFILE                                                        
         DC    AL4(ADDREC)                                                      
         DC    AL4(XSPFIL)                                                      
         DC    H'36'                                                            
*                                                                               
GOFILE   NTR1  BASE=*,LABEL=*                                                   
         ICM   RE,15,0(R1)                                                      
         A     RE,RELO                                                          
         ST    RE,DMCB             SET COMMAND ADDRESS                          
         MVC   DMCB(1),DMINBTS                                                  
*                                                                               
         ICM   RE,15,4(R1)                                                      
         A     RE,RELO                                                          
         ST    RE,DMCB+4           SET FILENAME ADDRESS                         
*                                                                               
         LA    RE,KEY                                                           
         AH    RE,8(R1)            GET DSPL OF DISK ADDRESS IN KEY              
         ST    RE,DMCB+8                                                        
*                                                                               
         MVC   DMCB+12(4),AIO                                                   
*                                                                               
         LA    RE,IOWORK                                                        
         ST    RE,DMCB+16                                                       
*                                                                               
         GOTOR VDATAMGR,DMCB                                                    
         TM    8(R1),X'FD'         TEST ALL BUT DELETED                         
         JNZ   *+2                                                              
         MVI   DMINBTS,0           RESET                                        
         XIT1                                                                   
         LTORG                                                                  
*                                                                               
*   RECTABLE:  TABLE OF RECORD IDENTIFIERS FROM PRINT QUEUE RECORDS.            
*        WITH ASSOCIATED FORMATING ROUTINES, AND BREAK INDICATOR                
*        FLAGS.                                                                 
*                                                                               
         DS    0F                  ALIGNMENT                                    
RECTABLE EQU   *                                                                
         DC    CL6'MKGHDR',XL1'01,00',AL4(MKGHDR)                               
LRECTAB  EQU   *-RECTABLE          SIZE OF ENTRY                                
HDR      EQU   (*-RECTABLE)/LRECTAB                                             
         DC    CL6'MKGDS1',XL1'01,00',AL4(MKGDS1)                               
DS1      EQU   (*-RECTABLE)/LRECTAB                                             
         DC    CL6'MKGTST',XL1'01,00',AL4(MKGTST)                               
TST      EQU   (*-RECTABLE)/LRECTAB                                             
         DC    CL6'MKGDS2',XL1'01,00',AL4(MKGDS2)                               
DS2      EQU   (*-RECTABLE)/LRECTAB                                             
         DC    CL6'MKGCDC',XL1'01,00',AL4(MKGCDC)                               
CDC      EQU   (*-RECTABLE)/LRECTAB                                             
         DC    CL6'MKGMNW',XL1'01,00',AL4(MKGMNW)                               
MNW      EQU   (*-RECTABLE)/LRECTAB                                             
         DC    CL6'MKGMSS',XL1'01,00',AL4(MKGMSS)                               
MSS      EQU   (*-RECTABLE)/LRECTAB                                             
         DC    CL6'MKGAAU',XL1'01,00',AL4(MKGAAU)                               
MAAU     EQU   (*-RECTABLE)/LRECTAB                                             
         DC    CL6'MKGONW',XL1'01,00',AL4(MKGONW)                               
ONW      EQU   (*-RECTABLE)/LRECTAB                                             
         DC    CL6'MKGBUY',XL1'01,00',AL4(MKGBUY)                               
BUY      EQU   (*-RECTABLE)/LRECTAB                                             
         DC    CL6'MKGAAU',XL1'01,00',AL4(MKGAAU)                               
OAAU     EQU   (*-RECTABLE)/LRECTAB                                             
         DC    CL6'MKGDEM',XL1'01,00',AL4(MKGDEM)                               
DEM      EQU   (*-RECTABLE)/LRECTAB                                             
         DC    CL6'MKGCOM',XL1'01,00',AL4(MKGCOM)                               
CDV      EQU   (*-RECTABLE)/LRECTAB                                             
         DC    CL6'MKGCDV',XL1'01,00',AL4(MKGCDV)                               
COM      EQU   (*-RECTABLE)/LRECTAB                                             
         DC    CL6'MKGORB',XL1'02,00',AL4(MKGORB)                               
ORB      EQU   (*-RECTABLE)/LRECTAB                                             
         DC    CL6'MKGDTL',XL1'03,00',AL4(MKGDTL)                               
DTL      EQU   (*-RECTABLE)/LRECTAB                                             
         DC    CL6'MKGTLR',XL1'04,00',AL4(MKGTLR)                               
TLR      EQU   (*-RECTABLE)/LRECTAB                                             
         DC    XL2'0000'           DELIMITER                                    
         EJECT                                                                  
         EJECT                                                                  
       ++INCLUDE FASYSLST                                                       
*                                                                               
WORKD    DSECT                                                                  
SRPARS   DS    0XL32                                                            
SRPAR1   DS    A                                                                
SRPAR2   DS    A                                                                
SRPAR3   DS    A                                                                
SRPAR4   DS    A                                                                
SRPAR5   DS    A                                                                
SRPAR6   DS    A                                                                
SRPAR7   DS    A                                                                
SRPAR8   DS    A                                                                
*                                                                               
DMCB     DS    6F                                                               
         ORG   DMCB                                                             
PAR1     DS    F                                                                
PAR2     DS    F                                                                
PAR3     DS    F                                                                
PAR4     DS    F                                                                
PAR5     DS    F                                                                
PAR6     DS    F                                                                
*                                                                               
RELO     DS    A                                                                
SAVERD   DS    A                                                                
SAVER1   DS    A                                                                
ATIA     DS    A                                                                
AUTL     DS    A                                                                
DUB      DS    D                                                                
MNDSKADD DS    F                                                                
DSKADDR  DS    F                                                                
FULL     DS    F                                                                
HALF     DS    H                                                                
BYTE     DS    X                                                                
ACOMFACS DS    A                                                                
VHEXIN   DS    A                                                                
VHEXOUT  DS    A                                                                
VGETFACT DS    A                                                                
VSWITCH  DS    A                                                                
VHELLO   DS    A                                                                
VDATCON  DS    A                                                                
VDATVAL  DS    A                                                                
VGETDAY  DS    A                                                                
VCALLOVL DS    A                                                                
VLOCKET  DS    A                                                                
VCASHVAL DS    A                                                                
VADDAY   DS    A                                                                
VRECUP   DS    A                                                                
VDEMOCON DS    A                                                                
*                                                                               
ASTAPACK DS    A                                                                
*                                                                               
KEY      DS    XL48                                                             
KEYSAVE  DS    XL48                                                             
NOTCKEY  DS    XL48                                                             
OFFRKEY  DS    XL48                                                             
WORK     DS    CL64                                                             
PQINDEX  DS    CL40                PRINT QUEUE INDEX                            
         DS    XL4                 PQ RECORD LENGTH                             
R        DS    CL210               PQ RECORD DATA                               
POWERCDE DS    CL2                 POWER CODE TO USE                            
LASTREC  DS    XL1                 LAST RECORD INDICATOR                        
VERSION  DS    X                   VERSION NUMBER                               
RECFLAG  DS    X                                                                
INPROG   EQU   X'80'               CURRENTLY BUILDING RECORDS                   
RECWRITN EQU   X'40'               NEW VERSION OF OFFFER RECORDS OUT            
NEWRECRD EQU   X'20'               NEW SEQUENCE OF OFFER RECORD ADDED           
REJCOMNT EQU   X'10'               REJECTION COMMENT                            
MISSDINC EQU   X'08'               MISSED SPOTS INCLUDED                        
                                                                                
RECFLAG2 DS    X                                                                
MNWINPRG EQU   X'80'               CURRENTLY BUILDING MKGMNW                    
ONWINPRG EQU   X'40'               CURRENTLY BUILDING MKGONW                    
DEMINPRG EQU   X'20'               CURRENTLY BUILDING MKGDEM                    
                                                                                
ELTAREA  DS    CL100               ELEMENT BUILD AREA                           
SVNTDCEL DS    CL100               SAVED NON-TRAD DEMO CATEGORY ELEM            
ELEM     DS    CL256               ELEMENT BUILD AREA                           
SVMODMEL DS    CL(MODMOVRH+L'MODMDEMO*4)   TRADITIONAL DEMO VALUE ELEM          
SVMG2DEC DS    C                                                                
SVNTDMEL DS    CL(MONTOVRH+L'MONTDEMO*10)  NON-TRAD DEMO VALUE ELEM             
       ++INCLUDE DEDBLOCK                                                       
         DS    XL20                RESERVE SPACE FOR DEDBLOCK                   
*                                                                               
RETCODE  DS    X                   RETURN CODE TO THE CALLER                    
*                                                                               
REVISION DS    XL1                                                              
DMINBTS  DS    XL1                                                              
BITFLAG1 DS    XL1                 1ST SERIES OF BIT FLAGS                      
BF1REPCH EQU   X'80'                - REP IN MKGD IS DIFFERENT                  
*                                                                               
IOWORK   DS    12D                 IO WORK AREA                                 
*                                                                               
ASPLAREA DS    A                   A(SPOOL AREA)                                
APQREC   DS    A                   ADDRESS OF PQREC                             
ASPOOL   DS    A                   ADDRESS OF PQREC                             
AIO      DS    A                   CURRENT IO1 ADDRESS                          
AIO1     DS    A                   ADDRESS OF IO1                               
AIO2     DS    A                   ADDRESS OF IO2                               
AIO3     DS    A                   ADDRESS OF IO3                               
AIO4     DS    A                   ADDRESS OF IO4                               
AIO5     DS    A                   ADDRESS OF IO5                               
TMKGCOST DS    F                   TOTAL OF MAKEGOOD COSTS                      
MKGCOUNT DS    F                   COUNT OF ALL RECORDS                         
DS1COUNT DS    X                   COUNT OF OFFER COMMENTS                      
BUYCOUNT DS    X                   COUNT OF BUYS HEADERS PER OFFER-NTWK         
ORBCOUNT DS    X                   COUNT OF ORBITS PER BUY HEADER               
COMCOUNT DS    X                   COUNT OF COMMENTS PER BUY HEADER             
DTLCOUNT DS    X                   COUNT OF COMMENTS PER BUY HEADER             
ERROR    DS    XL2                                                              
TMKGSPTS DS    XL3                 TOTAL MAKEGOOD SPOTS PER MKGHDR              
TBMKSPTS DS    XL3                 TOTAL MAKEGOOD SPOTS PER MKGBUY              
JDTTODAY DS    PL4                                                              
ORDRDATE DS    PL4                                                              
HEXORDER DS    XL4                                                              
PACKOF4B DS    PL4                                                              
PACK4    DS    PL4                                                              
RCHDBUY  DS    C                   Y=REACHED A BUY AFTER A MISSED SPOT          
RCHEMPTD DS    C                   Y=REACHED EMPTY DEMO ENTRY                   
*                                                                               
NEXISTS  DS    X                   NOTICE RECORD FLAG                           
OEXISTS  DS    X                   OFFER RECORD FLAG                            
CEXISTS  DS    X                   CABLE NOTICE/OFFER RECORD FLAG               
CODEXIST DS    X                   CABLE OFFER SELLER DEMO RECORD FLAG          
EXISTS   DS    X                   FOR THE FOLLOWING EXISTS VARIABLES           
EXONFILE EQU   X'80'                - X'80' RECORD IS ON FILE                   
EXDEL    EQU   X'40'                - X'40' RECORD IS DELETED                   
EXUPD    EQU   X'20'                - X'20' NEED TO BE WRITTEN TO FILE          
*                                                                               
ERRFLG1  DS    X                   ERROR FLAGS                                  
EF1SKUP  EQU   X'80'               SKIP UPDATE                                  
MISCFLG1 DS    X                   MISCELLANEOUS FLAGS                          
MF1SKIP  EQU   X'80'               SKIP THIS REPORT UNTIL NXT TIMER POP         
MF1NOUP  EQU   X'40'               SKIP UPDATE                                  
*                                                                               
RECSEQ   DS    X                                                                
RECOSEQ  DS    X                   LAST SEQ# OF OFFER MAKEGOOD RECS             
RECNSEQ  DS    X                   LAST SEQ# OF NOTICE MAKEGOOD RECS            
RECCSEQ  DS    X                   LAST SEQ# OF CABLE NOT/OFF MKG RECS          
DATADISP DS    H                                                                
ELCODE   DS    X                   ELEMENT CODE                                 
PTIME    DS    PL3                 PACKED DATE                                  
BINORDER DS    0XL4                BINARY ORDER NUMBER                          
BINORDDT DS    XL2                     DATE PORTION                             
BINORDSQ DS    XL2                     SEQUENCE PORTION                         
ORDNO    DS    CL8                 EBCIDIC ORDER NO.                            
SPTSENUM DS    XL1                 SPOT SYSTEM SENUM                            
AGYMD    DS    X                   BINARY AGENCY/MEDIA CODE                     
QAGY     DS    CL2                 AGENCY POWER CODE                            
BUYER    DS    CL3                 BUYER CODE                                   
BCLT     DS    XL2                 BINARY CLIENT CODE                           
BEST     DS    XL1                 BINARY ESTIMATE                              
BSTATN   DS    XL3                 BINARY STATION CODE                          
QSTATN   DS    CL5                 STATION CALL LETTERS                         
MKGREPID DS    XL2                 MAKEGOOD REP ID NUMBER                       
VER      DS    XL1                 VERSION NUMBER                               
MISSSEQ  DS    XL1                 MISSED BUYLINE SEQUENCE NUMBER               
LINENUM  DS    XL1                 LINE NUMBER IN REPORT                        
GROUPID  DS    CL3                 GROUP ID                                     
MSOFFNUM DS    X                                                                
MGOFFNUM DS    X                                                                
OFFERNUM DS    X                   BINARY OFFER NUMBER                          
SVOFFNUM DS    CL2                 OFFER NUMBER FROM MKGBUY                     
DAYOFWEK DS    X                   1=MON,...,7=SUN                              
SAVEHDR  DS    CL(MOFRHDRL)        SAVE THE HEADER                              
SAVEREJT DS    5XL73               SAVE REJECTION COMMENT                       
SAVEREJX EQU   *                                                                
SVCSLIC  DS    CL32                                                             
*                                                                               
*                                                                               
IO1      DS    6000X                                                            
IO2      DS    6000X                                                            
IO3      DS    6000X                                                            
IO4      DS    6000X                                                            
IO5      DS    6000X                                                            
SPULAREA DS    XL3200                                                           
         DS    0D                                                               
PQREC    DS    14336X              PRINT QUEUE BUFFER                           
WORKX    EQU   *                                                                
*                                                                               
         EJECT                                                                  
*                                                                               
*   DREPTABL:  DSECT COVERING THE SRTIM-GENERATED TABLE ENTRIES                 
*                                                                               
DREPTABL DSECT                                                                  
*                                                                               
DRUSERID DS    XL2                 BINARY USER ID CODE                          
DRSUBID  DS    CL3                 SUB ID                                       
DRREPT#  DS    XL2                 BINARY REPORT NUMBER                         
LCORTAB  EQU   *-DRUSERID          LENGTH OF ENTRY                              
*                                                                               
*                                                                               
*   DSECT COVERING PRINT QUEUE INDEX                                            
*                                                                               
       ++INCLUDE DMPRTQK                                                        
         EJECT                                                                  
*                                                                               
*                                                                               
         EJECT                                                                  
* FASYSLSTD                                                                     
* DDCOMFACS                                                                     
* FAFACTS                                                                       
* FADSECTS                                                                      
* SRDARFED                                                                      
* DDSPOOLD                                                                      
* FAPQPL                                                                        
* DDFLDHDR                                                                      
* CTGENFILE                                                                     
         PRINT OFF                                                              
       ++INCLUDE FASYSLSTD                                                      
       ++INCLUDE DDCOMFACS                                                      
       ++INCLUDE FAFACTS                                                        
       ++INCLUDE FADSECTS                                                       
       ++INCLUDE SRDARFED                                                       
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE SPSYSFAC                                                       
       ++INCLUDE FAPQPL                                                         
       ++INCLUDE DDFLDHDR                                                       
       ++INCLUDE CTGENFILE                                                      
       ++INCLUDE CTGENSTAD                                                      
       ++INCLUDE DERENTABD                                                      
       ++INCLUDE GEGENCDC                                                       
       ++INCLUDE GEGENTOK                                                       
         PRINT ON                                                               
       ++INCLUDE SPSTAPACKD                                                     
         EJECT                                                                  
ESTHDRD  DSECT                                                                  
       ++INCLUDE SPGENEST                                                       
         EJECT                                                                  
       ++INCLUDE SPDARMKGDD                                                     
         EJECT                                                                  
       ++INCLUDE SPDARDARED                                                     
         EJECT                                                                  
       ++INCLUDE SPGENDRORD        DARE ORDER RECORD                            
         EJECT                                                                  
       ++INCLUDE SPGENDRMKO        DARE MAKEGOOD OFFER RECORD                   
MODMELD  DSECT                                                                  
         ORG   MODMDEMO                                                         
MDEMNO   DS    XL3       B         X'00'/DEMO TYPE/DEMO NUMBER                  
MDSVI    DS    CL1       B         HUT ADJUSTMENT                               
MDEMRAW  DS    CL4       B         DEMO VALUE                                   
*                                                                               
MONTELD  DSECT                                                                  
         ORG   MONTDEMO                                                         
NTDEMNO  DS    XL3       B         X'00'/DEMO TYPE/DEMO NUMBER                  
NTDMFLG1 DS    XL1       B         DEMO FLAG1                                   
NTDEMRAW DS    XL4       B         DEMO VALUE                                   
         EJECT                                                                  
       ++INCLUDE SPGENDRMKN        DARE MAKEGOOD NOTICE RECORD                  
         EJECT                                                                  
       ++INCLUDE DDGETDARED                                                     
         EJECT                                                                  
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'135SRDAR01   11/03/20'                                      
         END                                                                    
