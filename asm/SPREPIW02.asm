*          DATA SET SPREPIW02  AT LEVEL 073 AS OF 08/03/20                      
*$PANAPT01S$ *******  FIRST LINE TO DELETE  *****************                   
*                                                           *                   
*  ATTN: THE LEVEL STAMPS ARE *LEGITIMATELY* OUT-OF-SYNC!   *                   
*        DELETE THIS COMMENT BLOCK ON THE NEXT PROMOTION!   *                   
*                                                           *                   
*  THIS SOURCE MODULE IS AT A HIGHER LEVEL NUMBER THAN ITS  *                   
*  CORRESPONDING LOAD OR OBJECT MODULE, BECAUSE THE MEMBER  *                   
*  WAS PROMOTED USING THE SRCE LIBCODE VIA MR# 050711.      *                   
*                                                           *                   
*  THIS COMMENT BLOCK WAS INSERTED *PROGRAMMATICALLY* BY    *                   
*  PANAPT, TO EXPLAIN WHY THE LEVEL STAMPS ARE OUT-OF-SYNC. *                   
*  BEFORE THIS MEMBER CAN BE PROMOTED AGAIN, THIS ENTIRE    *                   
*  COMMENT BLOCK *MUST* BE MANUALLY DELETED.                *                   
*                                                           *                   
*$PANAPT01X$ *******  LAST LINE TO DELETE  ******************                   
*PHASE SPIW02A                                                                  
*INCLUDE SORTER                                                                 
         TITLE 'SPIW02 - AUTOPAY WORKER FILE ERRORS'                            
***********************************************************************         
* USER    JIRA      DATE                  CHANGE LOG                  *         
* ---- ---------- -------- ------------------------------------------ *         
* AKAT SPEC-7767  11/15/16 SUPPORT NEW MEDIA OFFICE LIST              *         
***********************************************************************         
SPIW02   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,SPIW02,R8,RR=R2                                                
                                                                                
         L     RA,0(R1)                                                         
         USING SPWORKD,RA,R9                                                    
         LA    R9,2048(RA)                                                      
         LA    R9,2048(R9)                                                      
*                                                                               
         CLI   MODE,REQFRST                                                     
         BE    IW10                                                             
         B     EXIT                                                             
*                                                                               
IW10     DS    0H                                                               
         L     RF,VMASTC                                                        
         USING MASTD,RF                                                         
         CLI   MCNETPAK,C'Y'        NET AUTOPAY?                                
         BE    IWN10                                                            
         DROP  RF                                                               
*                                                                               
         GOTO1 DATCON,DMCB,(4,RCDATE),(2,CTDAY)                                 
         GOTO1 DATCON,DMCB,(4,RCDATE),(30,CTDAY64)                              
         XC    SVDATE,SVDATE                                                    
*                                                                               
         GOTO1 =V(SORTER),DMCB,SORTCARD,RECCARD                                 
         LA    R1,MYHEAD                                                        
         ST    R1,HEADHOOK                                                      
*                                                                               
         USING CT5REC,R6                                                        
         XC    KEY,KEY                                                          
         LA    R6,KEY                                                           
         MVI   CT5KTYP,CT5KTYPQ    RECORD TYPE '5'                              
         MVC   CT5KALPH,QAGY                                                    
         GOTO1 DATAMGR,DMCB,(0,=C'DMRDHI'),=C'CTFILE',KEY,IO2                   
         LA    R6,IO2                                                           
         CLC   CT5KEY(25),KEY                                                   
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         USING CTSYSD,R6                                                        
         LA    R6,28(R6)           GET SPOT SE NUMBER                           
IW11     CLI   0(R6),0                                                          
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLI   0(R6),X'21'                                                      
         BE    IW14                                                             
IW12     ZIC   R1,1(R6)                                                         
         AR    R6,R1                                                            
         B     IW11                                                             
                                                                                
IW14     CLI   CTSYSNUM,X'0A'                                                   
         BNE   IW12                                                             
         L     RE,UTL                                                           
         MVC   SVSPUTL,4(RE)                                                    
         MVC   4(1,RE),CTSYSSE                                                  
         DROP  R6                                                               
*                                                                               
         GOTO1 DATAMGR,DMCB,(0,=C'DMOPEN'),=C'CONTROL',                +        
               =C'NGENDIR NGENFIL X',WKBUFF,0                                   
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   WRKRID,RCORIGID     DEFAULT TO ORIG ID                           
         CLC   QAGY,=C'TH'         AGENCY TH?                                   
         BNE   *+14                NO                                           
         CLC   =C'$S',QCLT         OFFICE LIST $S?                              
         BE    IW18                YES - LEAVE WRKRID ALONE!                    
*                                                                               
         LA    R4,AGYTAB           OVERRIDE JCL USERID                          
IW15     CLI   0(R4),X'FF'         TO READ WORKER FILES                         
         BE    IW18                                                             
         CLC   QAGY,0(R4)                                                       
         BE    IW16                                                             
         LA    R4,4(R4)                                                         
         B     IW15                                                             
IW16     MVC   WRKRID,2(R4)                                                     
*                                                                               
IW18     LA    R4,IO2                                                           
         USING SORTRECD,R4                                                      
         XC    WRKRINDX,WRKRINDX                                                
         LA    R2,WRKRINDX                                                      
         USING UKRECD,R2                                                        
         MVC   UKUSRID,WRKRID                                                   
***      MVC   UKSYSPRG(3),=C'APY'                                              
         CLI   QOPT1,C'T'          TST SYSTEM                                   
         BNE   *+8                                                              
         MVI   UKSUBPRG,C'T'                                                    
         CLI   QOPT1,C'C'          CSC SYSTEM                                   
         BNE   *+8                                                              
         MVI   UKSUBPRG,C'S'                                                    
         GOTO1 DATAMGR,DMCB,(0,=C'GFILE'),=C'WRKFIL',WRKRINDX,IO,      C        
               AWKBUFF                                                          
         MVC   WRKFILE,UKUSRINF    WORKER FILE                                  
         XC    WRKRINDX,WRKRINDX   CLEAR THE KEY FOR FIRST READHI               
*                                                                               
IW20     DS    0H                                                               
         NI    FLAG,X'FF'-(TMSTMP+XAUTPAY)                                      
         LA    R2,WRKRINDX                                                      
         GOTO1 DATAMGR,DMCB,DINDEX,WRKFILE,WRKRINDX,IO,AWKBUFF                  
         CLI   8(R1),0                                                          
         BE    IW30                                                             
         CLI   8(R1),X'90'         EOF?                                         
         BE    IWX                 YES                                          
         DC    H'0'                                                             
                                                                                
IW30     DS    0H                                                               
         CLC   UKUSRID,WRKRID                                                   
         BNE   IW20                                                             
         CLC   UKSYSPRG(3),=C'APY'                                              
         BNE   IW20                                                             
*                                                                               
         TM    UKATTB,WLATNCD      WRKF USING NEW COMPRRESSED DATES?            
         BZ    IW32                NO                                           
         CLC   CTDAY64,UKAGERD     RETAIN DATE BEFORE TODAY?                    
         BH    IW20                YES, FILE HAS EXPIRED, SKIP IT               
         GOTO1 DATCON,DMCB,(14,UKAGELD),(2,SVDATE) DATE IS OLD COMPRSS          
         B     IW34                SAVE DATE FOR READING AUTOPAY RECS           
*                                                                               
IW32     CLC   CTDAY,UKAGERD       RETAIN DATE BEFORE TODAY?                    
         BH    IW20                YES, FILE HAS EXPIRED, SKIP IT               
         MVC   SVDATE,UKAGELD      SAVE DATE FOR READING AUTOPAY RECS           
         DROP  R2                                                               
*                                                                               
IW34     OI    FLAG,NEWRKR         PROCESSING A NEW WRKR FILE W/O ERROR         
                                                                                
IW40     DS    0H                                                               
         LA    R0,IO               SET 'TO' ADDRESS                             
         LA    R1,L'IO             SET 'TO' LENGTH                              
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
                                                                                
         GOTO1 DATAMGR,DMCB,DREAD,WRKFILE,WRKRINDX,IO,AWKBUFF                   
         CLI   8(R1),0                                                          
         BE    IW50                                                             
         CLI   8(R1),X'90'         EOF?                                         
         BE    IW160               YES                                          
         DC    H'0'                                                             
                                                                                
IW50     DS    0H                                                               
         TM    FLAG,TMSTMP         ALREADY GOT TIME STAMP?                      
         BO    IW60                                                             
         MVC   SVTMSTMP,IO+59+4    TIME STAMP                                   
         OI    FLAG,TMSTMP                                                      
         CLI   IO+16,C'X'          AUTOPAY ON XSPFIL?                           
         BNE   IW40                NO                                           
         OI    FLAG,XAUTPAY        YES                                          
         B     IW40                                                             
                                                                                
         USING WRECD,R3            WORKER  REC                                  
IW60     DS    0H                                                               
         LA    R3,IO+14                                                         
         CLC   =C'ES#',WRHDR       ANY ERROR (IT'S ES# ON TST)?                 
         BE    IW65                YES                                          
         CLC   =C'ES\',WRHDR       ANY ERROR (IT'S ES\ ON CSC)?                 
         BE    IW65                YES                                          
         CLC   =C'ES/',WRHDR       ANY ERROR?                                   
         BNE   IW40                NO GET NEXT REC                              
IW65     CLC   =C'0119',WRHDR+3    ERROR = ALL SPOTS ARE PAID                   
         BE    IW40                SKIP - NOT REALLY AN ERROR                   
         PACK  DUB,WRHDR+3(4)                                                   
         CVB   R1,DUB                                                           
         STCM  R1,3,ERRNO                                                       
                                                                                
IW100    XC    IO2(SRLENQ),IO2                                                  
                                                                                
         LA    R5,GTKEY            DEFINE MESSAGE KEY                           
         USING GMSGD,R5                                                         
         XC    GMKEY,GMKEY                                                      
         MVI   GMKREC,GMKRECQ                                                   
         MVI   GMKSYS,2                                                         
         MVI   GMKTYP,GMKTERR                                                   
         MVC   GMKMSG,ERRNO                                                     
         MVI   GMKLANG,X'FF'                                                    
                                                                                
         MVC   GTKEYSVE,GMKEY                                                   
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'GENDIR',GTKEYSVE,GMKEY,0              
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         LA    R1,WKBUFF                                                        
         CLC   GMKEY,GTKEYSVE                                                   
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         GOTO1 DATAMGR,DMCB,=C'GETREC',=C'GENFIL',GMKEY+36,WKBUFF,     +        
               IOWORK                                                           
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         DROP  R5                                                               
                                                                                
         LA    R5,WKBUFF                                                        
         USING GMSGEL,R5                                                        
         MVI   ELCODE,GMSGELC                                                   
         BAS   RE,GETEL2                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         ZIC   R1,GMSGELL                                                       
         SH    R1,=Y(GMSGFXDL+1)   LENGTH OF MESSAGE -1                         
         CHI   R1,49                                                            
         BNH   *+8                                                              
         LA    R1,49                                                            
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   SRERROR(0),GMSGTXT                                               
         DROP  R5                                                               
*                                                                               
IW130    TM    FLAG,XAUTPAY        AUTOPAY ON XSPFIL?                           
         BNZ   IW140               YES - WORKER FILE IS DIFFERENT               
         MVC   SRMED,WRMEDIA       MEDIA                                        
         MVC   SRCLT,WRCLIENT      CLIENT                                       
         MVC   SRPRD,WRPRD         PRODUCT                                      
         MVC   SRPRD2,WRPRD2       PARTNER                                      
         MVC   SREST,WREST         ESTIMATE                                     
         MVC   SRSTAT,WRSTAT       STATION                                      
         CLC   =C'CK',QAGY         COKE = SKIP SREP                             
         BE    *+10                                                             
         MVC   SRSREP,WRSREP       SREP                                         
         MVC   SRMONTH,WRMONTH     MONTH                                        
         MVC   SRINVCE,WRINVCE     INVOICE                                      
         CLC   =C'CK',QAGY         COKE = USE ACN                               
         BNE   *+10                                                             
         MVC   SRACN,WRACN         ACN NUMBER                                   
         B     IW150                                                            
         DROP  R3                                                               
*                                                                               
         USING WRECDX,R3           XSPFIL AUTOPAY WORKER FILE DSECT             
IW140    MVC   SRMED,WRMEDIAX      MEDIA                                        
         MVC   SRCLT,WRCLTX        CLIENT                                       
         MVC   SRPRD,WRPRDX        PRODUCT                                      
         MVC   SRPRD2,WRPRD2X      PARTNER                                      
         MVC   SREST,WRESTX        ESTIMATE                                     
         MVC   SRSTAT,WRSTATX      STATION                                      
         MVC   SRSREP,WRSREPX      SREP                                         
         MVC   SRMONTH,WRMONTHX    MONTH                                        
         MVC   SRINVCE(10),WRINVNUM INVOICE                                     
         DROP  R3                  DROP XSPFIL AUTOPAY WRKR FILE USING          
*                                                                               
IW150    GOTO1 =V(SORTER),DMCB,=C'PUT',(R4)                                     
         B     IW40                                                             
*                                                                               
IW160    GOTO1 DATAMGR,DMCB,DCLOSE,WRKFILE,0,IO,AWKBUFF                         
*&&DO*&& GOTO1 DATAMGR,DMCB,=C'SENT',WRKFILE,WRKRINDX,IO,AWKBUFF                
         B     IW20                                                             
*                                                                               
IWX      L     RE,UTL                                                           
         MVC   4(1,RE),SVSPUTL                                                  
*                                                                               
         BAS   RE,I2ERRS          GO READ FOR ERRORS ON AUTOPAY RECS            
         BAS   RE,PRINTIT          PRINT REPORT                                 
         GOTO1 =V(SORTER),DMCB,=C'END'                                          
*                                                                               
IWXX     GOTO1 AENDREQ                                                          
*********************************************************************           
*        NETPAK AUTOPAY                                                         
*********************************************************************           
IWN10    DS    0H                                                               
         GOTO1 DATCON,DMCB,(4,RCDATE),(2,CTDAY)                                 
         GOTO1 DATCON,DMCB,(4,RCDATE),(30,CTDAY64)                              
         XC    SVDATE,SVDATE                                                    
*                                                                               
         GOTO1 =V(SORTER),DMCB,SORTCAR2,RECCARD2                                
         LA    R1,MYHEADN                                                       
         ST    R1,HEADHOOK                                                      
*                                                                               
         USING CT5REC,R6                                                        
         XC    KEY,KEY                                                          
         LA    R6,KEY                                                           
         MVI   CT5KTYP,CT5KTYPQ    RECORD TYPE '5'                              
         MVC   CT5KALPH,QAGY                                                    
         GOTO1 DATAMGR,DMCB,(0,=C'DMRDHI'),=C'CTFILE',KEY,IO2                   
         LA    R6,IO2                                                           
         CLC   CT5KEY(25),KEY                                                   
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         USING CTSYSD,R6                                                        
         LA    R6,28(R6)           GET SPOT SE NUMBER                           
IWN11    CLI   0(R6),0                                                          
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLI   0(R6),X'21'                                                      
         BE    IWN14                                                            
IWN12    ZIC   R1,1(R6)                                                         
         AR    R6,R1                                                            
         B     IWN11                                                            
                                                                                
IWN14    CLI   CTSYSNUM,X'0A'                                                   
         BNE   IWN12                                                            
         L     RE,UTL                                                           
         MVC   SVSPUTL,4(RE)                                                    
         MVC   4(1,RE),CTSYSSE                                                  
         DROP  R6                                                               
*                                                                               
         GOTO1 DATAMGR,DMCB,(0,=C'DMOPEN'),=C'CONTROL',                +        
               =C'NGENDIR NGENFIL X',WKBUFF,0                                   
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   WRKRID,RCORIGID     DEFAULT TO ORIG ID                           
*                                                                               
         LA    R4,NAGYTAB          OVERRIDE JCL USERID                          
IWN15    CLI   0(R4),X'FF'         TO READ WORKER FILES                         
         BE    IWN18                                                            
         CLC   QAGY,0(R4)                                                       
         BE    IWN16                                                            
         LA    R4,4(R4)                                                         
         B     IWN15                                                            
IWN16    MVC   WRKRID,2(R4)                                                     
*                                                                               
IWN18    LA    R4,IO2                                                           
         USING NSRTRECD,R4                                                      
         XC    WRKRINDX,WRKRINDX                                                
         LA    R2,WRKRINDX                                                      
         USING UKRECD,R2                                                        
         MVC   UKUSRID,WRKRID                                                   
***      MVC   UKSYSPRG(3),=C'NAP'                                              
         CLI   QOPT1,C'T'          TST SYSTEM                                   
         BNE   *+8                                                              
         MVI   UKSUBPRG,C'T'                                                    
         CLI   QOPT1,C'C'          CSC SYSTEM                                   
         BNE   *+8                                                              
         MVI   UKSUBPRG,C'S'                                                    
         GOTO1 DATAMGR,DMCB,(0,=C'GFILE'),=C'WRKFIL',WRKRINDX,IO,      C        
               AWKBUFF                                                          
         MVC   WRKFILE,UKUSRINF    WORKER FILE                                  
         XC    WRKRINDX,WRKRINDX   CLEAR THE KEY FOR FIRST READHI               
*                                                                               
IWN20    DS    0H                                                               
         NI    FLAG,X'FF'-TMSTMP                                                
         LA    R2,WRKRINDX                                                      
         GOTO1 DATAMGR,DMCB,DINDEX,WRKFILE,WRKRINDX,IO,AWKBUFF                  
         CLI   8(R1),0                                                          
         BE    IWN30                                                            
         CLI   8(R1),X'90'         EOF?                                         
         BE    IWNX                YES                                          
         DC    H'0'                                                             
                                                                                
IWN30    DS    0H                                                               
         CLC   UKUSRID,WRKRID                                                   
         BNE   IWN20                                                            
         CLC   UKSYSPRG(3),=C'NAP'                                              
         BNE   IWN20                                                            
*                                                                               
         TM    UKATTB,WLATNCD      WRKF USING NEW COMPRRESSED DATES?            
         BZ    IWN32               NO                                           
         CLC   CTDAY64,UKAGERD     RETAIN DATE BEFORE TODAY?                    
         BH    IWN20               YES, FILE HAS EXPIRED, SKIP IT               
         GOTO1 DATCON,DMCB,(14,UKAGELD),(2,SVDATE) DATE IS OLD COMPRSS          
         B     IWN34               SAVE DATE FOR READING AUTOPAY RECS           
*                                                                               
IWN32    CLC   CTDAY,UKAGERD       IS RETAIN DATE BEFORE TODAY                  
         BH    IWN20               THEN SKIP FILE HAS EXPIRED                   
         MVC   SVDATE,UKAGELD      SAVE DATE FOR READING AUTOPAY RECS           
         DROP  R2                                                               
*                                                                               
IWN34    OI    FLAG,NEWRKR         PROCESSING A NEW WRKR FILE W/O ERROR         
*                                                                               
IWN40    DS    0H                                                               
         LA    R0,IO               SET 'TO' ADDRESS                             
         LA    R1,L'IO             SET 'TO' LENGTH                              
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
                                                                                
         GOTO1 DATAMGR,DMCB,DREAD,WRKFILE,WRKRINDX,IO,AWKBUFF                   
         CLI   8(R1),0                                                          
         BE    IWN50                                                            
         CLI   8(R1),X'90'         EOF?                                         
         BE    IWN160              YES                                          
         DC    H'0'                                                             
                                                                                
IWN50    DS    0H                                                               
         TM    FLAG,TMSTMP         ALREADY GOT TIME STAMP?                      
         BO    IWN60                                                            
         MVC   SVTMSTMP,IO+59+4    TIME STAMP                                   
         OI    FLAG,TMSTMP                                                      
         B     IWN40                                                            
                                                                                
         USING NWRECD,R3            WORKER  REC                                 
IWN60    DS    0H                                                               
                                                                                
         LA    R3,IO+14                                                         
*                                                                               
         CLC   =C'ET/0082',NWRMSG                                               
         BE    IWN40                 SKIP THESE                                 
         CLC   NWRMSG(3),=C'ET/'     ERROR?                                     
         BE    IWN100                                                           
         CLC   NWRMSG(3),=C'ET#'     ERROR?                                     
         BE    IWN100                                                           
*                                                                               
         LA    RF,ERRTAB                                                        
IWN65    CLI   0(RF),X'FF'                                                      
         BE    IWN40                                                            
         CLC   0(L'ERRTAB,RF),NWRMSG                                            
         BE    IWN100                                                           
         LA    RF,L'ERRTAB(RF)                                                  
         B     IWN65                                                            
                                                                                
IWN100   XC    IO2(NSRLENQ),IO2                                                 
*                                                                               
IWN130   MVC   NSRMED,NWRMED       MEDIA                                        
         MVC   NSRCLT,NWRCLI       CLIENT                                       
         MVC   NSRSTA,NWRSTA       STATION                                      
         MVC   NSRSREP,NWRSREP     SREP                                         
         MVC   NSRMONTH,NWRMONTH   MONTH                                        
*                                                                               
         MVC   NSRINV,NWRINV1      INVOICE                                      
         MVC   NSRERROR,NWRMSG     ERRORR                                       
         MVC   NSROPT,NWROPT       OPTIONS                                      
*                                                                               
         LA    RF,NWROPT                                                        
*                                                                               
IWN132   CLI   0(RF),C' '                                                       
         BE    IWN150                                                           
         CLC   0(2,RF),=C'E='      ESTIMATE?                                    
         BNE   IWN134                                                           
         AHI   RF,2                                                             
         LA    RE,NSREST                                                        
IWN132A  CLI   0(RF),C' '                                                       
         BE    IWN150                                                           
         CLI   0(RF),C','                                                       
         BNE   *+12                                                             
         AHI   RF,1                                                             
         B     IWN134                                                           
         MVC   0(1,RE),0(RF)                                                    
         AHI   RF,1                                                             
         AHI   RE,1                                                             
         B     IWN132A                                                          
*                                                                               
IWN134   CLI   0(RF),C' '                                                       
         BE    IWN150                                                           
         CLC   0(3,RF),=C'PA='      PACKAGE?                                    
         BNE   IWN136                                                           
         AHI   RF,3                                                             
         LA    RE,NSRPAK                                                        
IWN134A  CLI   0(RF),C' '                                                       
         BE    IWN150                                                           
         CLI   0(RF),C','                                                       
         BNE   *+12                                                             
         AHI   RF,1                                                             
         B     IWN136                                                           
         MVC   0(1,RE),0(RF)                                                    
         AHI   RF,1                                                             
         AHI   RE,1                                                             
         B     IWN134A                                                          
*                                                                               
IWN136   CLI   0(RF),C' '                                                       
         BE    IWN150                                                           
         CLC   0(2,RF),=C'P='       PRODUCT?                                    
         BNE   IWN140                                                           
         AHI   RF,3                                                             
         LA    RE,NSRPRD                                                        
IWN136A  CLI   0(RF),C' '                                                       
         BE    IWN150                                                           
         CLI   0(RF),C'-'           PARTNER?                                    
         BE    IWN138                                                           
         CLI   0(RF),C','                                                       
         BNE   *+12                                                             
         AHI   RF,1                                                             
         B     IWN140                                                           
         MVC   0(1,RE),0(RF)                                                    
         AHI   RF,1                                                             
         AHI   RE,1                                                             
         B     IWN136A                                                          
*                                                                               
IWN138   AHI   RF,1                                                             
         LA    RE,NSRPRD2                                                       
IWN138A  CLI   0(RF),C' '                                                       
         BE    IWN150                                                           
         CLI   0(RF),C','                                                       
         BNE   *+12                 PARTNER                                     
         AHI   RF,1                                                             
         B     IWN140                                                           
         MVC   0(1,RE),0(RF)                                                    
         AHI   RF,1                                                             
         AHI   RE,1                                                             
         B     IWN138A                                                          
*                                                                               
IWN140   DS    0H                                                               
         OC    NSRMED,SPACES                                                    
         OC    NSRCLT,SPACES                                                    
         OC    NSRPRD,SPACES                                                    
         OC    NSRPRD2,SPACES                                                   
         OC    NSRPAK,SPACES                                                    
         OC    NSREST,SPACES                                                    
         OC    NSRSTA,SPACES                                                    
         OC    NSRSREP,SPACES                                                   
         OC    NSRMONTH,SPACES                                                  
         OC    NSRINV,SPACES                                                    
         OC    NSRERR,SPACES                                                    
         OC    NSRERROR,SPACES                                                  
*                                                                               
IWN150   GOTO1 =V(SORTER),DMCB,=C'PUT',(R4)                                     
         B     IWN40                                                            
         DROP  R3                                                               
*                                                                               
IWN160   GOTO1 DATAMGR,DMCB,DCLOSE,WRKFILE,0,IO,AWKBUFF                         
*&&DO*&& GOTO1 DATAMGR,DMCB,=C'SENT',WRKFILE,WRKRINDX,IO,AWKBUFF                
         B     IWN20                                                            
*                                                                               
IWNX     L     RE,UTL                                                           
         MVC   4(1,RE),SVSPUTL                                                  
         BAS   RE,NPRINTIT          PRINT REPORT                                
         GOTO1 =V(SORTER),DMCB,=C'END'                                          
*                                                                               
IWNXX    GOTO1 AENDREQ                                                          
EXIT     XIT1                                                                   
         DROP  R4                                                               
*                                                                               
ERRTAB   DC    0CL(L'TOTERR)                                                    
TOTERR   DC    C'TOTALS D'                                                      
CLRERR   DC    C'** ERROR'                                                      
BSTERR   DC    C'MAX # OF'                                                      
TSARERR  DC    C'MAX # RE'                                                      
NOUNERR  DC    C'** NO UN'                                                      
         DC    C'ET#0081 '                                                      
         DC    X'FF'                                                            
         EJECT                                                                  
**********************************************************************          
*        SPECIAL AGENCY TABLE IF WORKER FILE UNDER DIFFERENT ID                 
*        AGENCY (2 CHAR) - USER-ID NUMBER                                       
**********************************************************************          
*                                                                               
         DS    0H                                                               
AGYTAB   DC    CL2'H9',H'9211'                                                  
**       DC    CL2'TH',H'12039'    DFZRES                                       
         DC    CL2'TH',H'12699'    ZEREP                                        
         DC    CL2'DF',H'12043'    DFRES                                        
         DC    X'FFFF'                                                          
         DS    0H                                                               
*                                                                               
NAGYTAB  DC    CL2'SJ',H'0017'                                                  
         DC    CL2'FR',H'6516'                                                  
         DC    CL2'H7',H'8594'                                                  
         DC    CL2'*B',H'2635'                                                  
         DC    CL2'*1',H'2650'                                                  
         DC    X'FFFF'                                                          
**********************************************************************          
*        GET ERRORS OFF OF AUTOPAY RECS TOO                                     
**********************************************************************          
I2ERRS   NTR1                                                                   
         OC    SVDATE,SVDATE       DO WE HAVE THE DATE FROM A WRKR FILE         
         BNZ   CE10                IF NOT USE TODAY-1                           
         GOTO1 DATCON,DMCB,(4,RCDATE),(0,TEMPDATE)                              
         GOTO1 ADDAY,DMCB,TEMPDATE,TEMPDAT2,-1                                  
         GOTO1 DATCON,DMCB,TEMPDAT2,(2,SVDATE)                                  
*                                                                               
CE10     CLI   QSTART,C' '         OR FORCE A PARTICULAR DAY                    
         BNH   CE14                                                             
         GOTO1 DATCON,DMCB,QSTART,(2,SVDATE)                                    
*                                                                               
CE14     XC    SVDATE,=X'FFFF'     COMPLIMENT                                   
         MVC   BYTE2,BAGYMD                                                     
         OI    BYTE2,X'F0'         AGENCY IN REQUEST                            
         USING APYRECD,R5                                                       
         XC    KEY,KEY                                                          
         LA    R5,KEY                                                           
         MVI   APYKTYP,APYKTYPQ    X'0D'                                        
         MVI   APYKSUB,APYKSUBQ    X'3A'                                        
         MVC   APYKDATE,SVDATE     DATE OF WORKER FILE                          
         GOTO1 HIGH                                                             
         B     CE20                                                             
CE20SEQ  GOTO1 SEQ                                                              
CE20     CLC   KEY(4),KEYSAVE      SAME THRU DATE                               
         BNE   CEX                                                              
         LA    R5,KEY                                                           
*                                                                               
         MVC   BYTE,KEY+4          AGY/MED                                      
         OI    BYTE,X'F0'          JUST AGY                                     
         CLC   BYTE,BYTE2          SAME AGY AS REQUEST                          
         BNE   CEX                                                              
*                                                                               
*NOT     TM    APYKCNTL,APYKCMON   ARE THERE MULTIPLE MONTHS?                   
*SET     BO    CE22                THEN CHECK ELEMS FOR ERRORS                  
*IN      TM    APYKCNTL,APYKCERR   ANY COKE ERROR TO REPORT                     
*KEY ??  BNO   CE20SEQ                                                          
*                                                                               
CE22     GOTO1 GETBUY                                                           
         L     R5,ADBUY                                                         
         MVI   MULTMONS,C'N'       SET NO MULTIPLE MONTHS IN RECORD             
         TM    APYRCNTL,APYRCMON   ARE THERE MULTIPLE MONTHS?                   
         BNO   *+12                NO, CHECK IF ERRORS                          
         MVI   MULTMONS,C'Y'       SET MULTIPLE MONTHS IN RECORD                
         B     CE24                                                             
         TM    APYRCNTL,APYRCERR   ANY ERROR TO REPORT                          
         BNO   CE20SEQ                                                          
*                                                                               
CE24     MVI   ELCODE,APYELQ       CHECK X'01' FOR ERRORS                       
         BAS   RE,GETEL                                                         
         B     *+8                                                              
CE26     BAS   RE,NEXTEL                                                        
         BNE   CE20SEQ             NEXT RECORD                                  
         USING APYEL,R5                                                         
         CLI   APYERRS,0                                                        
         BE    CE26                NEXT X'01' ELEM                              
*                                                                               
CE30     LA    R4,IO2                                                           
         USING SORTRECD,R4                                                      
         XC    IO2(SRLENQ),IO2                                                  
         MVC   SRMED,APYMED                  MEDIA                              
         MVC   SRCLT,APYCLT                  CLIENT                             
         MVC   SRPRD,APYPRD                  PRODUCT                            
         MVC   SRPRD2,APYPRD2                PARTNER                            
         MVC   SREST,APYEST                  ESTIMATE                           
         MVC   SRSTAT(L'APYSTA),APYSTA       STATION                            
         MVC   SRSREP,APYSREP                MARKET FOR COKE- NO SREP           
         CLC   =C'CK',QAGY                   COKE = SREP=MKT                    
         BNE   *+10                                                             
         MVC   SRSREP,APYMKT                 MARKET FOR COKE- NO SREP           
         MVC   SRMONTH,APYMONTH              MONTH                              
         MVC   SRERR,APYERRS                                                    
         CLC   =C'CK',QAGY                   COKE = USE ACN                     
         BNE   CE33                                                             
         CLI   APYLEN,APYLN2Q      BIG ELEM                                     
         BL    CE33                                                             
         MVC   SRACN,APYACN                                                     
*                                                                               
CE33     CLI   MULTMONS,C'Y'       PROCESSING MORE THAN 1 MONTH                 
         BNE   CE45                NO THEN GET INVOICE INFO                     
         GOTO1 =V(SORTER),DMCB,=C'PUT',(R4)                                     
         B     CE26                ANY MORE X'01' ELEMS                         
*                                                                               
CE45     L     R5,ADBUY                                                         
         USING APYIEL,R5                                                        
         MVI   ELCODE,APYIELQ      X'02' INVOICE ELEM                           
         BAS   RE,GETEL                                                         
         BNE   CE50                                                             
         MVC   SRINVCE(L'APYIINV),APYIINV                                       
         CLI   APYINUM,1                                                        
         BNH   *+8                                                              
         MVI   SRINVCE+10,C'+'                                                  
CE50     GOTO1 =V(SORTER),DMCB,=C'PUT',(R4)                                     
         B     CE20SEQ                                                          
*                                                                               
CEX      XIT1                                                                   
         DROP  R4,R5                                                            
         EJECT                                                                  
**********************************************************************          
*        PRINT REPORT FROM SORTER RECORDS (NET)                                 
**********************************************************************          
NPRINTIT NTR1                                                                   
         MVI   RCSUBPRG,2                                                       
         XC    LASTCLT,LASTCLT                                                  
         XC    LASTMED,LASTMED                                                  
         MVI   CLTSEC,C'N'         DEFAULT TO NOT SEEING FOR                    
         CLI   QCLT,C'$'           OFFICE LIST SECURITY                         
         BE    *+8                                                              
         MVI   CLTSEC,C'Y'         EVERYONE ELSE GETS ALL CLTS                  
NPR10    GOTO1 =V(SORTER),DMCB,=C'GET'                                          
         ICM   R4,15,4(R1)                                                      
         BZ    NPRX                                                             
         MVC   IO2(NSRLENQ),0(R4)                                               
         LA    R4,IO2                                                           
         USING NSRTRECD,R4                                                      
         LA    R5,P1                                                            
         USING NPRINTD,R5                                                       
*                                                                               
         CLC   NSRCLT,LASTCLT                                                   
         BNE   NPR20                                                            
         CLC   NSRMED,LASTMED                                                   
         BE    NPR60                                                            
NPR20    MVI   FORCEHED,C'Y'                                                    
         MVC   LASTCLT,NSRCLT                                                   
         MVC   LASTMED,NSRMED                                                   
*                                                                               
         CLI   QCLT,C'$'           OFFICE LIST SECURITY                         
         BNE   NPR60                                                            
*                                                                               
*        READ CLIENT FOR COFFICE                                                
*                                                                               
         BAS   RE,AMBIT            SET RIGHT MEDIA BIT IN BAGYMD                
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY+1(1),BAGYMD                                                  
         GOTO1 CLPACK,DMCB,NSRCLT,KEY+2                                         
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 GETCLT                                                           
         L     R2,ADCLT                                                         
         USING CLTRECD,R2                                                       
*                                                                               
         L     R3,ADCONLST                                                      
         USING SPADCONS,R3                                                      
         LA    R1,DUB                                                           
         USING OFFICED,R1                                                       
         XC    DUB(OFCLENQ),DUB                                                 
         MVI   OFCSYS,C'S'                                                      
         MVC   OFCAUTH,QCLT                                                     
         MVC   OFCLMT(3),QCLT                                                   
         MVC   OFCAGY,QAGY                                                      
         MVC   OFCOFC,COFFICE                                                   
         DROP  R1,R2                                                            
         MVI   CLTSEC,C'N'         DEFAULT TO NOT PRINTING CLT                  
         GOTO1 VOFFICER,DMCB,DUB,ACOMFACS                                       
         CLI   0(R1),0             TEST INCLUDE THIS CLIENT                     
         BNE   NPR10               NO                                           
         MVI   CLTSEC,C'Y'         SET TO PRINT THIS CLIENT                     
         DROP  R3                                                               
*                                                                               
NPR60    CLI   CLTSEC,C'Y'         PRINT CLIENT                                 
         BNE   NPR10               SKIP                                         
         MVC   NPRMED,NSRMED                                                    
         MVC   NPRCLT,NSRCLT                                                    
         MVC   NPRPRD,NSRPRD                                                    
         MVC   NPRPRD2,NSRPRD2                                                  
         MVC   NPRPAK,NSRPAK                                                    
         MVC   NPREST,NSREST                                                    
         MVC   NPRSTA,NSRSTA                                                    
         MVC   NPRSREP,NSRSREP                                                  
NPR80    MVC   NPRMONTH,NSRMONTH                                                
         MVC   NPRINVCE,NSRINV                                                  
         MVC   NPROPT,NSROPT                                                    
*                                                                               
         GOTO1 REPORT                                                           
*                                                                               
         CLC   NSRERROR,SPACES                                                  
         BNH   NPR100                                                           
         MVC   NPROPT(L'NSRERROR),NSRERROR                                      
         B     NPR200                                                           
*                                                                               
NPR100   LA    R2,ERRTABLE                                                      
NPR110   CLC   =X'FFFF',0(R2)                                                   
         BE    NPR200                                                           
         MVC   BYTE,NSRERR                                                      
         NC    BYTE,0(R2)          ONLY KEEP BIT WE ARE TESTING FOR             
         CLC   BYTE,0(R2)          IS THAT BIT ON                               
         BE    NPR130                                                           
NPR120   LA    R2,ERRTABLN(R2)                                                  
         B     NPR110                                                           
NPR130   MVC   NPROPT(30),1(R2)                                                 
         GOTO1 REPORT                                                           
         B     NPR120                                                           
*                                                                               
NPR200   GOTO1 REPORT                                                           
         B     NPR10                                                            
NPRX     XIT1                                                                   
*                                                                               
MYHEADN  NTR1                      HEADLINES                                    
         MVC   H2+10(3),NSRCLT                                                  
MYHNX    XIT1                                                                   
         DROP  R4                                                               
**********************************************************************          
*        PRINT REPORT FROM SORTER RECORDS                                       
**********************************************************************          
*                                                                               
PRINTIT  NTR1                                                                   
         MVI   RCSUBPRG,1                                                       
         XC    LASTCLT,LASTCLT                                                  
         XC    LASTMED,LASTMED                                                  
         MVI   CLTSEC,C'N'         DEFAULT TO NOT SEEING FOR                    
         CLI   QCLT,C'$'           OFFICE LIST SECURITY                         
         BE    *+8                                                              
         MVI   CLTSEC,C'Y'         EVERYONE ELSE GETS ALL CLTS                  
PR10     GOTO1 =V(SORTER),DMCB,=C'GET'                                          
         ICM   R4,15,4(R1)                                                      
         BZ    PRX                                                              
         MVC   IO2(SRLENQ),0(R4)                                                
         LA    R4,IO2                                                           
         USING SORTRECD,R4                                                      
         LA    R5,P1                                                            
         USING PRINTD,R5                                                        
*                                                                               
         CLC   SRCLT,LASTCLT                                                    
         BNE   PR20                                                             
         CLC   SRMED,LASTMED                                                    
         BE    PR60                                                             
PR20     MVI   FORCEHED,C'Y'                                                    
         MVC   LASTCLT,SRCLT                                                    
         MVC   LASTMED,SRMED                                                    
*                                                                               
         CLI   QCLT,C'$'           OFFICE LIST SECURITY                         
         BNE   PR60                                                             
*                                                                               
*        READ CLIENT FOR COFFICE                                                
*                                                                               
         BAS   RE,AMBIT            SET RIGHT MEDIA BIT IN BAGYMD                
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY+1(1),BAGYMD                                                  
         GOTO1 CLPACK,DMCB,SRCLT,KEY+2                                          
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 GETCLT                                                           
         L     R2,ADCLT                                                         
         USING CLTRECD,R2                                                       
*                                                                               
         L     R3,ADCONLST                                                      
         USING SPADCONS,R3                                                      
         LA    R1,DUB                                                           
         USING OFFICED,R1                                                       
         XC    DUB(OFCLENQ),DUB                                                 
         MVI   OFCSYS,C'S'                                                      
         MVC   OFCAUTH,QCLT                                                     
         MVC   OFCLMT(3),QCLT                                                   
         MVC   OFCAGY,QAGY                                                      
         MVC   OFCOFC,COFFICE                                                   
         DROP  R1,R2                                                            
         MVI   CLTSEC,C'N'         DEFAULT TO NOT PRINTING CLT                  
         GOTO1 VOFFICER,DMCB,DUB,ACOMFACS                                       
         CLI   0(R1),0             TEST INCLUDE THIS CLIENT                     
         BNE   PR10                NO                                           
         MVI   CLTSEC,C'Y'         SET TO PRINT THIS CLIENT                     
         DROP  R3                                                               
*                                                                               
PR60     CLI   CLTSEC,C'Y'         PRINT CLIENT                                 
         BNE   PR10                SKIP                                         
         MVC   PRMED,SRMED                                                      
         MVC   PRCLT,SRCLT                                                      
         MVC   PRPRD,SRPRD                                                      
         MVC   PRPRD2,SRPRD2                                                    
         MVC   PREST,SREST                                                      
         MVC   PRSTA,SRSTAT                                                     
         MVC   PRSREP,SRSREP                                                    
         CLC   =C'CK',QAGY         COKE = USE ACN                               
         BNE   PR80                                                             
         MVC   PRACN,SRACN                                                      
PR80     MVC   PRMONTH,SRMONTH                                                  
         MVC   PRINVCE,SRINVCE                                                  
         CLC   SRERROR,SPACES                                                   
         BNH   PR100                                                            
         MVC   PRERROR,SRERROR                                                  
         B     PR200                                                            
*                                                                               
PR100    LA    R2,ERRTABLE                                                      
PR110    CLC   =X'FFFF',0(R2)                                                   
         BE    PR200                                                            
         MVC   BYTE,SRERR                                                       
         NC    BYTE,0(R2)          ONLY KEEP BIT WE ARE TESTING FOR             
         CLC   BYTE,0(R2)          IS THAT BIT ON                               
         BE    PR130                                                            
PR120    LA    R2,ERRTABLN(R2)                                                  
         B     PR110                                                            
PR130    MVC   PRERROR(30),1(R2)                                                
         GOTO1 REPORT                                                           
         B     PR120                                                            
*                                                                               
PR200    GOTO1 REPORT                                                           
         B     PR10                                                             
PRX      XIT1                                                                   
*                                                                               
MYHEAD   NTR1                      HEADLINES                                    
         LA    R4,IO2                                                           
         USING SORTRECD,R4                                                      
         MVC   H2+10(3),SRCLT                                                   
         CLC   QAGY,=C'CK'                                                      
         BNE   MYHX                                                             
         MVC   H7+39(11),=C'STN   ACN  '                                        
         MVC   H8+39(11),=C'----- -----'                                        
         MVC   H7+51(4),=C'MKT '                                                
MYHX     XIT1                                                                   
         DROP  R5                                                               
*                                                                               
         EJECT                                                                  
**********************************************************************          
*        TURN ON RIGHT MEDIA BIT IN BAGYMD                                      
**********************************************************************          
*                                                                               
AMBIT    NTR1                                                                   
         NI    BAGYMD,X'F0'        KEEP AGY                                     
         LA    R5,MEDTAB                                                        
AMB10    CLC   SRMED,0(R5)                                                      
         BE    AMB20                                                            
         LA    R5,MEDTABLQ(R5)     NEXT ITEM IN MEDIA TABLE                     
         CLI   1(R5),X'FF'         END OF TABLE?                                
         BNE   AMB10                                                            
         OI    BAGYMD,X'01'        DEFAULT TO T                                 
         B     AMBX                                                             
AMB20    MVC   BYTE,1(R5)                                                       
         OC    BAGYMD,BYTE         TURN ON CORRECT MEDIA BIT                    
AMBX     XIT1                                                                   
*                                                                               
         DROP  R4                                                               
MEDTAB   DS    0X                                                               
         DC    CL1'T',XL1'01'                                                   
MEDTABLQ EQU   *-MEDTAB                                                         
         DC    CL1'R',XL1'02'                                                   
         DC    CL1'N',XL1'03'                                                   
         DC    CL1'X',XL1'04'                                                   
         DC    CL1'C',XL1'08'                                                   
         DC    X'FF'                                                            
         EJECT                                                                  
**********************************************************************          
*        ERROR TABLE AND STUFF                                                  
**********************************************************************          
*                                                                               
ERRTABLE DS    0H                                                               
         DC    AL1(APYERRS_NOMATCH)                                             
         DC    CL30'UNSUCCESSFUL MATCH'                                         
ERRTABLN EQU   *-ERRTABLE                                                       
         DC    AL1(APYERRS_FILMERR)                                             
         DC    CL30'FILM ANALYSIS ERROR'                                        
         DC    AL1(APYERRS_SEPERR)                                              
         DC    CL30'SECONDARY SEPARATION ERROR'                                 
         DC    AL1(APYERRS_HVRERR)                                              
         DC    CL30'HORIZ/VERT ROTATION ERROR'                                  
         DC    X'FFFF'                                                          
*                                                                               
         GETEL R5,24,ELCODE                                                     
         GETEL2 R5,42,ELCODE                                                    
**********************************************************************          
*        LTORG                                                                  
**********************************************************************          
         LTORG                                                                  
CTFLIST  DS    0F                                                               
         DC    CL8'NGENDIR'                                                     
         DC    CL8'NGENFIL'                                                     
         DC    CL8'NCTFILE'                                                     
         DC    C'X'                                                             
DINDEX   DC    CL8'INDEX'                                                       
DREAD    DC    CL8'READ'                                                        
DCLOSE   DC    CL8'CLOSE'                                                       
WRKF     DC    CL8'WRKF'                                                        
WRKFILE  DC    CL8'WRKFILE'                                                     
DAYS     DC    CL7'MTWTFSS'                                                     
AWKBUFF  DC    A(WKBUFF)                                                        
*                                                                               
SORTCARD DC    CL80'SORT FIELDS=(1,10,A),FORMAT=BI,WORK=1'                      
RECCARD  DC    CL80'RECORD TYPE=F,LENGTH=150'                                   
SORTCAR2 DC    CL80'SORT FIELDS=(1,10,A),FORMAT=BI,WORK=1'                      
RECCARD2 DC    CL80'RECORD TYPE=F,LENGTH=168'                                   
*                                                                               
IOWORK   DS    12D                 IO WORK AREA                                 
ELCODE   DS    X                                                                
SVSPUTL  DS    X                                                                
FLAG     DS    X                                                                
NEWRKR   EQU   X'80'               FIRST ERROR HAS BEEN FOUND                   
TMSTMP   EQU   X'40'               FOUND TIME STAMP                             
XAUTPAY  EQU   X'20'               AUTOPAY ON THE XSPFIL                        
*                                                                               
CLTSEC   DS    CL1                 Y=OK CLT                                     
BYTE2    DS    XL1                                                              
ERRNO    DS    XL2                 ERROR NUMBER                                 
CTDAY    DS    XL2                 COMP TODAY BASE 1900                         
CTDAY64  DS    XL2                 COMP TODAY BASE 1964                         
SVDATE   DS    XL2                 DATE OF WRKR FILE (IN AUTOPAY KEY)           
WRKRID   DS    XL2                                                              
MULTMONS DS    XL1                 MULTIPLE MONTHS?                             
TEMPDATE DS    CL6                                                              
TEMPDAT2 DS    CL6                                                              
SVTMSTMP DS    CL13                SAVE TIME STAMP ON UPLOAD                    
LASTCLT  DS    CL3                                                              
LASTMED  DS    CL1                                                              
WRKRINDX DS    CL42                                                             
GTKEY    DS    XL44                                                             
GTKEYSVE DS    XL44                                                             
IO       DS    XL255               IO AREA                                      
IO2      DS    XL1200              IO AREA                                      
WKBUFF   DS    14336C                                                           
*                                                                               
***********************************************************************         
*        WORKER FILE DSECT                                                      
***********************************************************************         
WRECD    DSECT                                                                  
WRHDR    DS    CL20                HEADER                                       
WRPAYER  DS    CL12                PAYER                                        
WRACN    DS    CL5                 ACN NUM FOR COKE                             
WRMEDIA  DS    CL1                 MEDIA                                        
WRCLIENT DS    CL3                 CLIENT                                       
WRPRD    DS    CL3                 PRODUCT                                      
WRPRD2   DS    CL3                 PARTNER                                      
WREST    DS    CL3                 ESTIMATE                                     
WRSTAT   DS    CL10                STATION                                      
WRSREP   DS    CL4                 SPECIAL REP                                  
WRMONTH  DS    CL6                 MONTH (MMM/YY)                               
WRINVCE  DS    CL12                INVOICE TRACKING                             
WRLENQ   EQU   *-WRECD                                                          
***********************************************************************         
*        XSPFIL WORKER FILE DSECT                                               
***********************************************************************         
WRECDX   DSECT                                                                  
WRHDRX   DS    CL20                HEADER                                       
WRPAYERX DS    CL12                PAYER                                        
WRMEDIAX DS    CL1                 MEDIA                                        
WRCLTX   DS    CL3                 CLIENT                                       
WRPRDX   DS    CL3                 PRODUCT                                      
WRPRD2X  DS    CL3                 PARTNER                                      
WRESTX   DS    CL3                 ESTIMATE                                     
WRSTATX  DS    CL10                STATION                                      
WRSREPX  DS    CL4                 "S" BEFORE REP CODE                          
WRMONTHX DS    CL6                 MONTH (MMM/YY)                               
WRLENQX  EQU   *-WRECDX            LENGTH WITHOUT INVOICE ENTRIES               
WRINVNUM DS    CL10                INVOICE NUMBER                               
WRINVAMT DS    CL11                INVOICE AMOUNT                               
WRLEN2Q  EQU   *-WRINVNUM          LENGTH OF INVOICE ENTRY                      
***********************************************************************         
*        NET WORKER FILE DSECT                                                  
***********************************************************************         
NWRECD   DSECT                                                                  
NWRHDR   DS    CL20                HEADER                                       
NWRMSG   DS    CL60                MESSAGE ERRORS/SUCCESS                       
NWRMED   DS    CL1                 MEDIA                                        
NWRCLI   DS    CL3                 CLIENT                                       
NWRSTA   DS    CL4                 STATION                                      
NWRSREP  DS    CL4                 SPECIAL REP                                  
NWRMONTH DS    CL20                MONTH (MMM/YY)                               
NWROPT   DS    CL65                OPTIONS                                      
NWRINV1  DS    CL10                INVOICE #                                    
NWRINV1$ DS    CL12                INVOICE DOLLARS                              
NWRLENQ  EQU   *-NWRECD                                                         
*                                                                               
***********************************************************************         
*        SORT RECORD DSECT                                                      
***********************************************************************         
SORTRECD DSECT                                                                  
SRCLT    DS    CL3                 CLIENT                                       
SRMED    DS    CL1                 MEDIA                                        
SRPRD    DS    CL3                 PRODUCT                                      
SRPRD2   DS    CL3                 PARTNER                                      
SREST    DS    CL3                 ESTIMATE                                     
SRSTAT   DS    CL10                STATION                                      
SRACN    DS    CL5                 ACN NUMBER FOR COKE                          
SRSREP   DS    CL4                 SPECIAL REP                                  
SRMONTH  DS    CL6                 MONTH (MMM/YY)                               
SRINVCE  DS    CL12                INVOICE TRACKING                             
SRERR    DS    XL1                 ERROR BYTE - FROM APY RECORD                 
SRERROR  DS    CL50                ERROR TEXT -                                 
SRLENQ   EQU   *-SORTRECD                                                       
*                                                                               
***********************************************************************         
*        NET SORT RECORD DSECT                                                  
***********************************************************************         
NSRTRECD DSECT                                                                  
NSRMED   DS    CL1                 MEDIA                                        
NSRCLT   DS    CL3                 CLIENT                                       
NSRPRD   DS    CL3                 PRODUCT                                      
NSRPRD2  DS    CL3                 PARTNER                                      
NSRPAK   DS    CL3                 PACKAGE                                      
NSREST   DS    CL3                 ESTIMATE                                     
NSRSTA   DS    CL4                 STATION                                      
NSRSREP  DS    CL4                 SPECIAL REP                                  
NSRMONTH DS    CL8                 MONTH (MMM/YY)                               
NSRINV   DS    CL10                INVOICE TRACKING                             
NSRERR   DS    XL1                 ERROR BYTE - FROM APY RECORD                 
NSRERROR DS    CL60                ERROR TEXT -                                 
NSROPT   DS    CL65                OPTIONS                                      
NSRLENQ  EQU   *-NSRTRECD                                                       
*                                                                               
***********************************************************************         
*        PRINT LINE  DSECT                                                      
***********************************************************************         
PRINTD   DSECT                                                                  
PRSEQ    DS    CL6                                                              
         DS    CL4                                                              
PRMED    DS    CL1                                                              
         DS    CL4                                                              
PRCLT    DS    CL3                                                              
         DS    CL3                                                              
PRPRD    DS    CL3                                                              
         DS    CL3                                                              
PRPRD2   DS    CL3                                                              
         DS    CL3                                                              
PREST    DS    CL3                                                              
         DS    CL3                                                              
PRSTA    DS    CL5                                                              
         DS    CL1                                                              
PRACN    DS    CL5                 ACN NUMBER FOR COKE                          
         DS    CL1                                                              
PRSREP   DS    CL4                 SPECIAL REP                                  
         DS    CL2                                                              
PRMONTH  DS    CL6                 MONTH (MMM/YY)                               
         DS    CL2                                                              
PRINVCE  DS    CL12                INVOICE TRACKING                             
         DS    CL2                                                              
PRERROR  DS    CL50                ERROR MESSAGE                                
*                                                                               
***********************************************************************         
*        PRINT LINE DSECT                                                       
***********************************************************************         
NPRINTD  DSECT                                                                  
         DS    CL1                                                              
NPRMED   DS    CL1                                                              
         DS    CL3                                                              
NPRCLT   DS    CL3                                                              
         DS    CL3                                                              
NPRPRD   DS    CL3                                                              
         DS    CL3                                                              
NPRPRD2  DS    CL3                                                              
         DS    CL3                                                              
NPRPAK   DS    CL3                                                              
         DS    CL3                                                              
NPREST   DS    CL3                                                              
         DS    CL3                                                              
NPRSTA   DS    CL5                                                              
         DS    CL1                                                              
NPRSREP  DS    CL4                 SPECIAL REP                                  
         DS    CL2                                                              
NPRMONTH DS    CL8                 MONTH (MMM/YY)                               
         DS    CL2                                                              
NPRINVCE DS    CL10                INVOICE TRACKING                             
         DS    CL2                                                              
NPROPT   DS    CL63                OPTIONS/ERROR MESSAGE                        
*                                                                               
         EJECT                                                                  
                                                                                
         PRINT OFF                                                              
       ++INCLUDE FAGETTXTD                                                      
       ++INCLUDE GEGENMSG                                                       
       ++INCLUDE SPREPMODES                                                     
       ++INCLUDE SPREPWORKD                                                     
       ++INCLUDE DMWRKFL                                                        
       ++INCLUDE DMWRKFK                                                        
       ++INCLUDE CTGENFILE                                                      
       ++INCLUDE SPGENAPY                                                       
       ++INCLUDE NEGENAPY                                                       
CLTRECD  DSECT                                                                  
       ++INCLUDE SPGENCLT                                                       
       ++INCLUDE DDOFFICED                                                      
       ++INCLUDE DDMASTD                                                        
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'073SPREPIW02 08/03/20'                                      
         END                                                                    
