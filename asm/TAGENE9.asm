*          DATA SET TAGENE9    AT LEVEL 012 AS OF 01/12/10                      
*PHASE T702E9A,*                                                                
         TITLE 'T702E9 - CHECK STOP'                                            
T702E9   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T702E9,R7                                                      
         SPACE 1                                                                
         L     RC,0(R1)            RC=GENCON STORAGE AREA                       
         USING GEND,RC                                                          
         L     RA,ATWA             RA=A(TWA)                                    
         USING T702FFD,RA                                                       
         L     R9,ASYSD            R9=ROOT STORAGE AREA                         
         USING SYSD,R9                                                          
         EJECT                                                                  
* VALIDATE THE CHECK KEY AND READ THE CHECK RECORD INTO AIO1                    
         SPACE                                                                  
         TM    TGSYSTAT,TASYSPID ARE WE USING PID NUM                           
         BZ    MODE05                                                           
         MVC   STPSHED(7),=C'PID NUM'                                           
         OI    STPSHEDH+6,X'80'                                                 
MODE05   CLI   MODE,VALKEY                                                      
         BNE   MODE10                                                           
         BAS   RE,VKEY                                                          
         SPACE                                                                  
* TURN ON PROSDPF BIT FIRST TIME INTO CHECK STOP SCREEN SO                      
* CHECK INFORMATION DISPLAYED BEFORE ASKING TO CHANGE                           
* (IF AGENCY IS PRESENT WE KNOW IT HAS BEEN DISPLAYED)                          
         SPACE                                                                  
MODE10   NI    PROSTAT,X'FF'-PROSDPF                                            
         TM    STPAGYNH+4,X'20'                                                 
         BO    MODE20                                                           
         OI    PROSTAT,PROSDPF                                                  
         BAS   RE,DREC                                                          
         SPACE                                                                  
* TURN ON PROWARN IF USER HAS CHANGED SOMETHING ON SCREEN                       
* AND HIT PFKEY13 BEFORE SAVING  (ONLY GIVE WARNING ONCE)                       
         SPACE                                                                  
MODE20   CLI   PFAID,13                                                         
         BNE   MODE30                                                           
         TM    PROSTAT,PROWARN                                                  
         BO    MODE30                                                           
         GOTO1 FLDVAL,DMCB,(X'40',STPSSNH),(X'80',999)                          
         BE    MODE30                                                           
         OI    PROSTAT,PROWARN                                                  
         MVI   MODE,VALREC                                                      
         B     MODE50                                                           
         SPACE                                                                  
* IF NO WARNING NECESSARY AND IF USER CAME FROM SOMWHERE                        
* OTHER THAN CHECK SLIST  ...  SEE IF USER WANTS TO GO TO                       
* CHECK DISPLAY                                                                 
         SPACE                                                                  
MODE30   NI    PROSTAT,X'FF'-PROWARN                                            
         CLI   THISLSEL,STOPL2M                                                 
         BE    MODE40                                                           
         GOTO1 INITIAL,DMCB,(X'40',PFTAB)                                       
         B     MODE50                                                           
         SPACE                                                                  
* IF NO WARNING NECESSARY, CHECK TO SEE IF USER CAME FROM                       
* CHECK SLIST, IF DONE HERE AND NOT REQUESTING CHECK DISPLAY                    
* GO BACK TO CHECK SLIST                                                        
         SPACE                                                                  
MODE40   GOTO1 FLDVAL,DMCB,(X'40',STPSSNH),(X'80',999)                          
         BNE   MODE45                                                           
         CLI   PFAID,0                                                          
         BNE   MODE45                                                           
         MVI   PFAID,18                                                         
         MVI   THISLSEL,0                                                       
         OI    TRNSTAT,OKINTPFK                                                 
         SPACE                                                                  
* IF NO WARNING NECESSARY, CHECK TO SEE IF USER CAME FROM                       
* CHECK SLIST, IF DONE HERE AND REQUESTING CHECK DISPLAY                        
* GO TO CHECK DISPLAY                                                           
         SPACE                                                                  
MODE45   GOTO1 INITIAL,DMCB,(X'40',LISTTAB)                                     
         SPACE                                                                  
* VALIDATE AND DISPLAY THE RECORD                                               
         SPACE                                                                  
MODE50   CLI   MODE,VALREC                                                      
         BNE   *+12                                                             
         BAS   RE,VREC                                                          
         BAS   RE,DREC                                                          
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO HANDLE MODE VALKEY.                                   
         SPACE                                                                  
VKEY     NTR1                                                                   
         SPACE                                                                  
         BAS   RE,PFINVIS          MAKE ALL PF KEYS INVISIBLE                   
         SPACE                                                                  
         LA    R2,STPCHKH          IF KEY ALREADY VALIDATED                     
         TM    4(R2),X'20'         JUST READ CHECK RECORD AND                   
         BO    VK20                EXIT                                         
         SPACE                                                                  
         LA    RE,CONHEADH         FIND END OF SCREEN                           
         XR    RF,RF                                                            
         ICM   RF,1,0(RE)                                                       
         BZ    *+10                                                             
         AR    RE,RF                                                            
         B     *-10                                                             
         LA    RE,3(RE)            BUMP PAST CONTROL BYTES                      
         LR    RF,RE                                                            
         SR    RF,RA                                                            
         SH    RF,=AL2(3520+64)    L'AVAIL TWA0 AS DEFINED IN DDGENTWA          
         LCR   RF,RF                                                            
         XCEFL ,                   CLEAR AREA AFTER SCREEN END                  
         SPACE 1                                                                
         CLI   5(R2),0             IF NO CHECK INPUT                            
         BNE   VK10                                                             
         OC    TGCHK,TGCHK         CHECK FOR GLOBAL NUMBER                      
         BZ    VK10                                                             
         MVC   STPCHK,TGCHK        AND USE IT                                   
         OI    6(R2),X'80'                                                      
         MVI   5(R2),L'TGCHK                                                    
         SPACE                                                                  
VK10     GOTO1 ANY                                                              
         CLI   5(R2),L'STPCHK      INSURE FULL CHECK NUMBER INPUT               
         BNE   ERRINV                                                           
         MVC   TGCHK,WORK          SAVE UNCOMPLEMENTED IN GLOBAL                
         SPACE                                                                  
         GOTO1 READCK,DMCB,(C'Y',0) READ THE CHECK RECORD                       
         SPACE                                                                  
         BAS   RE,LIMITCHK         CHECK LIMIT ACCESS                           
         BNE   ERNOFND                                                          
         SPACE                                                                  
         NI    STPAGYNH+4,X'FF'-X'20'                                           
         OI    STPCHKH+4,X'20'                                                  
         B     VK30                                                             
         SPACE                                                                  
VK20     GOTO1 READCK,DMCB,(C'Y',0)  READ CHECK RECORD                          
VK30     BAS   RE,PFVIS            MARK AND CHECK VALID PF KEYS                 
         SPACE                                                                  
VKX      B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO CHECK AGENCY AND CLIENT LIMIT ACCESS                  
         SPACE 1                                                                
LIMITCHK NTR1                                                                   
         LHI   R2,1                                                             
*                                                                               
         USING TLCKD,R4                                                         
         L     R4,AIO                                                           
         MVC   CHKAGY,TLCKAGY      INITIALIZE CHECK'S AGENCY                    
         XC    CHKCLI,CHKCLI       AND CLIENT                                   
         DROP  R4                                                               
*                                                                               
         USING TAOID,R4                                                         
         MVI   ELCODE,TAOIELQ      SAVE CHECK'S AGENCY                          
         BRAS  RE,GETEL                                                         
         BNE   LIMCHK10                                                         
         MVC   CHKAGY,TAOIAGY                                                   
         DROP  R4                                                               
*                                                                               
         USING TAPDD,R4                                                         
LIMCHK10 L     R4,AIO              CHECK CLIENT LIMIT ACCESS                    
         MVI   ELCODE,TAPDELQ                                                   
         BAS   RE,GETEL                                                         
         BNE   LIMCHK20                                                         
         MVC   CHKCLI,TAPDCLI                                                   
         DROP  R4                                                               
*                                                                               
         USING FAWSSVRD,R1                                                      
LIMCHK20 LA    R1,LIMBLK                                                        
         MVC   FAWSTOKN(3),=C'STF'                                              
         STC   R2,FAWSTOKN+3                                                    
         MVI   FAWSACTN,FAWSARST   RECALL STAFF2 INFORMATION VIA WWSVR          
         XC    FAWSLEN,FAWSLEN                                                  
         MVC   FAWSADR,TGAS2ACC                                                 
         GOTO1 WSSVR,(R1)                                                       
         CLI   FAWSRTN,0           IF NOT FOUND, STAFF HAS NO ACCESS            
         BNE   NO                                                               
         DROP  R1                                                               
*                                                                               
         AHI   R2,1                                                             
*                                                                               
         USING TAVAD,R1                                                         
         L     R1,TGAS2ACC                                                      
         OC    TAVAAGY,TAVAAGY     IF STAFF HAS NO AGENCY LIMITS,               
         BZ    YES                 STAFF HAS ACCESS TO ALL RECORDS              
*                                                                               
LIMCHK30 CLI   0(R1),0             RECALL NEXT RECORD FROM WSSVR                
         BE    LIMCHK20                                                         
*                                                                               
         CLC   CHKAGY,TAVAAGY      IF AGENCY IS FOUND IN STAFF LIMITS           
         BNE   LIMCHK50                                                         
*                                                                               
         CLI   TAVALEN,TAVALNQ     IF NO CLIENT LIMITS ARE DEFINED              
         BE    YES                 ACCESS IS GRANTED                            
*                                                                               
         ZIC   RE,TAVALEN                                                       
         SHI   RE,TAVALNQ                                                       
         LA    RF,TAVACLI                                                       
LIMCHK40 CLC   CHKCLI,0(RF)        IF CLIENT IS FOUND IN STAFF LIMITS           
         BE    YES                 ACCESS IS GRANTED                            
         LA    RF,L'TAVACLI(RF)                                                 
         SHI   RE,L'TAVACLI                                                     
         LTR   RE,RE                                                            
         BNZ   LIMCHK40                                                         
*                                                                               
LIMCHK50 ZIC   RE,TAVALEN          BUMP TO NEXT VALID AGENCY/CLIENT             
         AR    R1,RE               ELEMENT                                      
         B     LIMCHK30                                                         
         DROP  R1                                                               
*                                                                               
CHKAGY   DS    CL6                 CHECK'S AGENCY                               
CHKCLI   DS    CL6                 CHECK'S CLIENT                               
LIMBLK   DS    XL100               AGENCY/CLIENT LIMIT WORK BLOCK               
         EJECT                                                                  
*              MAKE ALL PF KEYS INVISIBLE AND TRANSMITTED                       
         SPACE 1                                                                
PFINVIS  NTR1                                                                   
         NI    STPPFAH+1,X'FF'-X'08'                                            
         NI    STPPFBH+1,X'FF'-X'08'                                            
         NI    STPPFCH+1,X'FF'-X'08'                                            
         OI    STPPFAH+1,X'0C'                                                  
         OI    STPPFBH+1,X'0C'                                                  
         OI    STPPFCH+1,X'0C'                                                  
         OI    STPPFAH+6,X'80'                                                  
         OI    STPPFBH+6,X'80'                                                  
         OI    STPPFCH+6,X'80'                                                  
         B     XIT                                                              
         EJECT                                                                  
*              MAKE PF KEYS VISIBLE BASED ON CHECK DATA                         
         SPACE 1                                                                
         USING TAKPD,R4                                                         
PFVIS    NTR1                                                                   
         OI    STPPF13H+6,X'80'                                                 
         MVC   STPPF13,=CL18'PF13=Check Display'                                
         SPACE                                                                  
         USING TACND,R4                                                         
         XC    CANCELBY(11),CANCELBY                                            
         L     R4,AIO             IF CHECK STOP CANCELLED                       
         MVI   ELCODE,TACNELQ     SAVE WHO CANCELLED IT AND WHEN                
         GOTO1 GETL,DMCB,(1,=AL1(TACNTYPS))                                     
         BNE   PFV05                                                            
         L     R4,TGELEM                                                        
         MVC   CANCELBY,TACNBY                                                  
         MVC   CANCELDT,TACNDT                                                  
         DROP  R4                                                               
         SPACE                                                                  
         USING TAKPD,R4                                                         
PFV05    L     R4,AIO                                                           
         MVI   ELCODE,TAKPELQ      TRY TO GET CHECK PULL ELEMENT                
         BAS   RE,GETEL                                                         
         BE    PFV10                                                            
         SPACE                                                                  
         NI    STPPFAH+1,X'FF'-X'0C'                                            
         OI    STPPFAH+1,X'08'                                                  
         MVC   STPPFA,=CL19'14=Add Request'                                     
         CLI   PFAID,0             IF CHECK PULL ELEMENT NOT                    
         BE    PFVX                YET ADDED                                    
         CLI   PFAID,13            ONLY VALID PF KEYS ARE 13                    
         BE    *+12                AND 14                                       
         CLI   PFAID,14                                                         
         BNE   ERRPFK                                                           
         B     PFVX                                                             
         SPACE                                                                  
PFV10    OC    CANCELBY(11),CANCELBY                                            
         BZ    PFV11                                                            
         CLI   PFAID,0             IF CHECK STOP REQUEST IS                     
         BE    PFVX                CANCELLED                                    
         CLI   PFAID,13            ONLY VALID PF KEY IS 13                      
         BE    PFVX                                                             
         B     ERRPFK                                                           
         SPACE                                                                  
PFV11    NI    STPPFCH+1,X'FF'-X'0C'                                            
         OI    STPPFCH+1,X'08'                                                  
         MVC   STPPFC,=CL19'17=Cancel Stop'                                     
         SPACE                                                                  
         OC    TAKPAPRB,TAKPAPRB                                                
         BNZ   PFV20                                                            
         NI    STPPFAH+1,X'FF'-X'0C'                                            
         OI    STPPFAH+1,X'08'                                                  
         MVC   STPPFA,=CL19'15=Change Request'                                  
         CLI   TGCTSTTY,TASTTYPP                                                
         BE    PFV12                                                            
         CLI   TGCTSTTY,TASTTYPA                                                
         BE    PFV12                                                            
         CLI   TGCTSTTY,TASTTYPB                                                
         BNE   PFV13                                                            
PFV12    NI    STPPFBH+1,X'FF'-X'0C'                                            
         OI    STPPFBH+1,X'08'                                                  
         MVC   STPPFB,=CL19'16=Appr for Reissue'                                
PFV13    CLI   PFAID,14            IF CHECK PULL ELEMENT IS                     
         BE    ERRPFK              ADDED BUT NOT YET APPROVED                   
         CLI   PFAID,16            PF KEY 14 IS INVALID                         
         BNE   PFVX                                                             
         CLI   TGCTSTTY,TASTTYPP   PF KEY 16 IS ONLY VALID IF                   
         BE    PFVX                STAFF MEMBER IS PROGRAMMER                   
         CLI   TGCTSTTY,TASTTYPA   OR TYPE A OR B                               
         BE    PFVX                                                             
         CLI   TGCTSTTY,TASTTYPB                                                
         BNE   ERRPFKE                                                          
         B     PFVX                                                             
         SPACE                                                                  
PFV20    NI    STPPFAH+1,X'FF'-X'0C'                                            
         OI    STPPFAH+1,X'08'                                                  
         MVC   STPPFA,=CL19'15=Change Request'                                  
         CLI   PFAID,14            IF CHECK STOP ELEMENT IS                     
         BE    ERRPFK              ADDED AND APPROVED                           
PFVX     B     XIT                 ONLY PFK 14 IS INVALID                       
         EJECT                                                                  
*              ROUTINE TO DISPLAY CHECK RECORD                                  
*                                                                               
DREC     NTR1                                                                   
         GOTO1 FLDVAL,DMCB,(X'01',STPSSNH),(X'80',999)                          
         SPACE                                                                  
         MVC   SVSTPB,SPACES      CLEAR STOP PLACED SAVED                       
         MVC   SVSTPD,SPACES      FIELDS                                        
         SPACE                                                                  
         USING TLCKD,R4                                                         
         L     R4,AIO                                                           
         MVC   STPAGY,TLCKAGY    COPY AGENCY,SS# AND INVOICE#                   
         MVC   STPSSN,TLCKSSN    FROM KEY TO SCREEN                             
         GOTO1 TINVCON,DMCB,TLCKINV,STPINV,DATCON                               
         DROP  R4                                                               
         SPACE                                                                  
         USING TAOID,R4                                                         
         L     R4,AIO            IF OLD AGENCY/INVOICE ELEMENT                  
         MVI   ELCODE,TAOIELQ    EXISTS                                         
         BAS   RE,GETEL          USE THIS AGENCY AND INVOICE                    
         BNE   DREC05            INSTEAD                                        
         MVC   STPAGY,TAOIAGY                                                   
         GOTO1 TINVCON,DMCB,TAOIINV,STPINV,DATCON                               
         DROP  R4                                                               
         SPACE                                                                  
DREC05   BAS   RE,SETTAL         SET TO READ FROM TAL FILE                      
         MVC   AIO,AIO2          DISPLAY AGENCY AND SS# NAMES                   
         GOTO1 RECVAL,DMCB,TLAYCDQ,(X'8C',STPAGY),STPAGYNH                      
         GOTO1 RECVAL,DMCB,TLW4CDQ,(X'8C',STPSSN),STPSSNNH                      
*                                                                               
         TM    TGSYSTAT,TASYSPID   ARE WE USING PID#                            
         BZ    DREC06                                                           
         GOTO1 SSNPACK,DMCB,TGSSN,TGPID                                         
         MVC   STPSSN,SPACES                                                    
         MVC   STPSSN(L'TGPID),TGPID                                            
         MVI   STPSSNH+5,6                                                      
         OI    STPSSNH+6,X'80'                                                  
*                                                                               
DREC06   MVC   AIO,AIO1                                                         
         SPACE                                                                  
         USING TACDD,R4                                                         
         L     R4,AIO            R4=A(CHECK DETAILS ELEMENT)                    
         MVI   ELCODE,TACDELQ                                                   
         BAS   RE,GETEL          COPY CHECK DATE                                
         BNE   DREC10                                                           
         GOTO1 DATCON,DMCB,(1,TACDDTE),(8,STPCHKD)                              
         OC    TACDNET,TACDNET   AND NET AMOUNT TO SCREEN                       
         BZ    DREC08                                                           
         EDIT  TACDNET,(11,STPNET),2,MINUS=YES,ALIGN=LEFT                       
         SPACE                                                                  
DREC08   MVC   STPCSHD(3),=C'N/A'                                               
         OC    TACDCSH,TACDCSH                                                  
         BZ    DREC09                                                           
         GOTO1 DATCON,DMCB,(1,TACDCSH),(8,STPCSHD)                              
         DROP  R4                                                               
         SPACE                                                                  
         USING TAOKD,R4                                                         
DREC09   L     R4,AIO                                                           
         MVI   ELCODE,TAOKELQ                                                   
         BAS   RE,GETEL                                                         
         BNE   DREC10                                                           
*        GOTO1 DATCON,DMCB,(1,TAOKDTE),(8,STPCHKD)                              
         DROP  R4                                                               
         SPACE                                                                  
DREC10   GOTO1 TINVCON,DMCB,STPINV,INV,DATCON                                   
         CLI   0(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'00'             READ CHECK'S INVOICE RECORD                    
         XC    INV,ALLFF         INTO AIO2                                      
         MVC   AIO,AIO2                                                         
         GOTO1 RECVAL,DMCB,TLINCDQ,(X'A4',INV)                                  
         BNE   DREC20                                                           
         SPACE                                                                  
         USING TACOD,R4                                                         
         L     R4,AIO            R4=A(COMMERCIAL DETAILS ELEMENT)               
         MVI   ELCODE,TACOELQ    DISPLAY COMMERCIAL CID AND                     
         BAS   RE,GETEL          NAME                                           
         BNE   DREC20            AND GET COMMERCIAL RECORD INTO                 
         MVC   STPCID,TACOCID    AIO2                                           
         GOTO1 RECVAL,DMCB,TLCOICDQ,(X'A8',STPCID),STPCOMH                      
         BNE   DREC20                                                           
         DROP  R4                                                               
         SPACE                                                                  
         USING TLCOD,R4                                                         
         L     R4,AIO            SAVE CLIENT FROM COMMERCIAL                    
         MVC   TGCLI,TLCOCLI     GET PRODUCT RECORD AND DISPLAY                 
         MVC   STPPRD,TLCOPRD    PRODUCT NAME                                   
         GOTO1 RECVAL,DMCB,TLPRCDQ,(X'A8',STPPRD),STPPRNH                       
         SPACE                                                                  
         USING TAKPD,R4                                                         
DREC20   MVC   AIO,AIO1          GET STOP CHECK ELEMENT                         
         L     R4,AIO                                                           
         MVI   ELCODE,TAKPELQ                                                   
         BAS   RE,GETEL                                                         
         BNE   DREC30                                                           
         SPACE                                                                  
         TM    TAKPCSST,TAKPCSYS INDICATE IF CHECK SHOULD BE                    
         BZ    *+12              REISSUED OR NOT                                
         MVI   STPRYES,C'X'                                                     
         MVI   STPRYESH+5,1                                                     
         SPACE                                                                  
         TM    TAKPCSST,TAKPCSNO                                                
         BZ    *+12                                                             
         MVI   STPRNO,C'X'                                                      
         MVI   STPRNOH+5,1                                                      
         SPACE                                                                  
         TM    TAKPCSST,TAKPCSCL     INDICATE WHO REQUSTED                      
         BZ    *+12                  STOP PAY                                   
         MVI   STPCLI,C'X'                                                      
         MVI   STPCLIH+5,1                                                      
         SPACE                                                                  
         TM    TAKPCSST,TAKPCSAG                                                
         BZ    *+12                                                             
         MVI   STPAGT,C'X'                                                      
         MVI   STPAGTH+5,1                                                      
         SPACE                                                                  
         TM    TAKPCSST,TAKPCSTA                                                
         BZ    *+12                                                             
         MVI   STPTAL,C'X'                                                      
         MVI   STPTALH+5,1                                                      
         SPACE                                                                  
         TM    TAKPCSST,TAKPCSUN                                                
         BZ    *+12                                                             
         MVI   STPUNI,C'X'                                                      
         MVI   STPUNIH+5,1                                                      
         SPACE                                                                  
         MVC   STPCINV,TAKPCBIN  COPY CR/BNP INVOICE TO SCREEN                  
         SPACE                                                                  
         MVC   STPAPRE,TAKPAPRB  COPY APPROVED FOR REISSUE INFO                 
         GOTO1 DATCON,DMCB,(1,TAKPAPRD),(8,STPARDT)   TO SCREEN                 
         SPACE                                                                  
         MVC   STPRECK,TAKPRECN  COPY REISSUED CHECK # INFO TO                  
         GOTO1 DATCON,DMCB,(1,TAKPRECD),(8,STPRCKD)     SCREEN                  
         MVC   STPRCKB,TAKPRECB                                                 
         SPACE                                                                  
         MVC   STPRQMB,TAKPSRBY   COPY STOP PAY REQUEST INFO TO                 
         GOTO1 DATCON,DMCB,(1,TAKPSRDT),(8,STPRQMD)      SCREEN                 
         SPACE                                                                  
         MVC   STPSTPB,TAKPSPBY   COPY STOP PLACED INFO TO                      
         GOTO1 DATCON,DMCB,(1,TAKPSPDT),(8,STPSTPD) SCREEN                      
         DROP  R4                                                               
         SPACE                                                                  
         MVC   SVSTPB,STPSTPB     SAVE STOP PLACED INFO                         
         MVC   SVSTPD,STPSTPD                                                   
         SPACE                                                                  
         MVI   LSTCHGF,25                                                       
         GOTO1 ACTVOUT,DMCB,(X'80',LSTCHGF)   DISP LAST CHANGED                 
         MVC   STPLCHP,LSTCHGF+8                                                
         MVC   STPLCHD,LSTCHGF+17                                               
         SPACE                                                                  
         MVC   STPCANB,CANCELBY  DISPLAY CANCELLED BY AND DATE                  
         GOTO1 DATCON,DMCB,(1,CANCELDT),(8,STPCAND)                             
         SPACE                                                                  
         USING TAXCD,R4                                                         
DREC30   L     R4,AIO            FILL IN COMMENT FIELDS WITH                    
         MVI   ELCODE,TAXCELQ    WITH TAXC ELEMENT DATA                         
         BAS   RE,GETEL                                                         
         B     *+8                                                              
DREC40   BAS   RE,NEXTEL                                                        
         BNE   DRECX                                                            
         LA    R2,STPREASH                                                      
         CLI   TAXCSEQ,1                                                        
         BE    DREC50                                                           
         LA    R2,STPSPINH                                                      
         CLI   TAXCSEQ,2                                                        
         BE    DREC50                                                           
         LA    R2,STPCML1H                                                      
         CLI   TAXCSEQ,3                                                        
         BE    DREC50                                                           
         LA    R2,STPCML2H                                                      
         CLI   TAXCSEQ,4                                                        
         BNE   DREC40                                                           
DREC50   ZIC   RE,TAXCLEN                                                       
         SHI   RE,4                                                             
         STC   RE,5(R2)                                                         
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   8(0,R2),TAXCCMNT                                                 
         B     DREC40                                                           
         SPACE                                                                  
DRECX    GOTO1 FLDVAL,DMCB,(X'22',STPSSNH),999                                  
         SPACE                                                                  
         TM    PROSTAT,PROSDPF     IF FIRST TIME INTO CHECK STOP                
         BO    ENTCHG              SCREEN GIVE PROPER MESSAGE                   
         SPACE                                                                  
         CLI   MODE,VALREC         IF RECORD JUST CHANGED GIVE                  
         BE    CHKCHG              PROPER MESSAGE                               
         B     XIT                                                              
         SPACE                                                                  
         EJECT                                                                  
*              ROUTINE TO VALIDATE CHECK RECORD                                 
         SPACE                                                                  
VREC     NTR1                                                                   
         MVI   TEMPSTAT,0        CLEAR TEMPORARY VARIABLES                      
         XC    INV,INV                                                          
         XC    REDTE,REDTE                                                      
         XC    PLDTE,PLDTE                                                      
         SPACE                                                                  
         LA    R2,STPCHKH                                                       
         SPACE                                                                  
         GOTO1 SAVPTRS,DMCB,PBLOCK                                              
         SPACE                                                                  
         CLI   PFAID,17                                                         
         BE    VREC10                                                           
         SPACE                                                                  
         USING TACDD,R4                                                         
         L     R4,AIO            IF CHECK ALREADY CASHED CANNOT                 
         MVI   ELCODE,TACDELQ    STOP PAYMENT                                   
         BAS   RE,GETEL                                                         
         BNE   VREC10                                                           
         OC    TACDCSH,TACDCSH                                                  
         BNZ   ERRINV                                                           
         MVC   SVCDSTAT,TACDSTAT                                                
         DROP  R4                                                               
         SPACE                                                                  
VREC10   XC    TEMPREQ,TEMPREQ                                                  
         XC    TEMPAPP,TEMPAPP                                                  
         SPACE                                                                  
         USING TAKPD,R4                                                         
         L     R4,AIO            GET STOP PAYMENT ELEMENT                       
         MVI   ELCODE,TAKPELQ    IF IT ALREADY EXISTS                           
         BAS   RE,GETEL                                                         
         BNE   VREC16                                                           
         SPACE                                                                  
         MVC   TEMPREQ,TAKPSRBY                                                 
         MVC   TEMPAPP,TAKPAPRB                                                 
         SPACE                                                                  
         CLI   PFAID,14          IF ADDING STOP PAY REQUEST                     
         BE    ERRPFK            CANNOT ALREADY BE ADDED                        
         SPACE                                                                  
         CLI   PFAID,16                                                         
         BNE   VREC17                                                           
         OC    TAKPAPRB,TAKPAPRB                                                
         BNZ   ERRPFK                                                           
         B     VREC17                                                           
         SPACE                                                                  
VREC16   TM    SVCDSTAT,TACDSVOI IF CHECK IS VOIDED                             
         BO    ERRINV            STOP CANNOT BE ADDED                           
         SPACE                                                                  
         CLI   PFAID,16          IF APPROVING FOR REISSUE                       
         BE    ERRINV            REQUEST MUST ALREADY BE ADDED                  
         SPACE                                                                  
VREC17   LA    R2,STPCLIH        MUST CHECK OFF ONE OF THE                      
         CLI   5(R2),0           SOURCE REQUESTING STOP PAY OPTIONS             
         BE    VREC20                                                           
         OI    TEMPSTAT,TAKPCSCL CLIENT                                         
         SPACE                                                                  
VREC20   LA    R2,STPAGTH                                                       
         CLI   5(R2),0           OR AGENT                                       
         BE    VREC30                                                           
         CLI   TEMPSTAT,0                                                       
         BNE   ERRINV                                                           
         OI    TEMPSTAT,TAKPCSAG                                                
         SPACE                                                                  
VREC30   LA    R2,STPTALH                                                       
         CLI   5(R2),0           OR TALENT                                      
         BE    VREC40                                                           
         CLI   TEMPSTAT,0                                                       
         BNE   ERRINV                                                           
         OI    TEMPSTAT,TAKPCSTA                                                
         SPACE                                                                  
VREC40   LA    R2,STPUNIH                                                       
         CLI   5(R2),0           OR UNION                                       
         BE    VREC50                                                           
         CLI   TEMPSTAT,0                                                       
         BNE   ERRINV                                                           
         OI    TEMPSTAT,TAKPCSUN                                                
         SPACE                                                                  
VREC50   LA    R2,STPCLIH                                                       
         CLI   TEMPSTAT,0                                                       
         BE    ERRMIS                                                           
         DROP  R4                                                               
         SPACE                                                                  
         USING TAXCD,R4                                                         
         L     R4,AIO                                                           
         MVI   ELCODE,TAXCELQ      REMOVE PREVIOUS TAXC                         
         BAS   RE,GETEL            ELEMENTS                                     
         B     *+8                                                              
VREC50A  BAS   RE,NEXTEL                                                        
         BNE   VREC50B                                                          
         CLI   TAXCSEQ,5           (EXCEPT CHECK PULL'S                         
         BE    VREC50A              SPECIAL INSTRUCTIONS)                       
         MVI   TAXCEL,X'FF'                                                     
         B     VREC50A                                                          
VREC50B  MVI   ELCODE,X'FF'                                                     
         GOTO1 REMELEM                                                          
         SPACE                                                                  
         LA    R2,STPREASH                                                      
         CLI   5(R2),0                                                          
         BE    ERRMIS                                                           
         XC    ELEMENT,ELEMENT                                                  
         LA    R4,ELEMENT                                                       
         USING TAXCD,R4                                                         
         MVI   TAXCEL,TAXCELQ                                                   
         ZIC   RE,5(R2)                                                         
         AHI   RE,4                                                             
         STC   RE,TAXCLEN         ADD TANX ELEMENT FOR                          
         MVI   TAXCSEQ,1          REASON FOR STOP PAY FIELD                     
         ZIC   RE,5(R2)                                                         
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   TAXCCMNT(0),8(R2)                                                
         GOTO1 ADDELEM                                                          
         SPACE 1                                                                
VREC51   LA    R2,STPRYESH       MUST CHECK OFF ONE OF THE                      
         CLI   5(R2),0           REISSUE CHECK OPTIONS                          
         BE    *+8                                                              
         OI    TEMPSTAT,TAKPCSYS YES                                            
         SPACE                                                                  
         LA    R2,STPRNOH                                                       
         CLI   5(R2),0           OR NO                                          
         BE    VREC60                                                           
         TM    TEMPSTAT,TAKPCSYS                                                
         BO    ERRINV                                                           
         OI    TEMPSTAT,TAKPCSNO                                                
         SPACE                                                                  
VREC60   LA    R2,STPRYESH                                                      
         TM    TEMPSTAT,TAKPCSYS+TAKPCSNO                                       
         BZ    ERRMIS                                                           
         SPACE                                                                  
         LA    R2,STPCINVH                                                      
         CLI   5(R2),0                                                          
         BE    VREC61            IF CR/BNP INVOICE INPUT IS                     
         BAS   RE,SETTAL         A VALID INVOICE THEN OK                        
         MVC   AIO,AIO2                                                         
         GOTO1 TINVCON,DMCB,STPCINV,INV,DATCON                                  
         CLI   0(R1),X'FF'                                                      
         BE    ERRINV                                                           
         XC    INV,ALLFF                                                        
         GOTO1 RECVAL,DMCB,TLINCDQ,(X'A4',INV)                                  
         BNE   ERRINV                                                           
         MVC   AIO,AIO1                                                         
         MVC   INV,STPCINV                                                      
         SPACE                                                                  
VREC61   LA    R2,STPSPINH                                                      
         CLI   5(R2),0                                                          
         BE    VREC62                                                           
         XC    ELEMENT,ELEMENT                                                  
         LA    R4,ELEMENT                                                       
         USING TAXCD,R4                                                         
         MVI   TAXCEL,TAXCELQ                                                   
         ZIC   RE,5(R2)                                                         
         AHI   RE,4              ADD TANX ELEMENT FOR                           
         STC   RE,TAXCLEN        SPECIAL INSTRUCTIONS                           
         MVI   TAXCSEQ,2                                                        
         ZIC   RE,5(R2)                                                         
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   TAXCCMNT(0),8(R2)                                                
         GOTO1 ADDELEM                                                          
         SPACE                                                                  
VREC62   LA    R2,STPCML1H                                                      
         CLI   5(R2),0                                                          
         BE    VREC63                                                           
         CLI   STPRYESH+5,0      REISSUED CHECK TO BE MAILED TO ONLY            
         BE    ERRINV            ALLOWED IF REISSUE CHECK IS YES                
         XC    ELEMENT,ELEMENT                                                  
         LA    R4,ELEMENT                                                       
         USING TAXCD,R4                                                         
         MVI   TAXCEL,TAXCELQ                                                   
         ZIC   RE,5(R2)                                                         
         AHI   RE,4                                                             
         STC   RE,TAXCLEN        ADD TANX ELEMENT FOR REISSUED                  
         MVI   TAXCSEQ,3         CHECK TO BE MAILED TO FIELD 1                  
         ZIC   RE,5(R2)                                                         
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   TAXCCMNT(0),8(R2)                                                
         GOTO1 ADDELEM                                                          
         SPACE                                                                  
VREC63   LA    R2,STPCML2H                                                      
         CLI   5(R2),0                                                          
         BE    VREC64                                                           
         CLI   STPRYESH+5,0      REISSUED CHECK TO BE MAILED TO ONLY            
         BE    ERRINV            ALLOWED IF REISSUE CHECK IS YES                
         XC    ELEMENT,ELEMENT                                                  
         LA    R4,ELEMENT                                                       
         USING TAXCD,R4                                                         
         MVI   TAXCEL,TAXCELQ    ADD TANX ELEMENT FOR REISSUED                  
         ZIC   RE,5(R2)          CHECK TO BE MAILED TO FIELD 2                  
         AHI   RE,4                                                             
         STC   RE,TAXCLEN                                                       
         MVI   TAXCSEQ,4                                                        
         ZIC   RE,5(R2)                                                         
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   TAXCCMNT(0),8(R2)                                                
         GOTO1 ADDELEM                                                          
         SPACE                                                                  
VREC64   LA    R2,STPRECKH                                                      
         CLI   5(R2),0                                                          
         BE    VREC64B                                                          
         CLI   STPRYESH+5,0      REISSUED ON CHECK# ONLY VALID                  
         BE    ERRINV            IF REISSUE CHECK IS YES                        
         MVC   SVCHK,TGCHK                                                      
         MVC   TGCHK,STPRECK     CHECK NUMBER MUST EXIST                        
         GOTO1 READCK,DMCB,(C'N',0)                                             
         MVC   TGCHK,SVCHK                                                      
         SPACE                                                                  
VREC64B  CLI   STPRCKDH+5,0      IF REISSUE ON CHECK DATE IS                    
         BE    VREC64C           INPUT BUT NO REISSUED CHECK                    
         CLI   5(R2),0           # GIVE MISSING ERROR                           
         BE    ERRMIS                                                           
         B     VREC64D                                                          
VREC64C  LA    R2,STPRCKDH       IF REISSUE ON CHECK DATE IS                    
         CLI   STPRECKH+5,0      NOT INPUT BUT REISSUED CHECK                   
         BNE   ERRMIS            # IS GIVE MISSING ERROR                        
         SPACE                                                                  
VREC64D  LA    R2,STPRCKDH                                                      
         CLI   5(R2),0           VALIDATE REISSUED CHECK DATE                   
         BE    VREC64E                                                          
         GOTO1 DTVAL,DMCB,REDTE                                                 
         SPACE                                                                  
VREC64E  LA    R2,STPRECKH                                                      
         CLI   STPRCKBH+5,0      IF REISSUE ON CHECK BY IS INPUT                
         BE    VREC64F           BUT NO REISSUED CHECK # GIVE                   
         CLI   5(R2),0           MISSING ERROR                                  
         BE    ERRMIS                                                           
         B     VREC65                                                           
VREC64F  LA    R2,STPRCKBH       IF REISSUE CHECK BY IS                         
         CLI   STPRECKH+5,0      NOT INPUT BUT REISSUED CHECK #                 
         BNE   ERRMIS            IS GIVE MISSING ERROR                          
         SPACE                                                                  
VREC65   LA    R2,STPSTPBH                                                      
         OC    STPSTPB,SPACES                                                   
         OC    SVSTPB,SPACES                                                    
         CLC   STPSTPB,SVSTPB    IF STOP PLACED BY                              
         BNE   VREC65A                                                          
         LA    R2,STPSTPDH                                                      
         OC    STPSTPD,SPACES                                                   
         OC    SVSTPD,SPACES                                                    
         CLC   STPSTPD,SVSTPD    OR DATE HAS BEEN CHANGED                       
         BE    VREC65B                                                          
VREC65A  CLI   TGCTSTTY,TASTTYPP STAFF MEMBER MUST BE PROGRAMMER                
         BE    VREC65B                                                          
         CLI   TGCTSTTY,TASTTYPA TYPE A                                         
         BE    VREC65B                                                          
         CLI   TGCTSTTY,TASTTYPB OR TYPE B                                      
         BNE   ERRNOC                                                           
         SPACE                                                                  
VREC65B  LA    R2,STPSTPBH        IF STOP PLACED BY IS NOT INPUT                
         CLI   5(R2),0            STOP PLACED DATE CANNOT BE                    
         BNE   VREC65C            EITHER                                        
         CLI   STPSTPDH+5,0                                                     
         BNE   ERRMIS                                                           
         B     VREC66                                                           
VREC65C  LA    R2,STPSTPDH        IF STOP PLACED BY IS INPUT                    
         CLI   5(R2),0            STOP PLACED DATE MUST ALSO                    
         BE    ERRMIS                                                           
         SPACE                                                                  
         LA    R2,STPSTPDH        VALIDATE STOP PLACED BY AND DATE              
         CLI   5(R2),0                                                          
         BE    VREC66                                                           
         GOTO1 DTVAL,DMCB,PLDTE                                                 
         SPACE                                                                  
VREC66   MVI   ELCODE,TAKPELQ    DELETE ALREADY EXISTING STOP                   
         GOTO1 REMELEM           PAY ELEMENT                                    
         SPACE                                                                  
         XC    ELEMENT,ELEMENT   BEGIN BUILDING NEW ONE                         
         LA    R4,ELEMENT                                                       
         USING TAKPD,R4                                                         
         MVI   TAKPEL,TAKPELQ                                                   
         MVI   TAKPLEN,TAKPLNQ                                                  
         MVC   TAKPCSST,TEMPSTAT                                                
         MVC   TAKPCBIN,INV                                                     
         MVC   TAKPSRBY(11),TEMPREQ                                             
         MVC   TAKPRECN,STPRECK                                                 
         MVC   TAKPRECD,REDTE                                                   
         MVC   TAKPRECB,STPRCKB                                                 
         MVC   TAKPSPBY,STPSTPB                                                 
         MVC   TAKPSPDT,PLDTE                                                   
         SPACE                                                                  
         CLI   PFAID,14          IF ADDING REQUEST                              
         BNE   VREC70                                                           
         MVC   TAKPSRBY,TGCTSTAF SAVE REQUESTER                                 
         MVC   TAKPSRDT,TGTODAY1 AND DATE                                       
         SPACE                                                                  
VREC70   MVC   TAKPAPRB(11),TEMPAPP                                             
         CLI   PFAID,16          IF APPROVING REISSUE                           
         BNE   VREC80                                                           
         MVC   TAKPAPRB,TGCTSTAF SAVE APPROVER                                  
         MVC   TAKPAPRD,TGTODAY1 AND DATE                                       
VREC80   GOTO1 ADDELEM                                                          
         SPACE                                                                  
         CLI   PFAID,17          IF CANCELLING STOP                             
         BNE   VREC85                                                           
         OC    TAKPSPDT,TAKPSPDT STOP CANNOT ALREADY BE PLACED                  
         BNZ   NOCANCEL                                                         
         XC    ELEMENT,ELEMENT                                                  
         LA    R4,ELEMENT                                                       
         USING TACND,R4                                                         
         MVI   TACNEL,TACNELQ                                                   
         MVI   TACNLEN,TACNLNQ                                                  
         MVI   TACNTYPE,TACNTYPS ADD CANCEL STOP ELEMENT                        
         MVC   TACNBY,TGCTSTAF   WITH CANCELLER                                 
         MVC   TACNDT,TGTODAY1   AND DATE                                       
         GOTO1 ADDELEM                                                          
         SPACE                                                                  
VREC85   GOTO1 ACTVIN,DMCB,(X'80',LSTCHGF)   ADD ACTIVITY ELEM                  
         SPACE                                                                  
         CLI   PFAID,14          ONLY PUT THE CHECK RECORD                      
         BE    VREC90            IF PF KEY 14, 15,16 OR 17 IS HIT               
         CLI   PFAID,15                                                         
         BE    VREC90                                                           
         CLI   PFAID,16                                                         
         BE    VREC90                                                           
         CLI   PFAID,17                                                         
         BNE   PRESSPF                                                          
         SPACE                                                                  
VREC90   MVC   AIO,AIO2                                                         
         GOTO1 READCK,DMCB,(C'Y',0)                                             
         MVC   AIO,AIO1                                                         
         GOTO1 PUTREC                                                           
         GOTO1 ADDPTRS,DMCB,(X'22',PBLOCK)                                      
         SPACE                                                                  
         MVI   PFAID,0                                                          
         BAS   RE,PFINVIS                                                       
         BAS   RE,PFVIS                                                         
         B     XIT                                                              
         EJECT                                                                  
READCK   NTR1                                                                   
         MVC   DOREAD,0(R1)                                                     
         SPACE                                                                  
         MVC   SYSDIR,=CL8'CHKDIR'                                              
         MVC   SYSFIL,=CL8'CHKFIL'                                              
         SPACE                                                                  
         LA    R4,KEY              R4 = A(CHECK KEY)                            
         USING TLCKPD,R4                                                        
         XC    KEY,KEY                                                          
         MVI   TLCKPCD,TLCKCCDQ    RECORD CODE                                  
         MVC   TLCKCCHK,TGCHK      CHECK NUMBER                                 
         XC    TLCKCCHK,ALLFF      COMPLEMENTED                                 
         SPACE                                                                  
         CLI   DOREAD,C'N'                                                      
         BNE   *+10                                                             
         MVC   TGCHK,SVCHK                                                      
         SPACE                                                                  
         GOTO1 HIGH                READ FOR KEY                                 
         CLC   KEY(TLCKCBNK-TLCKPD),KEYSAVE                                     
         BNE   ERNOFND                                                          
         SPACE                                                                  
RCK10    MVC   LSTFNDKY,KEY                                                     
         GOTO1 SEQ                                                              
         CLC   KEY(TLCKCSEQ-TLCKPD),LSTFNDKY                                    
         BE    RCK10                                                            
         MVC   KEY,LSTFNDKY                                                     
         GOTO1 HIGH                                                             
         CLC   KEY(TLCKCSEQ-TLCKPD),LSTFNDKY                                    
         BE    *+6                                                              
         DC    H'00'                                                            
         SPACE                                                                  
         CLI   DOREAD,C'Y'                                                      
         BNE   XIT                                                              
         SPACE                                                                  
         MVI   RDUPDATE,C'Y'                                                    
         OI    DMINBTS,X'80'                                                    
         GOTO1 GETREC              READ CHECK RECORD                            
         DROP  R4                                                               
         B     XIT                                                              
         SPACE 2                                                                
SETTAL   DS    0H                                                               
         MVC   SYSDIR,=CL8'TALDIR'                                              
         MVC   SYSFIL,=CL8'TALFIL'                                              
         BR    RE                                                               
         SPACE 2                                                                
YES      XR    RC,RC                                                            
NO       LTR   RC,RC                                                            
XIT      XIT1                                                                   
         EJECT                                                                  
*              ERROR MESSAGES                                                   
         SPACE                                                                  
ERRINV   MVI   ERROR,INVALID                                                    
         B     ERREND                                                           
         SPACE                                                                  
ERRMIS   MVI   ERROR,MISSING                                                    
         B     ERREND                                                           
         SPACE                                                                  
ERNOFND  MVI   ERROR,NOTFOUND                                                   
         B     ERREND                                                           
ERRPFKE  NI    PROSTAT,X'FF'-PROSDPF                                            
ERRPFK   MVI   ERROR,ERINVPFK                                                   
         B     ERREND                                                           
         SPACE                                                                  
ERREND   NI    PROSTAT,X'FF'-PROWARN                                            
         SPACE                                                                  
         TM    PROSTAT,PROSDPF                                                  
         BZ    MESSEND                                                          
         SPACE                                                                  
ENTCHG   MVI   MYMSGNO1,107                                                     
         B     SPECMEND                                                         
         SPACE                                                                  
CHKCHG   MVI   MYMSGNO1,108                                                     
         NI    PROSTAT,X'FF'-PROWARN                                            
         B     SPECMEND                                                         
         SPACE                                                                  
NOCANCEL LA    R2,STPSTPBH                                                      
         MVI   MYMSGNO1,110                                                     
         B     SPECMEN2                                                         
         SPACE                                                                  
ERRNOC   MVI   MYMSGNO1,111                                                     
         B     SPECMEN2                                                         
         SPACE                                                                  
PRESSPF  MVI   MYMSGNO1,106                                                     
         B     SPECMEND                                                         
         SPACE                                                                  
SPECMEND L     R2,EFHREC                                                        
SPECMEN2 OI    GENSTAT2,USGETTXT                                                
         B     MESSEND                                                          
         SPACE                                                                  
MESSEND  GOTO1 EXIT,DMCB,0                                                      
         EJECT                                                                  
         GETEL R4,DATADISP,ELCODE                                               
         EJECT                                                                  
*              CONSTANTS, ETC.                                                  
ALLFF    DC    8X'FF'                                                           
SPACES   DC    20C' '                                                           
         SPACE 2                                                                
*              PF KEY TABLE                                                     
         SPACE                                                                  
PFTAB    DS    0C                  PF TABLE                                     
         DC    AL1(PF13X-*,13,0,0,0)                                            
         DC    CL3' ',CL8'CHECK  ',CL8'DISPLAY'                                 
PF13X    EQU   *                                                                
         DC    AL1(PF14X-*,14,0,0,PFTRETRN)                                     
         DC    CL3' ',CL8'        ',CL8'        '                               
PF14X    EQU   *                                                                
         DC    AL1(PF15X-*,15,0,0,PFTRETRN)                                     
         DC    CL3' ',CL8'        ',CL8'        '                               
PF15X    EQU   *                                                                
         DC    AL1(PF16X-*,16,0,0,PFTRETRN)                                     
         DC    CL3' ',CL8'        ',CL8'        '                               
PF16X    EQU   *                                                                
         DC    AL1(PF17X-*,17,0,0,PFTRETRN)                                     
         DC    CL3' ',CL8'        ',CL8'        '                               
PF17X    EQU   *                                                                
         DC    X'FF'                                                            
         EJECT                                                                  
LISTTAB  DS    0H                                                               
         DC    AL1(LT13X-*,13,0,0,0)                                            
         DC    CL3' ',CL8'CHECK  ',CL8'DISPLAY'                                 
LT13X    EQU   *                                                                
         DC    AL1(LT14X-*,14,0,0,PFTRETRN)                                     
         DC    CL3' ',CL8'        ',CL8'        '                               
LT14X    EQU   *                                                                
         DC    AL1(LT15X-*,15,0,0,PFTRETRN)                                     
         DC    CL3' ',CL8'        ',CL8'        '                               
LT15X    EQU   *                                                                
         DC    AL1(LT16X-*,16,0,0,PFTRETRN)                                     
         DC    CL3' ',CL8'        ',CL8'        '                               
LT16X    EQU   *                                                                
         DC    AL1(LT17X-*,17,0,0,PFTRETRN)                                     
         DC    CL3' ',CL8'        ',CL8'        '                               
LT17X    EQU   *                                                                
         DC    AL1(LT18X-*,18,PFTRPROG+PFTINT,0,0)                              
         DC    CL3' ',CL8' ',CL8' '                                             
LT18X    EQU   *                                                                
         DC    X'FF'                                                            
         SPACE 2                                                                
         LTORG                                                                  
*                                                                               
         EJECT                                                                  
       ++INCLUDE TAGENFFD                                                       
         EJECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE DDGENTWA                                                       
         PRINT ON                                                               
         ORG   CONTAGH                                                          
       ++INCLUDE TASCRE9D                                                       
         EJECT                                                                  
         ORG   STPWORK                                                          
*                                                                               
DOREAD   DS    C                                                                
INV      DS    CL6                                                              
TEMPSTAT DS    X                                                                
TEMPREQ  DS    XL11                                                             
TEMPAPP  DS    XL11                                                             
SVCHK    DS    CL8                                                              
REDTE    DS    XL3                                                              
PLDTE    DS    XL3                                                              
CANCELBY DS    XL8                                                              
CANCELDT DS    XL3                                                              
SVSTPB   DS    XL8                                                              
SVSTPD   DS    XL8                                                              
SVCDSTAT DS    XL(L'TACDSTAT)                                                   
LSTCHGF  DS    XL25                                                             
LSTFNDKY DS    XL32                                                             
PROSTAT  DC    X'00'                                                            
PROWARN  EQU   X'80'                                                            
PROSDPF  EQU   X'40'                                                            
*                                                                               
PBLOCK   DS    CL(L'TLDRREC*22+1)                                               
*                                                                               
BEG      EQU   (*-CONHEADH)                                                     
         DS    CL(3520-BEG)        CANNOT EXCEED DDGENTWA                       
         EJECT                                                                  
* TAGENWORKD                                                                    
*        PRINT OFF                                                              
       ++INCLUDE TAGENWORKD                                                     
*        PRINT ON                                                               
         EJECT                                                                  
* TAGENFILE                                                                     
* TASYSEQUS                                                                     
* TASYSDSECT                                                                    
* DDSPLWORKD                                                                    
* TAGENEQUS                                                                     
* FAWSSVRD                                                                      
*        PRINT OFF                                                              
       ++INCLUDE TAGENFILE                                                      
       ++INCLUDE TASYSEQUS                                                      
       ++INCLUDE TASYSDSECT                                                     
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE TAGENEQUS                                                      
       ++INCLUDE FAWSSVRD                                                       
*        PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'012TAGENE9   01/12/10'                                      
         END                                                                    
