*          DATA SET SRLST00    AT LEVEL 003 AS OF 04/15/14                      
*PHASE T11900A                                                                  
*INCLUDE GETIDS                                                                 
*INCLUDE TIMEOUT                                                                
         TITLE '$LIST - LIST TERMINAL CONNECT DATA'                             
         PRINT NOGEN                                                            
LST      CSECT                                                                  
         NMOD1 WORKL,**$LIST**,R8,CLEAR=YES,RR=RE                               
         USING WORKD,RC                                                         
         ST    RE,RELO                                                          
         MVC   SRPARS,0(R1)                                                     
*                                                                               
         L     RA,ATWA             RA=A(TWA)                                    
         USING SRLSTFFD,RA                                                      
         L     R9,ASYSFAC          R9=SYSFAC LIST                               
         USING SYSFACD,R9                                                       
*                                                                               
         L     RF,ACOMFACS         A(COMFACS)                                   
         USING COMFACSD,RF                                                      
         MVC   VHEXOUT,CHEXOUT                                                  
         MVC   VHEXIN,CHEXIN                                                    
         MVC   VGETFACT,CGETFACT                                                
         MVC   VBINSRCH,CBINSRCH                                                
         MVC   VDATCON,CDATCON                                                  
         DROP  RF                                                               
*                                                                               
         L     RF,=V(TIMEOUT)      GET TIME                                     
         A     RF,RELO                                                          
         ST    RF,VTIMEOUT                                                      
         L     RF,=A(ERRMSGS)                                                   
         A     RF,RELO                                                          
         ST    RF,AERRMSGS                                                      
*                                                                               
         TIME  TU                                                               
         ST    R0,TIMENOW1         TIME IN MVS TU'S (1/38400 SEC)               
         SRDL  R0,32                                                            
         D     R0,=F'384'                                                       
         ST    R1,TIMENOW          TIME IN 1/100 SEC                            
         XC    MSG,MSG                                                          
*                                                                               
         GOTO1 VGETFACT,DUB,0      GET A(SYSLST)                                
         L     R1,0(R1)                                                         
         USING FACTSD,R1                                                        
         MVC   VSYSLST,FASYSLST-FACTSD(R1)                                      
*                                                                               
         NI    SRVIDH+6,X'BF'      UNSET CURSOR                                 
         L     RE,VSSB             EXTRACT SSB DATA                             
         MVI   TIMEUNIT,0          SET 1/100 SEC UNITS                          
         TM    SSBSTAT3-SSBD(RE),SSBMVSTU                                       
         BZ    *+8                                                              
         MVI   TIMEUNIT,1          SET 1/38400 SEC UNITS                        
         MVC   SYSNAME,SSBSYSN4-SSBD(RE)                                        
         MVC   CURDATE,SSBDATEB-SSBD(RE)                                        
*                                                                               
         LA    R0,SRVL1AH                                                       
         ST    R0,ANXTLIN          FIRST OUTPUT LINE                            
         ZAP   ACTRMS,PZERO                                                     
         ZAP   NTRMS,PZERO                                                      
*                                                                               
         LA    R2,SRVIDH           VALIDATE PRIVILEGED USER                     
         USING FHD,R2                                                           
         BRAS  RE,PRIV                                                          
         BZ    *+12                                                             
         MVI   FERN,0                                                           
         B     ERROR                                                            
*                                                                               
         LA    R2,SRVP1H           EDIT PARAMS                                  
         USING FHD,R2                                                           
EDITP    CLI   FHIL,0                                                           
         BE    NXTFLD                                                           
         BRAS  RE,TRYSPEC                                                       
         BE    NXTFLD                                                           
         BL    ERROR                                                            
*                                                                               
         BRAS  RE,TRYAG                                                         
         BE    NXTFLD                                                           
         BL    ERROR                                                            
*                                                                               
         BRAS  RE,TRYSYS                                                        
         BE    NXTFLD                                                           
         BL    ERROR                                                            
*                                                                               
         BRAS  RE,TRYPROG                                                       
         BE    NXTFLD                                                           
         BL    ERROR                                                            
*                                                                               
         BRAS  RE,TRYTIM                                                        
         BE    NXTFLD                                                           
         BL    ERROR                                                            
*                                                                               
         BRAS  RE,TRYTNO                                                        
         BE    NXTFLD                                                           
         BL    ERROR                                                            
*                                                                               
         BRAS  RE,TRYLUID                                                       
         BE    NXTFLD                                                           
         BL    ERROR                                                            
*                                                                               
         BRAS  RE,TRYRPL                                                        
         BE    NXTFLD                                                           
         BL    ERROR                                                            
*                                                                               
         BRAS  RE,TRYMODE                                                       
         BE    NXTFLD                                                           
         BL    ERROR                                                            
*                                                                               
         BRAS  RE,TRYPRNT                                                       
         BE    NXTFLD                                                           
         BL    ERROR                                                            
*                                                                               
         MVI   FERN,2                                                           
         B     ERROR                                                            
*                                                                               
NXTFLD   LLC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         TM    1(R2),X'20'         TEST PROTECTED                               
         BZ    EDITP                                                            
*                                                                               
         SAM31                                                                  
         CLI   RPL,C'Y'                                                         
         BNE   *+12                                                             
         BRAS  RE,DORPL                                                         
         B     *+8                                                              
         BRAS  RE,LIST                                                          
         BRAS  RE,FMTTOTS                                                       
         B     EXIT                                                             
*                                                                               
ERROR    MVC   SRVMSG(15),=C'ED/9999 (XXXX) '                                   
         MVC   SRVMSG+09(4),SYSNAME                                             
         LLC   RF,FERN                                                          
         MHI   RF,L'ERRMSGS                                                     
         A     RF,AERRMSGS                                                      
         MVC   MSG(32),0(RF)                                                    
         MVC   SRVMSG+15(L'SRVMSG-15),MSG                                       
         OI    6(R2),X'40'         CURSOR                                       
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO LIST OUT UTL ENTRIES                                     *         
***********************************************************************         
LIST     NTR1                                                                   
         OC    FSTIM,FSTIM                                                      
         BNZ   *+12                                                             
         MVI   FSTIM+3,1           NON-ZERO START TIME                          
         MVI   FSTIM1+3,1                                                       
*                                                                               
         L     R3,VUTL                                                          
         LH    R4,0(R3)                                                         
         L     R5,2(R3)                                                         
         AHI   R3,6                                                             
         USING UTLD,R3                                                          
*                                                                               
         CLI   SORTSW,C'N'         SORTING UTLS?                                
         BE    LIST02              NO                                           
*                                                                               
         LA    R0,BSTAB            SET BSPARS                                   
         ST    R0,BSPAR2                                                        
         LA    R0,TUTLXALN         LENGTH                                       
         ST    R0,BSPAR4                                                        
         LHI   R0,L'TTIME+L'TSIN                                                
         ST    R0,BSPAR5           KEY LEN AND DISPLACEMENT                     
         MVI   BSPAR5,(TSIN-TNUM)                                               
         LHI   R0,33               MAX                                          
         ST    R0,BSPAR6                                                        
*                                                                               
LIST02   AP    NTRMS,PONE                                                       
         BRAS  RE,FILTER                                                        
         BNE   LISTNXT                                                          
*                                                                               
         AP    ACTRMS,PONE         BUMP NUMBER OF TERMINALS                     
         CLI   SORTSW,C'N'         TEST SORTED OUTPUT                           
         BE    LIST06                                                           
*                                                                               
         MVC   WORK,UTLD                                                        
TMP      USING UTLD,WORK                                                        
         XC    TMP.TTIME,EFFS      COMPLIMENT TO SORT                           
         XC    TMP.TSIN,TMP.TSIN                                                
         MVC   TMP.TSIN,TDATEB                                                  
         XC    TMP.TSIN(3),EFFS    COMPLIMENT DATE TO TSIN                      
*                                                                               
LIST04   LA    RF,WORK             PUT UTL ENTRY IN BSTAB                       
         ST    RF,BSPARS                                                        
         MVI   BSPARS,1            SET TO ADD                                   
         GOTO1 VBINSRCH,BSPARS,TO24=Y                                           
         SAM31                                                                  
*                                                                               
         OC    BSPAR1+1(3),BSPAR1+1 TEST TABLE FULL                             
         BNZ   LISTNXT             NO                                           
         LHI   R0,32               SHORTEN TABLE                                
         ST    R0,BSPAR3                                                        
         B     LIST04              TRY TO ADD AGAIN                             
*                                  NO 'SORT'- PUT TO SCREEN NOW                 
LIST06   BRAS  RE,PUTTRM                                                        
*                                                                               
LISTNXT  BXLE  R3,R4,LIST02                                                     
         CLI   FLUID,0                                                          
         BE    LIST46                                                           
         CLI   HAVLUID,C'Y'                                                     
         BE    LIST46                                                           
*                                                                               
         L     R2,LINPARAM                                                      
         MVI   FERN,4              INVALID LINE ID                              
         B     EXITL                                                            
*                                                                               
LIST46   CLI   SORTSW,C'N'                                                      
         BE    EXITOK                                                           
*                                  PUT BSTAB LIST TO SCREEN                     
         L     R3,BSPAR2           A(TABLE)                                     
         ICM   R4,15,BSPAR3        NUMBER IN TABLE                              
         BNP   EXITOK                                                           
*                                                                               
LIST50   XC    TTIME,EFFS          RECOMPLIMENT IN TABLE                        
         BRAS  RE,PUTTRM                                                        
         A     R3,BSPARS+12        LENGTH                                       
         BCT   R4,LIST50                                                        
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* OUTPUT RPL INFORMATION                                              *         
***********************************************************************         
DORPL    NTR1                                                                   
         L     R7,ARPL             SCAN RPL LIST                                
         USING FARPLD,R7                                                        
RPL02    TM    FARPLFLG,FARPLBSY   LOOKING FOR A BUSY ONE                       
         BNO   RPL06                                                            
*                                                                               
         MVC   NEYE,FARPLEYE       SAVE EYECATCHER                              
         L     R3,FARPLRPL                                                      
         L     R3,92(R3)           RPLUSFLD                                     
         USING UTLD,R3                                                          
         MVC   NSYM,TSYM           GET LUID FROM UTL                            
*                                                                               
         L     R3,VUTL                                                          
         LH    RE,0(R3)                                                         
         L     RF,2(R3)                                                         
         AHI   R3,6                                                             
         USING UTLD,R3                                                          
RPL04    CLC   NSYM,TSYM           FIND UTL ENTRY                               
         BE    *+12                                                             
         BXLE  R3,RE,RPL04                                                      
         B     RPL06               LUID NOT IN UTL  (THIS IS OK)                
*                                                                               
         BRAS  RE,PUTTRM           DISPLAY TERMINAL                             
*                                                                               
RPL06    ICM   R7,15,FARPLNXT                                                   
         BNZ   RPL02                                                            
         B     EXITOK              LAST RPL SO EXIT                             
         DROP  R3,R7                                                            
         EJECT                                                                  
***********************************************************************         
* FILTER UTL ENTRY TO SEE IF VALID TO BE INCLUDED                     *         
* R3=A(UTL ENTRY)                                                     *         
***********************************************************************         
         USING UTLD,R3                                                          
FILTER   NTR1                                                                   
         CLI   FLUID,0             FILTERING ON VTAM LUID                       
         BE    FLT06               NO                                           
         CLI   FLUID,X'FF'         START LUID ALREADY REACHED                   
         BE    FLT06                                                            
*                                                                               
         LA    RF,TLUID            POINT TO LUID IN UTL                         
         LA    RE,FLUID                                                         
         LHI   R0,L'TLUID                                                       
FLT02    CLI   0(RE),C' '                                                       
         BE    FLT04                                                            
         CLI   0(RE),C'*'                                                       
         BE    FLT04                                                            
         CLI   0(RE),X'00'                                                      
         BE    FLT04                                                            
         CLC   0(1,RE),0(RF)                                                    
         BNE   EXITL                                                            
*                                                                               
FLT04    AHI   RE,1                                                             
         AHI   RF,1                                                             
         BCT   R0,FLT02                                                         
         MVI   HAVLUID,C'Y'                                                     
*                                                                               
FLT06    CLI   FAGY,0              AGENCY CODE                                  
         BE    *+14                                                             
         CLC   TAGY,FAGY                                                        
         BNE   EXITL                                                            
*                                                                               
         CLI   FSYSF,0             SYS NO                                       
         BE    *+14                                                             
         CLC   TSYS,FSYSF                                                       
         BNE   EXITL                                                            
*                                                                               
         CLI   FSYSO,0             OVERLAY SYS                                  
         BE    *+14                                                             
         CLC   TOVSYS,FSYSO                                                     
         BNE   EXITL                                                            
*                                                                               
         CLI   FPROG,0             PROGRAM                                      
         BE    *+14                                                             
         CLC   TPRG,FPROG                                                       
         BNE   EXITL                                                            
*                                                                               
         OC    FTNO,FTNO           START TERM NUM                               
         BE    *+14                                                             
         CLC   TNUM,FTNO                                                        
         BL    EXITL                                                            
*                                                                               
         CLI   FPROC,C'P'          IN PROCESS                                   
         BNE   *+12                                                             
         TM    TSTAT2,TSTATTIP                                                  
         BZ    EXITL                                                            
*                                                                               
         CLI   FCON,C'C'           CONNECTED                                    
         BNE   *+12                                                             
         CLI   TAGY,0                                                           
         BE    EXITL                                                            
*                                                                               
         CLI   FCON,C'D'           NOT CONNECTED                                
         BNE   *+12                                                             
         CLI   TAGY,0                                                           
         BNE   EXITL                                                            
*                                                                               
         CLI   FVTBLD,C'N'         VTAM TERMINAL BUILD FAILED                   
         BNE   *+12                                                             
         TM    TSTAT5,TST5TBF                                                   
         BZ    EXITL                                                            
*                                                                               
         CLI   FBROAD,C'B'         BROADCAST PENDING                            
         BNE   *+12                                                             
         TM    TSTAT2,TSTATBCP                                                  
         BZ    EXITL                                                            
*                                                                               
         ICM   RF,1,STEREO         STEREO                                       
         BZ    FLT08                                                            
         EX    RF,*+8                                                           
         BNO   EXITL                                                            
         TM    TSTAT6,0                                                         
*                                                                               
FLT08    ICM   RF,1,FJOBS          JOB SUBMIT/READY                             
         BZ    FLT10                                                            
         EX    RF,*+8                                                           
         BNO   EXITL                                                            
         TM    TJOBFLAG,0                                                       
         OC    TPRNT,TPRNT         TEST PRINTER                                 
         BNZ   EXITL                                                            
*                                                                               
FLT10    CLI   TIMEUNIT,0          TIME                                         
         BNE   *+14                                                             
         CLC   TTIMETU,FSTIM1                                                   
         B     *+10                                                             
         CLC   TTIMETU,FSTIM                                                    
         BNL   FLT12                                                            
         CLI   FVTBLD,C'N'                                                      
         BE    FLT12                                                            
         CLI   FCON,C'D'                                                        
         BE    FLT12                                                            
         B     EXITL                                                            
*                                                                               
FLT12    CLI   FPRINT,C'P'                                                      
         BNE   *+14                                                             
         OC    TPRNT,TPRNT                                                      
         BZ    EXITL                                                            
*                                                                               
         LA    R1,ASPEC            SPECIAL FILTERS                              
FLT14    ICM   RF,15,0(R1)                                                      
         BZ    FLT16                                                            
         BASR  RE,RF               GOTO SPECIAL ACTION TEST                     
         BZ    EXITL               IF CC=ZERO REJECT                            
         LA    R1,4(R1)                                                         
         LA    RF,ASPECX                                                        
         CR    R1,RF                                                            
         BNH   FLT14               UP TO 4 USER TESTS                           
*                                                                               
FLT16    TM    DDS,X'01'           PRIVILEGED USER                              
         BO    FLT20                                                            
         OC    TUSER,TUSER         MUST BE CONNECTED                            
         BZ    EXITL                                                            
*                                                                               
         L     RF,AIDLIST                                                       
FLT18    CLI   0(RF),X'FF'         MUST BE IN LIST OF COMPAT IDS                
         BE    EXITL                                                            
         CLC   10(2,RF),TUSER                                                   
         BE    FLT20                                                            
         LA    RF,12(RF)                                                        
         B     FLT18                                                            
*                                                                               
FLT20    B     EXITOK                                                           
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* OUTPUT UTL ENTRY TO SCREEN                                          *         
* R3=A(UTL ENTRY)                                                     *         
***********************************************************************         
         USING UTLD,R3                                                          
PUTTRM   NTR1                                                                   
         CLI   ANXTLIN,X'FF'       EOS                                          
         BE    EXITOK                                                           
         L     R2,ANXTLIN                                                       
         USING TRMLD,R2                                                         
         USING FHD,TRMHDR                                                       
         CLI   FSTAR,C'*'          TEST PUT ONLY ONE                            
         BNE   PTT02                                                            
         LA    R0,SRVL1AH                                                       
         CR    R2,R0                                                            
         BH    EXITOK                                                           
*                                                                               
PTT02    LH    R0,TNUM             TERMINAL NUMBER                              
         CVD   R0,DUB                                                           
         OI    DUB+L'DUB-1,X'0F'                                                
         UNPK  TRMNUM,DUB                                                       
         MVC   TRMLUID,TSYM                                                     
         MVC   TRMAGY,TAGY                                                      
*                                                                               
         CLI   SORTSW,C'N'         ANY SORT                                     
         BE    PTT03                                                            
         MVC   FULL,TSIN                                                        
         XC    FULL,EFFS                                                        
         CLC   CURDATE,FULL        SAME AS CURRENT                              
         BE    PTT03                                                            
         MVC   CURDATE,FULL                                                     
*                                                                               
         MVC   TRMLINE,=C'------------- ??????? ------------    '               
         GOTO1 VDATCON,DMCB,(3,FULL),(17,TRMAGY)                                
         LLC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         CLI   0(R2),0             EOS ON FIRST BUMP=DONE                       
         BNE   PUT031                                                           
         MVI   ANXTLIN,X'FF'                                                    
         B     PTT02                                                            
*                                                                               
PUT031   IC    R0,0(R2)                                                         
         AR    R2,R0                                                            
         CLI   0(R2),0             EOS ON 2ND BUMP=DO RIGHT SIDE                
         BNE   PUT032                                                           
         LA    R2,SRVL1BH                                                       
PUT032   ST    R2,ANXTLIN                                                       
         B     PTT02                                                            
*                                                                               
PTT03    MVC   HALF+0(1),TSYS      SET SYSTEM PROGRAM                           
         MVC   HALF+1(1),TPRG                                                   
*                                                                               
PTT04    CLI   HALF,0              CONNECTED?                                   
         BE    PTT10               NO                                           
*                                                                               
         L     R1,VSELIST          GET SELIST ENTRY                             
         LH    RE,0(R1)                                                         
         ICM   RF,15,2(R1)                                                      
         AHI   R1,6                                                             
         USING SELISTD,R1                                                       
         CLC   SESYS,HALF                                                       
         BE    *+10                                                             
         BXLE  R1,RE,*-10                                                       
         DC    H'0'                                                             
*                                                                               
         MVC   VPGMLST,SEPGMS      SAVE A(PGMLST)                               
         MVC   TRMSYS,SENAME                                                    
*                                                                               
         CLI   SEFILSET,0          TEST MULTI-SYSTEM                            
         BE    PTT06                                                            
         MVC   HALF1+0(1),SEOVSYS                                               
         MVC   HALF1+1(1),SEFILSET                                              
         DROP  R1                                                               
*                                                                               
         L     R1,VSYSLST                                                       
         LH    RE,0(R1)                                                         
         ICM   RF,15,2(R1)                                                      
         AHI   R1,6                                                             
         USING SYSLSTD,R1                                                       
         CLC   SYSLNUM,HALF1                                                    
         BE    *+10                                                             
         BXLE  R1,RE,*-10                                                       
         DC    H'0'                                                             
*                                                                               
         MVC   TRMSYS(L'SYSLSHRT),SYSLSHRT                                      
         LLC   RF,HALF1+1                                                       
         LA    RF,ALPHANUM(RF)                                                  
         MVC   TRMSYS+L'SYSLSHRT(1),0(RF)                                       
         DROP  R1                                                               
*                                                                               
PTT06    CLI   HALF+1,0            PROGRAM SET?                                 
         BE    PTT10               YES                                          
         L     R1,VPGMLST                                                       
         LH    RE,0(R1)                                                         
         ICM   RF,15,2(R1)                                                      
         AHI   R1,6                                                             
         USING PGMLSTD,R1                                                       
         CLC   PGMNUM,HALF+1                                                    
         BE    PTT08                                                            
         BXLE  R1,RE,*-10                                                       
         GOTO1 VHEXOUT,DMCB,HALF+1,TRMPRG,1,0,TO24=Y                            
         SAM31                                                                  
         B     PTT10                                                            
*                                                                               
PTT08    MVC   TRMPRG,PGMNAME                                                   
         DROP  R1                                                               
*                                                                               
PTT10    MVC   FULL,TTIME          TERMINAL TIME                                
         CLI   TIMEUNIT,0                                                       
         BE    PTT12                                                            
         L     R0,TTIME                                                         
         SRDL  R0,32                                                            
         D     R0,=F'384'                                                       
         ST    R1,FULL                                                          
*                                                                               
PTT12    GOTO1 VTIMEOUT,DMCB,(1,FULL),(X'45',WORK),TO24=Y                       
         SAM31                                                                  
         MVC   TRMTIM,WORK                                                      
*                                                                               
         TM    TSTAT6,TST6STRO                                                  
         BZ    PTT14                                                            
         MVI   TRMSTRO,C'-'        SET STEREO                                   
         TM    TSTAT6,TST6STFU                                                  
         BZ    *+8                                                              
         MVI   TRMSTRO,C'='        SET FULL STEREO                              
*                                                                               
PTT14    CLI   RPL,C'Y'            ARE WE LISTING RPLS                          
         BNE   *+14                                                             
         MVC   TRMSTAT,NEYE        PUT EYECATCHER INTO STATUS                   
         B     PUT30                                                            
*                                                                               
         TM    TSTAT2,TSTATTIP                                                  
         BZ    *+8                                                              
         MVI   TRMSTAT,C'P'        IN PROC                                      
         TM    TSTAT2,TSTATNIT                                                  
         BZ    *+8                                                              
         MVI   TRMSTAT,C'N'        VTAM TERMINAL BUILD FAILED                   
         TM    TSTAT2,TSTATBCP                                                  
         BZ    *+8                                                              
         MVI   TRMSTAT+1,C'B'      BROADCAST PENDING                            
         OC    TPRNT,TPRNT                                                      
         BZ    *+10                                                             
         MVC   TRMSTAT(2),=C'RP'                                                
         TM    TJOBFLAG,TJOBFINQ+TJOBFSUB+TJOBFOUT                              
         BZ    *+8                                                              
         MVI   TRMSTAT+2,C'J'      JOB INFO                                     
         TM    TSTAT8,TST8BINT                                                  
         BZ    *+8                                                              
         MVI   TRMSTAT+2,C'B'      BINARY INTERFACE                             
*                                                                               
PUT30    OI    FHOI,FHOITR                                                      
*                                  BUMP TO NEXT FIELD                           
         LLC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         CLI   0(R2),0             EOS ON FIRST BUMP=DONE                       
         BNE   PUT31                                                            
         MVI   ANXTLIN,X'FF'                                                    
         B     EXITOK                                                           
*                                                                               
PUT31    IC    R0,0(R2)                                                         
         AR    R2,R0                                                            
         CLI   0(R2),0             EOS ON 2ND BUMP=DO RIGHT SIDE                
         BNE   PUT32                                                            
         LA    R2,SRVL1BH                                                       
PUT32    ST    R2,ANXTLIN                                                       
         B     EXITOK                                                           
         DROP  R2,R3                                                            
         EJECT                                                                  
***********************************************************************         
* OUTPUT TOTALS LINE                                                  *         
***********************************************************************         
FMTTOTS  NTR1                                                                   
         LA    R2,SRVTOT                                                        
         MVC   0(6,R2),=C'TERMS='                                               
         LA    R2,6(R2)                                                         
         EDIT  (P3,NTRMS),(4,0(R2)),ALIGN=LEFT,ZERO=NOBLANK                     
         AR    R2,R0                                                            
         MVC   0(9,R2),=C', ACTIVE='                                            
         LA    R2,9(R2)                                                         
         EDIT  (P3,ACTRMS),(4,0(R2)),ALIGN=LEFT,ZERO=NOBLANK                    
         AR    R2,R0                                                            
         L     R3,VSSB                                                          
         USING SSBD,R3                                                          
         MVC   0(6,R2),=C', SIN='                                               
         LA    R2,6(R2)                                                         
         EDIT  (B4,SSBSIN),(7,0(R2)),ALIGN=LEFT                                 
         AR    R2,R0                                                            
         L     R3,VSELIST                                                       
         LH    R4,0(R3)                                                         
         L     R5,2(R3)                                                         
         AHI   R3,6                                                             
         USING SELISTD,R3                                                       
         SR    R1,R1                                                            
*                                                                               
         MVC   DUB(2),SEQLEN                                                    
         AH    R1,DUB                                                           
         BXLE  R3,R4,*-10                                                       
         LTR   R1,R1                                                            
         BZ    FMT2                                                             
         MVC   0(7,R2),=C', QLEN='                                              
         LA    R2,7(R2)                                                         
         EDIT  (R1),(4,0(R2)),ALIGN=LEFT                                        
         AR    R2,R0                                                            
FMT2     L     R3,VSSB                                                          
         USING SSBD,R3                                                          
         OC    SSBLSTTM,SSBLSTTM                                                
         BZ    FMT6                                                             
         L     R0,TIMENOW                                                       
         S     R0,SSBLSTTM                                                      
         LA    RF,100                                                           
         BRAS  RE,DIV                                                           
         LR    RF,R1                                                            
         L     R0,SSBSIN                                                        
         S     R0,SSBLSTSN                                                      
         MH    R0,=H'10000'                                                     
         BRAS  RE,DIV                                                           
         MVC   0(6,R2),=C', T/R='                                               
         LA    R2,6(R2)                                                         
         EDIT  (R1),(7,0(R2)),4,ALIGN=LEFT,ZERO=NOBLANK                         
         AR    R2,R0                                                            
         L     R1,SSBSIN                                                        
         S     R1,SSBLSTSN                                                      
         MVC   0(8,R2),=C', NTRNS='                                             
         LA    R2,8(R2)                                                         
         EDIT  (R1),(7,0(R2)),ALIGN=LEFT,ZERO=NOBLANK                           
*                                                                               
FMT6     OI    SRVTOTH+6,X'80'                                                  
         MVC   MSG(41),=C'(XXXX) TERMINAL CONNECT DATA DISPLAYED - '            
         MVC   MSG+1(4),SYSNAME                                                 
         GOTO1 VTIMEOUT,DMCB,(1,TIMENOW),(X'45',MSG+41),TO24=Y                  
         SAM31                                                                  
         OI    MSG+41,C'0'                                                      
         OI    SRVIDH+6,X'40'                                                   
         MVC   SRVMSG,MSG                                                       
         OI    SRVMSGH+6,X'80'                                                  
*                                  SET TIME + SIN IN SSB                        
         L     R3,VSSB                                                          
         MVC   SSBLSTSN,SSBSIN                                                  
         MVC   SSBLSTTM,TIMENOW                                                 
         B     EXIT                                                             
         DROP  R3                                                               
                                                                                
***********************************************************************         
* EXIT POINTS AND HANDY ROUTINES                                      *         
***********************************************************************         
EXITOK   CR    RB,RB                                                            
         B     EXIT                                                             
*                                                                               
EXITL    CLI   *,255                                                            
         B     EXIT                                                             
*                                                                               
EXITH    CLI   *,0                                                              
         B     EXIT                                                             
*                                                                               
EXIT     XIT1                                                                   
         ORG   *-2                                                              
         BSM   0,RE                                                             
*                                                                               
DIV      SR    R1,R1                                                            
         SRDA  R0,31                                                            
         LTR   RF,RF                                                            
         BNZ   *+8                                                              
         SR    R1,R1                                                            
         BR    RE                                                               
         DR    R0,RF                                                            
         LTR   R1,R1                                                            
         BNP   *+8                                                              
         A     R1,=F'1'                                                         
         SRL   R1,1                                                             
         BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
* TEST SPECIAL DDS FILTERS                                            *         
* R2=A(FIELD)                                                         *         
***********************************************************************         
         USING FHD,R2                                                           
TRYSPEC  NTR1                                                                   
         CLI   FHDA,C'='           TEST FOR SPECIAL DDS FUNCTIONS               
         BNE   EXITH                                                            
*                                                                               
         TM    DDS,X'01'           DDS TERMINAL                                 
         BZ    TSPCERR                                                          
         CLI   FHIL,2              SPECIAL FIELD IS =KEYWORD                    
         BL    TSPCERR                                                          
         CLI   FHIL,9              KEYWORD MUST BE 1-8 CHRS                     
         BH    TSPCERR                                                          
*                                                                               
         LA    RF,SPECTAB          TABLE OF KEYWORDS                            
         LLC   R1,FHIL                                                          
         AHI   R1,-2                                                            
*                                                                               
TSPC02   CLI   0(RF),0                                                          
         BE    TSPCERR                                                          
         EX    R1,TSPCCLC          COMPARE FIELD                                
         BE    TSPC04                                                           
         AHI   RF,L'SPECTAB        TRY  NEXT TABLE ENTRY                        
         B     TSPC02                                                           
*                                                                               
TSPCCLC  CLC   0(0,RF),FHDA+1                                                   
*                                                                               
TSPC04   LA    R1,ASPEC                                                         
         OC    0(4,R1),0(R1)       FIND FIRST EMPTY SLOT                        
         BZ    *+12                                                             
         LA    R1,4(R1)                                                         
         B     *-14                                                             
         ICM   RF,15,8(RF)         GET A(ROUTINE)                               
         A     RF,RELO                                                          
         ST    RF,0(R1)            SAVE TRUE ADDRESS                            
         B     EXITOK                                                           
*                                                                               
TSPCERR  MVI   FERN,2                                                           
         B     EXITL               KEYWORD NOT IN TABLE                         
*                                                                               
         DS    0F                                                               
SPECTAB  DS    0CL12                                                            
         DC    CL8'TSTATCLS',A(USER0001)                                        
         DC    CL8'TST4CLIP',A(USER0002)                                        
         DC    CL8'TST4TRC ',A(USER0003)                                        
         DC    CL8'TBUFF   ',A(USER0004)                                        
         DC    CL8'TSTATWRT',A(USER0005)                                        
         DC    CL8'TSTATWIP',A(USER0006)                                        
         DC    CL8'TST4UNLG',A(USER0007)                                        
         DC    CL8'TSTATDDS',A(USER0008)                                        
         DC    CL8'TSVCREQ ',A(USER0009)                                        
         DC    CL8'TST5PSWD',A(USER0010)                                        
         DC    CL8'TFLAGRTS',A(USER0011)                                        
         DC    CL8'TPERSON ',A(USER0012)                                        
         DC    CL8'TSTATAVA',A(USER0013)                                        
         DC    CL8'TBADCON ',A(USER0014)                                        
         DC    CL8'TST5TBP ',A(USER0015)                                        
         DC    CL8'TSTATLOG',A(USER0016)                                        
         DC    CL8'TSTATSSV',A(USER0017)                                        
         DC    CL8'VIOLATE ',A(USER0017)                                        
         DC    CL8'TST4HUNG',A(USER0018)                                        
         DC    CL8'TFLAGIRB',A(USER0019)                                        
         DC    CL8'TST8ASWP',A(USER0020)                                        
         DC    CL8'TSTATWEB',A(USER0021)                                        
         DC    H'0'                                                             
         EJECT                                                                  
***********************************************************************         
* USER FILTER ROUTINES  R1 & RE CANNOT BE USED HERE                   *         
* ON RETURNING TO RE IF CC=ZERO TERMINAL IS REJECTED                  *         
***********************************************************************         
         USING UTLD,R3                                                          
USER0001 TM    TSTAT3,TSTATCLS     CLSDST PENDING                               
         BR    RE                                                               
USER0002 TM    TSTAT4,TST4CLIP     CLSDST IN PROCESS                            
         BR    RE                                                               
USER0003 TM    TSTAT4,TST4TRC      VTAM TRACE ACTIVE                            
         BR    RE                                                               
USER0004 OC    TBUFF,TBUFF         BUFFER ASSIGNED TO TRM                       
         BR    RE                                                               
USER0005 TM    TSTAT3,TSTATWRT     WRITE PENDING                                
         BR    RE                                                               
USER0006 TM    TSTAT3,TSTATWIP     WRITE IN PROGRESS                            
         BR    RE                                                               
USER0007 TM    TSTAT4,TST4UNLG     UNABLE TO LOGON                              
         BR    RE                                                               
USER0008 TM    TSTAT1,TSTATDDS     DDS TERMINAL                                 
         BR    RE                                                               
USER0009 OC    TSVCREQ,TSVCREQ     TSVCREQ                                      
         BR    RE                                                               
USER0010 TM    TSTAT5,TST5PSWD     TST5PSWD                                     
         BR    RE                                                               
USER0011 TM    TFLAG,TFLAGRTS      TFLAGRTS                                     
         BR    RE                                                               
USER0012 OC    TPERSON,TPERSON     TPERSON                                      
         BR    RE                                                               
USER0013 TM    TSTATU,TSTATAVA     TEST AVAILABLE FOR RE-USE                    
         BR    RE                                                               
USER0014 CLC   TSYS(2),=X'0A00'    TEST CONTROL 00                              
         BNE   USERNO                                                           
         MVI   TSYS,0                                                           
         CLI   TSYS,255                                                         
         BR    RE                                                               
USER0015 TM    TSTAT5,TST5TBP      TEST BUILD PENDING                           
         BR    RE                                                               
USER0016 TM    TSTAT3,TSTATLOG     TEST LOGON PENDING                           
         BR    RE                                                               
USER0017 TM    TSTAT1,TSTATSSV     TEST SECURITY VIOLATIONS                     
         BR    RE                                                               
USER0018 TM    TSTAT4,TST4HUNG     TEST WRITE HUNG                              
         BR    RE                                                               
USER0019 TM    TFLAG,TFLAGIRB      TEST INHIBIT BROADCAST                       
         BR    RE                                                               
USER0020 TM    TSTAT8,TST8ASWP     TEST AUTOSWAP                                
         BR    RE                                                               
USER0021 TM    TSTAT1,TSTATWEB     TEST WEB                                     
         BR    RE                                                               
USERYES  LTR   RB,RB                                                            
         BR    RE                                                               
USERNO   CR    RB,RB                                                            
         BR    RE                                                               
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* TEST TO SEE IF FILTERING ON AN AGENCY                               *         
* R2=A(FIELD)                                                         *         
***********************************************************************         
         USING FHD,R2                                                           
TRYAG    NTR1                                                                   
         CLI   FHIL,2              MUST BE 2 LONG                               
         BNE   EXITH                                                            
*                                                                               
         CLI   FHDA,C'0'           REJECT IF BOTH NUMERIC                       
         BL    *+12                                                             
         CLI   FHDA+1,C'0'                                                      
         BNL   EXITH                                                            
*                                                                               
         CLI   FHDA,C'#'           OR NUMBER SIGN                               
         BE    EXITH                                                            
         OC    FAGY,FAGY           AND NOT ALREADY SET                          
         BZ    *+12                                                             
         MVI   FERN,1                                                           
         B     EXITL                                                            
*                                                                               
         MVC   FAGY,FHDA           SAVE AGENCY FILTER                           
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* TEST SYSTEM FILTER                                                  *         
***********************************************************************         
         USING FHD,R2                                                           
TRYSYS   NTR1                                                                   
         CLI   FSYSO,0             SYSTEM                                       
         BNE   EXITH                                                            
         CLI   FHIL,3              MUST BE BETWEEN 3 AND 7 LONG                 
         BL    EXITH                                                            
         CLI   FHIL,7                                                           
         BH    EXITH                                                            
*&&US                                                                           
         CLI   FHIL,5                                                           
         BL    TSY02                                                            
         CLC   FHDA(5),=C'PRINT'   PRINT=PRNT                                   
         BNE   TSY02                                                            
         MVC   FHDA+0(4),=CL4'PRNT'                                             
         MVC   FHDA+4(3),FHDA+5                                                 
         MVI   FHDA+8,C' '                                                      
         LLC   R0,FHIL                                                          
         BCTR  R0,0                                                             
         STC   R0,FHIL                                                          
         OI    FHOI,FHOITR                                                      
*&&                                                                             
TSY02    L     R3,VSELIST                                                       
         LH    RE,0(R3)                                                         
         L     RF,2(R3)                                                         
         AHI   R3,6                                                             
         USING SELISTD,R3                                                       
*                                                                               
         LLC   R1,FHIL                                                          
         BCTR  R1,0                                                             
         EX    R1,TSYCLC           TRY TO MATCH NAME                            
         BE    TSY04                                                            
         BXLE  R3,RE,*-8                                                        
         B     EXITH                                                            
*                                                                               
TSYCLC   CLC   FHDA(0),SENAME                                                   
*                                                                               
TSY04    MVC   FSYSO,SEOVSYS       SET OVERLAY SYS NO                           
         MVC   VPGMLST,SEPGMS                                                   
         OC    FHDA(7),SPACES                                                   
         CLC   SENAME,FHDA         IF INPUT=SYS NAME EXACTLY                    
         BNE   EXITOK                                                           
         MVC   FSYSF,SESYS         USE FILE SYS NUM ALSO                        
         B     EXITOK                                                           
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO TRY TO MATCH AGAINST A PROGRAM NAME                      *         
***********************************************************************         
         USING FHD,R2                                                           
TRYPROG  NTR1                                                                   
         CLI   FPROG,0             NEED SYSTEM, 3 CHARACTERS AND NO             
         BNE   EXITH               PROGRAM ALREADY INPUT                        
         CLI   FHIL,3                                                           
         BL    EXITH                                                            
         CLI   FSYSO,0                                                          
         BE    EXITH                                                            
*                                                                               
         L     R3,VPGMLST          FROM SYSTEM ALREADY INPUT                    
         LH    RE,0(R3)                                                         
         L     RF,2(R3)                                                         
         AHI   R3,6                                                             
         USING PGMLSTD,R3                                                       
*                                                                               
         LLC   R1,FHIL                                                          
         BCTR  R1,0                                                             
         EX    R1,TPGCLC                                                        
         BE    TPG02                                                            
         BXLE  R3,RE,*-8                                                        
         B     EXITH                                                            
*                                                                               
TPGCLC   CLC   FHDA(0),PGMNAME                                                  
*                                                                               
TPG02    MVC   FPROG,PGMNUM                                                     
         MVC   FHDA(L'PGMNAME),PGMNAME                                          
         OI    FHOI,FHOITR                                                      
         B     EXITOK                                                           
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* TEST TIME FILTER                                                    *         
***********************************************************************         
         USING FHD,R2                                                           
TRYTIM   NTR1                                                                   
         LLC   RF,FHIL             TIME                                         
         LA    RF,FHDA(RF)                                                      
         BCTR  RF,0                POINT TO LAST                                
         LHI   RE,3600                                                          
         CLI   0(RF),C'H'          HRS                                          
         BE    TTI02                                                            
         LHI   RE,60                                                            
         CLI   0(RF),C'M'          MINS                                         
         BE    TTI02                                                            
         LA    RE,1                                                             
         CLI   0(RF),C'S'          SECS                                         
         BE    TTI02                                                            
         CLI   0(RF),C'0'                                                       
         BL    EXITH                                                            
         AHI   RF,1                                                             
*                                                                               
TTI02    BCTR  RF,0                LAST BYTE OF NUMBER                          
         LA    R1,FHDA                                                          
TTI04    CLI   0(R1),C'0'                                                       
         BL    EXITH                                                            
         LA    R1,1(R1)                                                         
         CR    R1,RF                                                            
         BNH   TTI04                                                            
*                                                                               
         SR    RF,R2                                                            
         AHI   RF,-(FHDAD)                                                      
         EX    RF,*+8                                                           
         B     *+10                                                             
         PACK  DUB,FHDA(0)                                                      
*                                                                               
         CVB   R1,DUB                                                           
         MSR   R1,RE               CONVERT TO SECS                              
         MHI   R1,100                                                           
*                                                                               
         OC    FSTIM,FSTIM         MAKE SURE NOT ALREADY SET                    
         BZ    *+12                                                             
         MVI   FERN,1                                                           
         B     EXITL                                                            
*                                                                               
         L     R0,TIMENOW                                                       
         SR    R0,R1                                                            
         BNM   *+6                                                              
         XR    R0,R0                                                            
         ST    R0,FSTIM            START TIME IN 1/100 SEC                      
         SRDL  R0,32                                                            
         M     R0,=F'384'                                                       
         ST    R1,FSTIM1           START TIME IN 1/38400 SEC                    
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* TERMINAL NUMBER                                                     *         
***********************************************************************         
         USING FHD,R2                                                           
TRYTNO   NTR1                                                                   
         CLI   FHDA,C'#'           NUMBER SIGN                                  
         BNE   EXITH                                                            
         CLI   FHIL,2                                                           
         BL    EXITH                                                            
*                                                                               
         LLC   R4,FHIL                                                          
         AHI   R4,-2                                                            
         MVC   DUB,ZEROS                                                        
         EX    R4,*+8                                                           
         B     *+10                                                             
         MVZ   DUB(0),FHDA+1                                                    
         CLC   DUB,ZEROS                                                        
         BNE   EXITH                                                            
*                                                                               
         EX    R4,*+8                                                           
         B     *+10                                                             
         PACK  DUB,FHDA+1(0)                                                    
*                                                                               
         CVB   R0,DUB                                                           
         OC    FTNO,FTNO                                                        
         BZ    *+12                                                             
         MVI   FERN,1                                                           
         B     EXITL                                                            
*                                                                               
         STH   R0,FTNO                                                          
         OC    FLUID,FLUID                                                      
         BZ    *+12                INCOMP WITH LINE-ADDRESS                     
         MVI   FERN,3                                                           
         B     EXITL                                                            
*                                                                               
         MVI   SORTSW,C'N'         NO SORT                                      
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO MATCH LUID                                               *         
***********************************************************************         
         USING FHD,R2                                                           
TRYLUID  NTR1                                                                   
         CLI   FHIL,4              LUID IS 4 THRU 8 CHRS                        
         BL    EXITH                                                            
         CLI   FHIL,8                                                           
         BH    EXITH                                                            
         OC    FLUID,FLUID                                                      
         BNZ   EXITH                                                            
*                                                                               
         OC    FTNO,FTNO                                                        
         BZ    *+12                INCOMP WITH TERM NO                          
         MVI   FERN,3                                                           
         B     EXITH                                                            
*                                                                               
         MVC   FLUID,FHDA                                                       
         ST    R2,LINPARAM         SAVE ADDR OF PARAM                           
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO DISPLAY RPL CONTENTS                                     *         
***********************************************************************         
         USING FHD,R2                                                           
TRYRPL   NTR1                                                                   
         MVI   RPL,C'N'                                                         
         TM    DDS,X'01'                                                        
         BZ    EXITH                                                            
         CLI   FHIL,3                                                           
         BNE   EXITH                                                            
         CLC   FHDA(3),=C'RPL'                                                  
         BNE   EXITH                                                            
*                                                                               
         GOTO1 VLCWRITE,DMCB,VTGETRPL,0                                         
         MVC   ARPL,4(R1)          GET A(FIRST RPL)                             
         MVI   RPL,C'Y'                                                         
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO TEST MODE                                                *         
***********************************************************************         
         USING FHD,R2                                                           
TRYMODE  NTR1                                                                   
         CLI   FHIL,1              1 BYTE ONLY                                  
         BNE   TYM09                                                            
         CLI   FHDA,C'C'           CONNECTED ONLY                               
         BE    TYM02                                                            
         CLI   FHDA,C'D'           NOT CONNECTED ONLY                           
         BNE   TYM04                                                            
         MVI   SORTSW,C'N'                                                      
*                                                                               
TYM02    CLI   FCON,0                                                           
         BE    *+12                                                             
         MVI   FERN,1                                                           
         B     EXITL                                                            
*                                                                               
         MVC   FCON,FHDA                                                        
         B     EXITOK                                                           
*                                                                               
TYM04    CLI   FHDA,C'P'           IN PROCESS ONLY                              
         BNE   *+12                                                             
         MVI   FPROC,C'P'                                                       
         B     EXITOK                                                           
*                                                                               
         CLI   FHDA,C'N'           N=VTAM TERMINAL BUILD FAILED                 
         BNE   TYM06                                                            
         MVI   FVTBLD,C'N'                                                      
         MVI   SORTSW,C'N'         NO SORT                                      
         B     EXITOK                                                           
*                                                                               
TYM06    CLI   FHDA,C'*'           ONE ONLY                                     
         BNE   TYM08                                                            
         MVI   FSTAR,C'*'                                                       
         MVI   SORTSW,C'N'         NO SORT                                      
         B     EXITOK                                                           
*                                                                               
TYM08    CLI   FHDA,C'B'           BROADCAST PENDING                            
         BNE   *+12                                                             
         MVI   FBROAD,C'B'                                                      
         B     EXITOK                                                           
*                                                                               
         CLI   FHDA,C'E'           STEREO EMULATOR                              
         BNE   *+12                                                             
         OI    STEREO,TST6STRO                                                  
         B     EXITOK                                                           
*                                                                               
         CLI   FHDA,C'F'           STEREO FULL                                  
         BNE   *+12                                                             
         OI    STEREO,TST6STRO+TST6STFU                                         
         B     EXITOK                                                           
*                                                                               
TYM09    CLC   FHDA(2),=C'S='      STEREO                                       
         BNE   TYM10                                                            
         CLI   FHDA+2,C'Y'         S=Y                                          
         BNE   *+8                                                              
         OI    STEREO,TST6STRO                                                  
         CLC   FHDA+2(2),=C'YY'    S=YY                                         
         BNE   *+8                                                              
         OI    STEREO,TST6STRO+TST6STFU                                         
         B     EXITOK                                                           
*                                                                               
TYM10    LLC   R1,FHLN                                                          
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         BNE   TYM12                                                            
         CLC   FHDA(0),=C'SUBMITTED'                                            
         OI    FJOBS,TJOBFANY                                                   
         B     EXITOK                                                           
*                                                                               
TYM12    EX    R1,*+8                                                           
         BNE   TYM14                                                            
         CLC   FHDA(0),=C'READY    '                                            
         OI    FJOBS,TJOBFOUT                                                   
         B     EXITOK                                                           
*                                                                               
TYM14    B     EXITH                                                            
         EJECT                                                                  
***********************************************************************         
* PRINTER REQUESTED                                                   *         
***********************************************************************         
TRYPRNT  NTR1                                                                   
         CLI   FHIL,7                                                           
         BNE   EXITH                                                            
         CLC   FHDA(7),=C'PRINTER'                                              
         BNE   EXITH                                                            
         MVI   FPRINT,C'P'                                                      
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* SET UP FOR PRIVILEGED USER                                          *         
***********************************************************************         
PRIV     NTR1                                                                   
         L     R3,AMYUTL           R3=UTL SET CURSOR FLAG                       
         USING UTLD,R3                                                          
         XR    R0,R0               R0 IS RETURN CODE                            
         OI    TSVCREQ,X'02'                                                    
         L     RF,ATIA             USE TIA AS A(PRIV USER ID RECORD)            
         ST    RF,APVUREC                                                       
         LA    RF,2048(RF)         USE TIA+2028 AS A(USER ID LIST)              
         ST    RF,AIDLIST                                                       
         MVI   0(RF),X'FF'                                                      
         MVI   DDS,0               INITIALISE TERMINAL FLAG                     
         TM    TSTAT,X'60'                                                      
         BZ    *+12                                                             
         MVI   DDS,X'01'           SET DDS TERMINAL                             
         B     PRIVX                                                            
         TM    TSTAT7,TST7PQPU                                                  
         BZ    PRIVERR                                                          
         OI    DDS,X'02'           SET PRIVILEGED USER                          
         OC    TUSER,TUSER                                                      
         BZ    PRIVERR             MUST BE LOGGED ON                            
         MVC   USERID,TUSER                                                     
*                                                                               
PRIV2    L     RF,APVUREC          READ PRIV USER ID RECORD                     
         XC    0(25,RF),0(RF)                                                   
         MVI   0(RF),C'I'                                                       
         MVC   23(2,RF),USERID                                                  
         GOTO1 VDATAMGR,DMCB,=C'DMREAD',=C'CTFILE',(RF),(RF)                    
         CLI   8(R1),0                                                          
         BNE   PRIVERR             ERROR IF REC NOT FOUND                       
*                                                                               
PRIV3    MVC   DMCB(4),APVUREC     USE TIA FOR GETID LIST                       
         MVI   DMCB,C'C'                                                        
         MVC   DMCB+4(4),AIDLIST                                                
         MVC   DMCB+8(4),VDATAMGR                                               
         GOTO1 =V(GETIDS),DMCB,RR=RELO                                          
         B     PRIVX                                                            
*                                                                               
PRIVERR  B     EXITL               INVALID PRIVILEGED USER                      
*                                                                               
PRIVX    B     EXITOK              EXIT WITH CC EQL IS ALL OK                   
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* LITERALS AND CONSTANTS                                              *         
***********************************************************************         
         LTORG                                                                  
SPACES   DC    CL80' '                                                          
ZEROS    DC    8C'0'                                                            
EFFS     DC    8X'FF'                                                           
*                                                                               
PZERO    DC    P'0'                                                             
PONE     DC    P'1'                                                             
*                                                                               
ALPHANUM DC    C'0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ'                          
*                                                                               
ERRMSGS  DS    0CL32                                                            
         DC    CL32'MUST BE LOGGED ON'                                          
         DC    CL32'DUPLICATE PARAMETER'                                        
         DC    CL32'INVALID PARAMETER'                                          
         DC    CL32'INCOMPATIBLE PARAMETERS'                                    
         DC    CL32'INVALID LINE ID'                                            
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* DSECT TO COVER WORKING STORAGE                                      *         
***********************************************************************         
WORKD    DSECT                                                                  
DUB      DS    D                                                                
SRPARS   DS    0XL28                                                            
ASYSFAC  DS    A                   A(SYSFAC)                                    
ATIA     DS    A                   A(TIA)                                       
AMYUTL   DS    A                   A(UTL)                                       
ACOMFACS DS    A                   A(COMFACS)                                   
ASELIST  DS    A                   A(SELIST)                                    
ATWA     DS    A                   A(TWA)                                       
APHMAP   DS    A                   A(PHASE MAP)                                 
ATIOB    DS    A                   A(TRANSLATOR TWA DATA)                       
*                                                                               
BSPARS   DS    0XL32               BINSRCH PARAMETERS                           
BSPAR1   DS    F                                                                
BSPAR2   DS    F                                                                
BSPAR3   DS    F                                                                
BSPAR4   DS    F                                                                
BSPAR5   DS    F                                                                
BSPAR6   DS    F                                                                
BSPAR7   DS    F                                                                
BSPAR8   DS    F                                                                
*                                                                               
DMCB     DS    6F                                                               
*                                                                               
VHEXIN   DS    A                                                                
VHEXOUT  DS    A                                                                
VGETFACT DS    A                                                                
VSYSLST  DS    A                                                                
VBINSRCH DS    A                                                                
VDATCON  DS    A                                                                
VTIMEOUT DS    A                                                                
AERRMSGS DS    A                                                                
*                                                                               
FULL     DS    F                                                                
HALF     DS    H                                                                
HALF1    DS    H                                                                
VPGMLST  DS    V                                                                
ANXTLIN  DS    A                                                                
*                                                                               
TIMENOW  DS    F                   TIME IN 1/100 SEC                            
TIMENOW1 DS    F                   TIME IN 1/38400 SEC                          
RELO     DS    A                                                                
*                                                                               
FSTIM    DS    F                                                                
FSTIM1   DS    F                                                                
TIMEUNIT DS    X                                                                
         DS    X                                                                
FAGY     DS    CL2                                                              
FSYSF    DS    X                                                                
FSYSO    DS    X                                                                
FLUID    DS    CL8                                                              
FTNO     DS    XL2                                                              
FSTAR    DS    C                                                                
FPROC    DS    C                                                                
FVTBLD   DS    C                                                                
FCON     DS    C                                                                
FPROG    DS    X                                                                
FBROAD   DS    C                                                                
FPRINT   DS    C                                                                
FJOBS    DS    X                                                                
STEREO   DS    X                                                                
ASPEC    DS    A                   SPECIAL FILTERS                              
         DS    A                                                                
         DS    A                                                                
ASPECX   DS    A                   1-4                                          
*                                                                               
CURDATE  DS    PL3                                                              
NTRMS    DS    PL3                                                              
ACTRMS   DS    PL3                                                              
HAVLUID  DS    X                                                                
RPL      DS    C                                                                
LINPARAM DS    A                                                                
ARPL     DS    A                                                                
NSYM     DS    CL8                                                              
NEYE     DS    CL3                                                              
SORTSW   DS    X                                                                
DDS      DS    X                                                                
USERID   DS    XL2                                                              
SYSNAME  DS    CL4                                                              
MSG      DS    CL60                                                             
WORK     DS    XL256                                                            
FERN     DS    X                                                                
*                                                                               
APVUREC  DS    A                                                                
AIDLIST  DS    A                                                                
BSTAB    DS    36XL(TUTLXALN)                                                   
WORKL    EQU   *-WORKD                                                          
         EJECT                                                                  
***********************************************************************         
* DSECT TO COVER OUTPUT LINE                                          *         
***********************************************************************         
TRMLD    DSECT                                                                  
         DS    0CL46                                                            
TRMHDR   DS    CL8                                                              
TRMLINE  DS    0CL38                                                            
TRMNUM   DS    CL4                                                              
         DS    CL1                                                              
TRMLUID  DS    CL8                                                              
         DS    CL1                                                              
TRMAGY   DS    CL2                                                              
         DS    CL1                                                              
TRMSYS   DS    CL4                                                              
TRMSTRO  DS    CL1                                                              
TRMPRG   DS    CL3                                                              
         DS    CL1                                                              
TRMTIM   DS    CL8                                                              
         DS    CL1                                                              
TRMSTAT  DS    CL3                                                              
         EJECT                                                                  
         ISTDNIB                                                                
         EJECT                                                                  
***********************************************************************         
* OTHER INCLUDED DSECTS                                               *         
***********************************************************************         
* FADSECTS                                                                      
       ++INCLUDE FADSECTS                                                       
* DDFH                                                                          
       ++INCLUDE DDFH                                                           
* DDCOMFACS                                                                     
       ++INCLUDE DDCOMFACS                                                      
* FARPLD                                                                        
       ++INCLUDE FARPLD                                                         
* FAFACTS                                                                       
       ++INCLUDE FAFACTS                                                        
* FASYSLSTD                                                                     
       ++INCLUDE FASYSLSTD                                                      
         EJECT                                                                  
***********************************************************************         
* SCREEN DSECT                                                        *         
***********************************************************************         
SRLSTFFD DSECT                                                                  
         DS    CL64                                                             
* SRLSTFFD                                                                      
       ++INCLUDE SRLSTFFD                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'003SRLST00   04/15/14'                                      
         END                                                                    
