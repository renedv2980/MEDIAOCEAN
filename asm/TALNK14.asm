*          DATA SET TALNK14    AT LEVEL 001 AS OF 12/24/08                      
*PHASE T70414A                                                                  
TALNK14  TITLE '- TALENT - REPORT REQUESTS'                                     
         PRINT NOGEN                                                            
SVRDEF   LKSVR TYPE=D,CODE=CODE,REQUEST=*,SYSTEM=TALSYSQ,              *        
               BLOCKS=(B#WORKD,WORKD,B#SAVED,SAVED)                             
                                                                                
B#STFREC EQU   3                   I/O AREA FOR STAFF RECORDS                   
B#CLIREC EQU   B#STFREC            I/O AREA FOR CLIENT RECORDS                  
B#CMTREC EQU   B#STFREC            I/O AREA FOR COMMENT RECORDS                 
B#HCMREC EQU   B#STFREC            I/O AREA FOR HISTORY COMMENT RECORDS         
         EJECT                                                                  
CODE     NMOD1 0,**TA14**,RR=RE                                                 
         LR    R5,R1                                                            
         USING LP_D,R5             R5=A(LP_D)                                   
         L     R7,LP_ARUNP                                                      
         USING RUNPARMD,R7         R7=A(RUNPARMS)                               
                                                                                
         L     R9,LP_ABLK1         ROOT PASSES A(WORKD) IN BLOCK 1              
         USING WORKD,R9            R9=A(GLOBAL W/S)                             
         L     R8,LP_ABLK2         ROOT PASSES A(SAVED) IN BLOCK 2              
         USING SAVED,R8            R8=A(SAVE W/S)                               
         L     RA,ATWA                                                          
         USING TWAD,RA             RA=A(TWA)                                    
                                                                                
         ST    RE,SRVRRELO         SAVE PROGRAM RELOCATION FACTOR               
         STM   R2,RB,LP_R2RB       SAVE REGISTERS FOR SUB-ROUTINES              
         MVC   RUNMODE,RUNPMODE    EXTRACT CALLING MODE                         
         DROP  R7                                                               
         EJECT                                                                  
***********************************************************************         
* INITIALIZE FOR RUNNING                                              *         
***********************************************************************         
                                                                                
RUNSTR   CLI   RUNMODE,RRUNSTRQ    TEST 'FIRST FOR RUN' MODE                    
         BNE   PRCWRK                                                           
                                                                                
         MVC   LP_BLKS+((B#STFREC-1)*L'LP_BLKS),AIO2                            
         MVC   WVALUES(WVALUEL),LVALUES                                         
                                                                                
         J     EXITY                                                            
                                                                                
***********************************************************************         
* LVALUES MUST MATCH WVALUES                                          *         
***********************************************************************         
                                                                                
LVALUES  DS    0D                                                               
         DC    XL4'FFFFFFFF'                                                    
         DC    AL2(I#ANGDLD)       AUTOMATIC NOTICE GENERATION DOWNLOAD         
         EJECT                                                                  
***********************************************************************         
* FIRST FOR NEW WORK                                                  *         
***********************************************************************         
                                                                                
PRCWRK   CLI   RUNMODE,RPRCWRKQ    TEST 'PROCESS WORK' MODE                     
         BNE   RUNREQ                                                           
         LA    R0,REQVALS                                                       
         LHI   R1,REQVALL                                                       
         XR    RE,RE                                                            
         XR    RF,RF                                                            
         MVCL  R0,RE                                                            
         J     EXITY                                                            
         EJECT                                                                  
***********************************************************************         
* RUN A DOWNLOAD REQUEST                                              *         
***********************************************************************         
                                                                                
RUNREQ   CLI   RUNMODE,RRUNREQQ    TEST 'RUN REQUEST' MODE                      
         JNE   EXITY                                                            
                                                                                
         LA    R0,OUTVALS          CLEAR OUTPUT VALUES                          
         LHI   R1,OUTVALL                                                       
         XR    RE,RE                                                            
         XR    RF,RF                                                            
         MVCL  R0,RE                                                            
                                                                                
         MVC   COMPANY,LP_AGYB     SET COMPANY CODE                             
         MVC   CPYALPH,LP_AGY      SET COMPANY ID                               
         MVC   MAP,LP_QMAPN        SET PROCESSING MAP NUMBER                    
                                                                                
         GOTOR LP_APUTO,LP_D       CALL DDLINK OUTPUT PROCESSOR                 
                                                                                
RUNREQX  J     EXITY                                                            
         EJECT                                                                  
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
         DROP  RB                                                               
***********************************************************************         
*        PROCESS AUTO NOTICE GENERATION RECORD                        *         
***********************************************************************         
                                                                                
NXTANG   J     *+12                                                             
         DC    C'*NXTANG*'                                                      
         LR    RB,RF                                                            
         USING NXTANG,RB                                                        
                                                                                
         CLI   LP_RMODE,LP_RFRST   INITIALIZE VARIABLES                         
         JNE   NOMORE                                                           
         XC    ANGVALS(ANGVALL),ANGVALS                                         
                                                                                
         MVI   ERRTAB,X'FF'        INITIALIZE ERROR TABLE                       
                                                                                
         USING TLCOPD,R1                                                        
         LA    R1,IOKEY                                                         
         XC    TLCOPKEY,TLCOPKEY   READ FOR COMMERCIAL KEY                      
         MVI   TLCOPCD,TLCOCCDQ                                                 
         MVC   TLCOCCOM,RQANCOM                                                 
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IODIR'                                   
         JE    NANG10                                                           
         GOTOR (#ADDERR,AADDERR),DMCB,ERCIDNFO,ERRTAB                           
         J     NANG100                                                          
         DROP  R1                                                               
                                                                                
NANG10   GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOFIL+IO3'                              
         JE    *+6                                                              
         DC    H'00'                                                            
                                                                                
         USING TLCOD,R1                                                         
         L     R1,IOADDR           R1=A(COMMERCIAL RECORD)                      
         MVC   ANGAGY,TLCOAGY      SAVE AGENCY CODE                             
                                                                                
         LA    R1,TLCOELEM         R1=A(FIRST ELEMENT)                          
         DROP  R1                                                               
                                                                                
         USING TACOD,R1                                                         
         CLI   0(R1),0             BUMP TO COMM'L DETAILS ELEMENT               
         JNE   *+6                                                              
         DC    H'00'               (MUST FIND IT)                               
NANG30   CLI   0(R1),TACOELQ                                                    
         JE    NANG40                                                           
         ZIC   RF,1(R1)                                                         
         AR    R1,RF                                                            
         J     NANG30                                                           
NANG40   MVC   ANGCID,TACOCID      SAVE COMMERCIAL ID                           
         DROP  R1                                                               
                                                                                
         USING T703FFD,R2          SET UP HOLDING FEE REQUEST                   
         L     R2,AIO4             SCREEN IN AIO4                               
                                                                                
         LR    RE,R2                                                            
         LHI   RF,8                                                             
         XC    0(250,RE),0(RE)                                                  
         LA    RE,250(RE)                                                       
         BCT   RF,*-10                                                          
                                                                                
         MVI   1(R2),C'*'                                                       
         MVC   10(2,R2),LP_USRID                                                
         MVC   12(2,R2),=X'FFFF'                                                
         MVC   14(2,R2),LP_AGY                                                  
         MVC   25(5,R2),=X'02000C0B01'                                          
         MVI   36(R2),X'FA'                                                     
         MVC   38(2,R2),=X'045F'                                                
                                                                                
         MVC   64(8,R2),=X'442000010000001B'                                    
                                                                                
         MVC   132(8,R2),=X'1901003EC0098009'                                   
         MVC   140(9,R2),=C'*REPORT'                                            
                                                                                
         MVC   157(7,R2),=X'5F2A0051000080'                                     
                                                                                
         MVI   244(R2),X'01'                                                    
         MVI   248(R2),X'FF'                                                    
                                                                                
         MVC   252(8,R2),=X'0E2800A100000006'                                   
         MVC   260(6,R2),=CL6'Report'                                           
                                                                                
         MVC   266(8,R2),=X'180200A944038003'                                   
         MVC   274(8,R2),=CL8'HLD'                                              
         MVC   282(8,R2),=X'02000000FF000000'                                   
                                                                                
         MVC   290(8,R2),=X'0E2800B300000006'                                   
         MVC   298(6,R2),=C'Action'                                             
                                                                                
         MVC   304(8,R2),=X'180200BB44038006'                                   
         MVC   312(8,R2),=CL8'REPORT'                                           
         MVC   320(8,R2),=X'03000000FF000000'                                   
                                                                                
         MVC   328(8,R2),=X'0B2000C500000003'                                   
         MVC   336(3,R2),=C'Key'                                                
                                                                                
         MVC   339(8,R2),=X'320200CD00080000'                                   
         MVC   347(8,R2),RQANSTF                                                
         MVC   381(8,R2),=X'04000000FF000000'                                   
                                                                                
         MVC   389(8,R2),=X'0D2800F100000005'                                   
         MVC   397(5,R2),=C'Print'                                              
                                                                                
         MVC   402(8,R2),=X'180200F940070007'                                   
         MVC   410(7,R2),=C'SOON,XTP'                                           
         MVC   418(8,R2),=X'05000000FF000000'                                   
                                                                                
         MVC   426(8,R2),=X'0E20010300000006'                                   
         MVC   434(6,R2),=C'Output'                                             
                                                                                
         MVC   440(8,R2),=X'1802010B00000000'                                   
         MVC   456(5,R2),=X'06000000FF'                                         
                                                                                
         MVC   464(8,R2),=X'0D20011500000005'                                   
         MVC   472(5,R2),=C'Dest.'                                              
                                                                                
         MVC   477(8,R2),=X'1802011D00000000'                                   
         MVC   493(5,R2),=X'07000000FF'                                         
                                                                                
         MVC   501(8,R2),=X'0E20012700000006'                                   
         MVC   509(6,R2),=C'Others'                                             
                                                                                
         MVC   515(8,R2),=X'2002012F00000000'                                   
         MVC   539(5,R2),=X'08000000FF'                                         
                                                                                
         MVC   547(8,R2),=X'0E28019100000006'                                   
         MVC   555(6,R2),=C'Period'                                             
                                                                                
         MVC   SHFPDH,=X'2102019900000000'                                      
         MVC   SHFPDX(5),=X'0D000000FA'                                         
                                                                                
         MVC   594(8,R2),=X'0E20023100000006'                                   
         MVC   602(6,R2),=C'Agency'                                             
                                                                                
         MVC   SHFAGYH,=X'1A02023964068003'                                     
         MVC   SHFAGY,ANGAGY                                                    
         MVC   SHFAGYX(5),=X'10000000FA'                                        
                                                                                
         MVC   SHFAGYNH,=X'2C20024B00000001'                                    
                                                                                
         MVC   678(8,R2),=X'0E20028100000006'                                   
         MVC   686(6,R2),=C'Client'                                             
                                                                                
         MVC   SHFCLIH,=X'1A02028900000000'                                     
         MVC   SHFCLIX(5),=X'20000000FA'                                        
                                                                                
         MVC   SHFCLINH,=X'2C20029B00008000'                                    
                                                                                
         MVC   762(8,R2),=X'0F2002D100000007'                                   
         MVC   770(7,R2),=C'Product'                                            
                                                                                
         MVC   SHFPRDH,=X'1A0202D900000000'                                     
         MVC   SHFPRDX(5),=X'30000000FA'                                        
                                                                                
         MVC   SHFPRDNH,=X'2C2002EB00008000'                                    
                                                                                
         MVC   847(8,R2),=X'0E20032100000006'                                   
         MVC   855(6,R2),=C'Comm l'                                             
         MVI   859(R2),X'7D'                                                    
                                                                                
         MVC   SHFCOMH,=X'1C020329640C8008'                                     
         MVC   SHFCOM,ANGCID                                                    
         MVC   SHFCOMX(5),=X'40000000FA'                                        
                                                                                
         MVC   SHFCOMNH,=X'2C20033B00008012'                                    
                                                                                
         MVC   933(8,R2),=X'0F20037100000007'                                   
         MVC   941(7,R2),=C'Cli Grp'                                            
                                                                                
         MVC   SHFCLGH,=X'1C02037900000000'                                     
         MVC   SHFCLGX(5),=X'28000000FA'                                        
                                                                                
         MVC   SHFCLGNH,=X'2C20038B00008000'                                    
                                                                                
         MVC   1020(8,R2),=X'0E2003C100000006'                                  
         MVC   1028(6,R2),=C'Office'                                            
                                                                                
         MVC   SHFOFFH,=X'1A0203C900000000'                                     
         MVC   SHFOFFX(5),=X'0F000000FA'                                        
                                                                                
         MVC   SHFOFFNH,=X'2C2003DB00008000'                                    
                                                                                
         MVC   1104(8,R2),=X'0F20046100000007'                                  
         MVC   1112(7,R2),=C'Options'                                           
                                                                                
         MVC   SHFOPTH,=X'4C020469C0070000'                                     
         MVC   SHFOPT(7),=C'WEBIT=Y'                                            
         MVC   SHFOPTX(5),=X'3B000000FA'                                        
         DROP  R2                                                               
                                                                                
         GOTO1 VGETFACT,DMCB,0     GET FACPAK SYSTEM VALUES                     
                                                                                
         USING FACTSD,R1                                                        
         L     R1,DMCB                                                          
                                                                                
         USING REQD,R2                                                          
         L     R2,AIO5             SET UP REQUEST RECORD IN AIO5                
         XC    REQHDR,REQHDR                                                    
         MVC   REQREQ,SPACES                                                    
         MVC   REQREQ(2),=C'HF'                                                 
         MVC   REQREQ+2(L'LP_AGY),LP_AGY                                        
         EDIT  (4,FASIN),(6,REQREQ+5),FILL=0                                    
         DROP  R1,R2                                                            
                                                                                
         USING SPOOK,R2                                                         
         L     R2,AIO6             SET UP SPOOK RECORD IN AIO6                  
         XC    SPOOK(SPOOKXL),SPOOK                                             
         MVC   SPOOKUID,LP_USRID                                                
         MVI   SPOOKSEN,X'04'                                                   
         MVC   SPOOKERN,LP_EMSYS                                                
         MVC   SPOOKAGY,LP_AGY                                                  
         MVC   SPOOKAGX,LP_AGYB                                                 
         MVC   SPOOKDID,=C'XTP'                                                 
         MVC   SPOOKSYS,=C'TA'                                                  
         MVC   SPOOKEOD,=C'HF'                                                  
         MVC   SPOOKJCL,=C'HF'                                                  
         MVC   SPOOKWEN,=X'02'                                                  
         MVC   SPOOKXT,=C'XT='                                                  
         DROP  R2                                                               
                                                                                
         GOTO1 VREQTWA,DMCB,AIO4,AIO5,VDATAMGR,LP_ACOM,AIO6                     
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'00'                                                            
                                                                                
         L     RE,DMCB+8           GET ADDRESS OF PRTQUE KEY                    
                                                                                
         MVC   ANGRPT(3),2(RE)     BUILD XXX,9999 FOR &T SUBSTITUTION           
         MVI   ANGRPT+3,C','                                                    
         MVC   HALF1,6(RE)         SAVE REPORT NUMBER IN HALF1                  
         EDIT  HALF1,(5,ANGRPT+4),ALIGN=LEFT                                    
                                                                                
NANG100  MVI   ANGSTAT,ANGSTERR                                                 
         CLI   ERRTAB,X'FF'                                                     
         JNE   NANGX                                                            
         MVI   ANGSTAT,ANGSTOK                                                  
                                                                                
NANGX    LA    R0,ANGVALS                                                       
         STCM  R0,15,LP_ADATA                                                   
         MVI   LP_RMODE,LP_RNEXT                                                
         J     EXITY                                                            
         DROP  RB                                                               
         EJECT                                                                  
***********************************************************************         
*              ERROR ENTRIES FOR AUTO NOTICE GENERATION               *         
*              (LOOK AT ERRENTD FOR DSECT)                            *         
***********************************************************************         
                                                                                
ERCIDNFO DC    AL1(ECIDNFOX-*),AL2(1),AL1(ERRCATY3),AL1(D#ANCOM)                
         DC    C'Commercial no longer exists'                                   
ECIDNFOX EQU   *                                                                
         EJECT                                                                  
***********************************************************************         
* PROCESS RELEASE ERRORS                                              *         
***********************************************************************         
                                                                                
NXTERR   J     *+12                                                             
         DC    C'*NXTERR*'                                                      
         LR    RB,RF                                                            
         USING NXTERR,RB                                                        
                                                                                
         CLI   LP_RMODE,LP_RFRST   INITIALIZE VARIABLES                         
         JNE   NERR10                                                           
         LA    RE,ERRTAB                                                        
         ST    RE,ANXTERR                                                       
         XC    ERRVALS(ERRVALL),ERRVALS                                         
                                                                                
         USING ERRENTD,R4                                                       
NERR10   L     R4,ANXTERR          R4=A(CURRENT ERROR TABLE ENTRY)              
                                                                                
         CLI   0(R4),X'FF'         EXIT IF ALL ERRORS HAVE BEEN                 
         JE    NOMORE              PROCESSED                                    
                                                                                
         MVC   ERRNUMB,EENUMB                                                   
         MVC   ERRCATY,EECATY                                                   
         MVC   ERRFILD,EEFIELD                                                  
                                                                                
         ZIC   R0,EELEN                                                         
                                                                                
         LR    RE,R0                                                            
         SHI   RE,6                                                             
         EX    RE,*+8                                                           
         J     *+10                                                             
         MVC   ERREMSG(0),EEMSG                                                 
                                                                                
         AR    R4,R0               SAVE ADDRESS OF NEXT ERROR TABLE             
         ST    R4,ANXTERR          ENTRY                                        
                                                                                
         MVI   LP_RMODE,LP_RNEXT                                                
         LA    R0,ERRVALS                                                       
         STCM  R0,15,LP_ADATA                                                   
         J     EXITY                                                            
         DROP  R4,RB                                                            
         EJECT                                                                  
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* CONSTANTS AND LITERALS                                              *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* KEY DRIVER TABLES                                                   *         
***********************************************************************         
         EJECT                                                                  
***********************************************************************         
* EXIT CONDITIONS                                                     *         
***********************************************************************         
                                                                                
NOMORE   MVI   LP_RMODE,LP_RLAST   SET NO MORE RECORDS AND EXIT                 
         J     EXITY                                                            
EXITN    LHI   RE,0                                                             
         J     EXIT                                                             
EXITY    LHI   RE,1                                                             
EXIT     CHI   RE,1                                                             
EXITCC   XIT1  ,                                                                
         EJECT                                                                  
***********************************************************************         
* REQUEST MAP FOR AUTOMATIC NOTICE GENERATION                         *         
***********************************************************************         
                                                                                
REQANG   LKREQ H,I#ANGDLD,OUTANG                                                
RqStf    LKREQ F,D#ANSTF,(D,B#SAVED,RQANSTF),CHAR,TEXT=TA#STAFF,COL=*           
RqCom    LKREQ F,D#ANCOM,(D,B#SAVED,RQANCOM),HEXD,TEXT=TA#COMCD,COL=*           
         LKREQ E                                                                
         EJECT                                                                  
***********************************************************************         
* OUTPUT MAP - AUTO NOTICE GENERATION DOWNLOAD                        *         
***********************************************************************         
                                                                                
OUTANG   LKOUT H                                                                
                                                                                
ANGREC   LKOUT R,O#ANGSTA                   ** STATUS VALUES **                 
Array    LKOUT C,O#ANGSTA,(A,ARYANG)                                            
         LKOUT E                                                                
                                                                                
AERREC   LKOUT R,O#ANGERR                   ** ERROR VALUES **                  
Array    LKOUT C,O#ANGERR,(A,ARYERR)                                            
         LKOUT E                                                                
                                                                                
         LKOUT X                                                                
         EJECT                                                                  
***********************************************************************         
* ARRAY DEFINITION FOR ERROR MESSAGE RECORDS                          *         
***********************************************************************         
                                                                                
ARYERR   LKOUT A,(R,NXTERR),MULTIROW=Y,ROWNAME=ERRVALS                          
                                                                                
RtNum    LKOUT C,1,(D,,ERRNUMB),UBIN,ND=Y                                       
RtCat    LKOUT C,2,(D,,ERRCATY),UBIN,ND=Y                                       
RtFld    LKOUT C,3,(D,,ERRFILD),UBIN,ND=Y                                       
RtErM    LKOUT C,4,(D,,ERREMSG),CHAR,ND=Y                                       
                                                                                
         LKOUT E                                                                
         EJECT                                                                  
***********************************************************************         
* ARRAY DEFINITION FOR AUTO NOTICE GENERATION RECORDS                 *         
***********************************************************************         
                                                                                
ARYANG   LKOUT A,(R,NXTANG),ROWNAME=ANGVALS                                     
                                                                                
RtSta    LKOUT C,1,(D,,ANGSTAT),UBIN,ND=Y                                       
RtAgy    LKOUT C,2,(D,,ANGAGY),CHAR,ND=Y                                        
RtCid    LKOUT C,3,(D,,ANGCID),CHAR,ND=Y                                        
RtRpt    LKOUT C,4,(D,,ANGRPT),CHAR,ND=Y                                        
                                                                                
         LKOUT E                                                                
         EJECT                                                                  
***********************************************************************         
* SAVED STORAGE                                                       *         
***********************************************************************         
                                                                                
SAVED    DSECT                     ** DSECT TO COVER SAVED STORAGE **           
                                                                                
WVALUES  DS    0X                  ** LITERAL VALUES **                         
FFFS     DS    XL4                                                              
ANGDLD   DS    AL2                 AUTO NOTICE GENERATION DL EQUATE             
WVALUEL  EQU   *-WVALUES                                                        
                                                                                
*** REQUEST VALUES ****                                                         
                                                                                
* AUTO NOTICE GENERATION DOWNLOAD                                               
                                                                                
REQVALS  DS    0F                  ** REQUEST VALUES **                         
RQANSTF  DS    CL8                 STAFF                                        
RQANCOM  DS    XL4                 INTERNAL COMMERCIAL NUMBER                   
         ORG                                                                    
                                                                                
REQVALL  EQU   *-REQVALS                                                        
                                                                                
*** OUTPUT VALUES ****                                                          
                                                                                
OUTVALS  DS    0X                  ** OUTPUT VALUES **                          
                                                                                
* ** ERROR VALUES **                                                            
                                                                                
ERRVALS  DS    0X                                                               
ERRNUMB  DS    CL2                 NUMBER                                       
ERRCATY  DS    CL1                 CATEGORY                                     
ERRFILD  DS    CL1                 FIELD                                        
ERREMSG  DS    CL60                ERROR MESSAGE                                
ERRVALL  EQU   *-ERRVALS                                                        
                                                                                
* ** AUTO NOTICE GENERATION RECORD **                                           
                                                                                
         ORG   OUTVALS             RESET DISPLACEMENT                           
ANGVALS  DS    0X                                                               
ANGSTAT  DS    CL1                 STATUS                                       
ANGSTOK  EQU   1                   NO ERRORS                                    
ANGSTERR EQU   2                   ERRORS                                       
ANGAGY   DS    CL6                 AGENCY                                       
ANGCID   DS    CL12                COMMERCIAL ID                                
ANGRPT   DS    CL12                REPORT ID                                    
ANGVALL  EQU   *-ANGVALS                                                        
         ORG                                                                    
                                                                                
OUTVALL  EQU   *-OUTVALS                                                        
                                                                                
*** REGULAR STORAGE ***                                                         
                                                                                
RUNMODE  DS    XL(L'RUNPMODE)      RUNNER/DDLINK MODE                           
USERID   DS    XL2                 HEX USER ID                                  
COMPANY  DS    XL(L'LP_AGYB)       COMPANY CODE                                 
CPYALPH  DS    CL(L'LP_AGY)        COMPANY ALPHA ID                             
                                                                                
MAP      DS    XL2                                                              
                                                                                
SVADDR   DS    A                   SAVED AREA FOR NEEDED ADDRESSES              
         ORG   SVADDR                                                           
ANXTERR  DS    A                   A(LAST PROCESSED ERROR TABLE ENTRY)          
SVADDR2  DS    A                   SAVED AREA FOR NEEDED ADDRESSES              
                                                                                
ERRTAB   DS    XL200               ERROR MESSAGE TABLE                          
         EJECT                                                                  
*              DSECT TO COVER REQUEST RECORD                                    
         SPACE 1                                                                
REQD     DSECT                                                                  
REQHDR   DS    CL26                HEADER                                       
REQREQ   DS    CL80                REQUEST CARD                                 
         EJECT                                                                  
* INCLUDED DSECTS                                                               
         PRINT OFF                                                              
       ++INCLUDE TALNKWRK                                                       
       ++INCLUDE CTGENFILE                                                      
       ++INCLUDE FAFACTS                                                        
       ++INCLUDE DDSPOOK                                                        
       ++INCLUDE TAREPFFD                                                       
         ORG     CONTAGH                                                        
       ++INCLUDE TAREPFAD                                                       
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'001TALNK14   12/24/08'                                      
         END                                                                    
