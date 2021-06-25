*          DATA SET NEWRI73    AT LEVEL 028 AS OF 07/12/04                      
*PHASE T32073A                                                                  
*INCLUDE MOBILE                                                                 
*INCLUDE GETBROAD                                                               
*INCLUDE ADDAY                                                                  
*INCLUDE GETDAY                                                                 
*INCLUDE SPBVAL                                                                 
*INCLUDE KHDUMMY                                                                
         TITLE 'T32073 - HD HOMEDEPOT BILLING REPORT'                           
T32073   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**NE73**,RR=R2                                                 
         LA    R5,2048(RB)                                                      
         LA    R5,2048(R5)                                                      
         USING T32073,RB,R5       NOTE R5 BASE REG                              
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     RA,ATWA                                                          
         USING T320FFD,RA                                                       
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         L     R9,ASYSD                                                         
         USING NETSYSD,R9                                                       
         MVC   NBACLI,ANETWS1     CLIENT RECORD IN ANETWS1                      
         L     R7,ANETWS2         ANETWS2+250 =WORKING STORAGE                  
         LA    R7,500(R7)                                                       
         USING MYD,R7                                                           
         ST    R2,RELO                                                          
         SPACE 1                                                                
         CLI   MODE,PRINTREP                                                    
         BNE   RP2                                                              
                                                                                
         L     R1,=A(UDEFLST)                                                   
         ST    R1,NDUDEFD                                                       
         L     R1,=A(MYIO)                                                      
         ST    R1,AMYIO                                                         
         L     R1,=A(NETBUFF)                                                   
         ST    R1,NBANBUFF                                                      
                                                                                
         BAS   RE,REPMOD                                                        
         B     XIT                                                              
*                                                                               
RP2      CLI   MODE,VALREC                                                      
         BNE   RP4                                                              
         BAS   RE,EDITMOD                                                       
         B     XIT                                                              
RP4      EQU   *                                                                
XIT      XIT1                                                                   
         EJECT                                                                  
*              EDIT ROUTINES                                                    
         SPACE 3                                                                
EDITMOD  NTR1                                                                   
         MVI   NBQINIT,0           DO ALL VALIDATIONS EACH TIME                 
         MVI   NBDATA,C'U'         UNIT RECORDS ONLY                            
                                                                                
         MVI   FTERMFLG,0          FOLLOWING FIELDS ARE REQUIRED                
                                                                                
         LA    R2,SPLCLIH                CLIENT                                 
         NETGO NVCLIALL,DMCB                                                    
                                                                                
         LA    R2,SPLPROH                PRODUCT                                
         NETGO NVPRDALL,DMCB                                                    
         MVI   POLRUN,C'N'                                                      
         CLC   =C'POL',NBSELPRD                                                 
         BNE   *+8                                                              
         MVI   POLRUN,C'Y'                                                      
                                                                                
         LA    R2,SPLESTH                ESTIMATE                               
         NETGO NVESTRNG,DMCB                                                    
                                                                                
         LA    R2,SPLNETH                NETWORK                                
         NETGO NVNETALL,DMCB                                                    
                                                                                
         LA    R2,SPLPRDH                PERIOD                                 
         NETGO NVGETFLD,DMCB                                                    
         GOTO1 DATVAL,DMCB,(2,SPLPRD),NBSELSTR        MMMYY TO YYMM01           
         OC    DMCB(4),DMCB                                                     
         BZ    EDINV                                                            
         MVC   PERIODSV,SPLPRD                            SAVE PERIOD           
         ZIC   R1,SPLPRDH+5        GET INPUT LENGTH                             
         BCTR  R1,0                                                             
         LA    R2,PERIODSV                                                      
         AR    R2,R1                                                            
         CLI   0(R2),C'B'          CHECK BROADCAST MONTH REQUEST                
         BNE   EDT08                                                            
* - BRAODCAST MONTHS                                                            
         MVI   0(R2),X'40'         CLEAR C'B' FROM PERIODSV                     
         MVC   WORK(6),NBSELSTR                                                 
         MVC   WORK+4(2),=X'F1F5'                                               
         L     R2,=V(GETDAY)                                                    
         A     R2,RELO                                                          
         L     R3,=V(ADDAY)                                                     
         A     R3,RELO                                                          
         GOTO1 =V(GETBROAD),DMCB,(1,WORK),NBSELSTR,(R2),(R3),RR=RELO            
         CLI   DMCB,X'FF'                                                       
         BNE   *+6                                                              
         DC    H'0'                                                             
         B     EDT11                                                            
         EJECT                                                                  
* - CALENDAR MONTHS                                                             
EDT08    MVC   NBSELSTR+4(2),=X'F0F1'                                           
         MVC   DMCB+8(4),=F'31'                                                 
EDT10    GOTO1 ADDAY,DMCB,NBSELSTR,NBSELEND           NEED YYMMDD               
         CLC   NBSELSTR+2(2),NBSELEND+2               IF MONTHS=, OK            
         BE    EDT11                                                            
         L     R1,DMCB+8                                                        
         BCTR  R1,0                                                             
         ST    R1,DMCB+8                                                        
         C     R1,=F'27'                                                        
         BNL   EDT10                                                            
         DC    H'0'                                                             
* - SET UP COMPRESSED START/END FOR NETIO                                       
EDT11    GOTO1 DATCON,DMCB,(0,NBSELSTR),(2,NBCMPSTR)                            
         GOTO1 DATCON,DMCB,(0,NBSELEND),(2,NBCMPEND)                            
* - FORCE NETIO TO FILTER DATES                                                 
         MVI   NBDONTFD,0                                                       
         EJECT                                                                  
EDT12    DS    0H                                                               
*                                                                               
         MVI   FTERMFLG,1          FOLLOWING FIELDS ARE OPTIONAL                
*                                                                               
EDT14    LA    R2,SPLOPTH          OPTIONS                                      
         NETGO NVGETFLD,DMCB                                                    
         BZ    EDT20                                                            
         GOTO1 SCANNER,DMCB,(R2),(10,BLOCK)                                     
         LA    R3,BLOCK                                                         
         ZIC   R0,DMCB+4                                                        
         LTR   R0,R0                                                            
         BZ    EDINV                                                            
         SPACE 1                                                                
EDT16    CLC   12(4,R3),=C'DOWN'   OPTION TO DOWNLOAD                           
         BNE   OPT3                                                             
         MVI   DOWNOPT,C'Y'                                                     
         B     EDT18                                                            
OPT3     MVI   PEROPT,0                                                         
*        CLC   12(3,R3),=C'PER'    PERIOD OPTION                                
*        BNE   OPT5                                                             
*        MVC   PEROPT,22(R3)                                                    
*        CLI   PEROPT,C'C'         CAN BE CALENDAR                              
*        BE    EDT18                                                            
*        CLI   PEROPT,C'B'         BROADCAST                                    
*        BE    EDT18                                                            
*        CLI   PEROPT,C'S'         OR SPECIAL                                   
*        BE    EDT18                                                            
*        B     EDINV               OTHERWISE ITS NO GOOD                        
OPT5     DS    0H                                                               
                                                                                
OPTINV   B     EDINV                                                            
                                                                                
EDT18    LA    R3,32(R3)                                                        
         BCT   R0,EDT16                                                         
         B     EDT20                                                            
         SPACE 1                                                                
EDT20    LA    R2,SPLDSNH          DOUBLE SPACE NETWORKS                        
*        CLI   5(R2),0                                                          
*        BE    EDT22                                                            
*        CLI   SPLDSN,C'Y'                                                      
*        BE    EDT21                                                            
*        CLI   SPLDSN,C'N'                                                      
*        BNE   EDINV                                                            
*EDT21    MVC   DOUBLNET,SPLDSN                                                 
         B     EDT22                                                            
*                                                                               
EDT22    MVI   SUPRSDET,C'Y'        SUPPRESS DETAIL COSTS                       
         LA    R2,SPLSDCH                                                       
         CLI   5(R2),0                                                          
         BE    EDT24                                                            
         CLI   8(R2),C'Y'                                                       
         BE    EDT23                                                            
         CLI   8(R2),C'N'                                                       
         BNE   EDINV                                                            
EDT23    MVC   SUPRSDET,8(R2)                                                   
         B     EDT24                                                            
         SPACE 1                                                                
EDT24    MVI   PMKTRECP,C'Y'        PRINT MARKET RECAP                          
         LA    R2,SPLPMKH                                                       
         CLI   5(R2),0                                                          
         BE    EDT26                                                            
         CLI   8(R2),C'Y'                                                       
         BE    EDT26                                                            
         CLI   8(R2),C'N'                                                       
         BNE   EDINV                                                            
         MVI   PMKTRECP,C'N'                                                    
         SPACE 1                                                                
EDT26    MVI   PINVREP,C'Y'         PRINT INVOICE REPORT                        
         LA    R2,SPLPIRH                                                       
         CLI   5(R2),0                                                          
         BE    EDT28                                                            
         CLI   8(R2),C'Y'                                                       
         BE    EDT28                                                            
         CLI   8(R2),C'N'                                                       
         BNE   EDINV                                                            
         MVI   PINVREP,C'N'                                                     
         SPACE 1                                                                
EDT28    MVI   PREBILL,C'N'         PRE-BILL                                    
         LA    R2,SPLPRBH                                                       
         CLI   5(R2),0                                                          
         BE    EDT30                                                            
         CLI   8(R2),C'N'                                                       
         BE    EDT30                                                            
         CLI   8(R2),C'Y'                                                       
         BNE   EDINV                                                            
         MVI   PREBILL,C'Y'                                                     
         SPACE 1                                                                
EDT30    LA    R2,SPLUDDH           UNBILLED/DUE DATE                           
         CLI   5(R2),0                                                          
         BE    EDT32                                                            
         GOTO1 DATVAL,DMCB,(0,SPLUDD),WORK                                      
         ZIC   R1,5(R2)                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   UNBILDUD(0),SPLUDD                                               
         B     EDT32                                                            
         SPACE 1                                                                
EDT32    LA    R2,SPLINDH           INVOICE DATE                                
         CLI   5(R2),0                                                          
         BE    EDTX                                                             
         GOTO1 DATVAL,DMCB,(0,8(R2)),(X'80',INVDATSV)                           
         OC    DMCB(4),DMCB                                                     
         BZ    EDINV                                                            
         MVC   WORK(6),INVDATSV                  NEED MM/DD/YY                  
         MVC   INVDATSV(2),WORK+2                                               
         MVI   INVDATSV+2,C'/'                                                  
         MVC   INVDATSV+3(2),WORK+4                                             
         MVI   INVDATSV+5,C'/'                                                  
         MVC   INVDATSV+6(2),WORK                                               
         B     EDTX                                                             
         SPACE 1                                                                
EDTX     LA    R2,SPLCLIH                                                       
         XIT1  REGS=(R2)                                                        
*                                                                               
*                                                                               
EDINV    DS    0H                                                               
         MVI   ERROR,INVALID                                                    
         B     EDERR                                                            
         PRINT GEN                                                              
EDERR    GOTO1 ERREX                                                            
         PRINT NOGEN                                                            
         EJECT                                                                  
*              REPORT INITIALIZATION                                            
         SPACE 3                                                                
REPMOD   NTR1                                                                   
*                                         INITIALIZATION FOR DRIVER             
         SPACE 1                                                                
         GOTO1 CALLOV,DMCB,0,X'D9031EB7'  LOAD T31EB7 (GLOBAL STORAGE)          
         L     R6,DMCB                                                          
         USING GLOBALD,R6                                                       
         GOTO1 CALLOV,DMCB,0,X'D9000A3A'  LOAD T00A3A (DRIVER)                  
         MVC   DRIVER,DMCB                                                      
         GOTO1 CALLOV,DMCB,0,X'D9000A4D'  LOAD T00A4D (NETWORK DRIVER)          
         MVC   GLASYSDR,DMCB                                                    
         GOTO1 CALLOV,DMCB,0,X'D9032074'  LOAD T32074 (DPG PHASE)               
         MVC   GLAPROG,DMCB                                                     
         SPACE 1                                                                
         ST    RC,GLAWORKD                                                      
         MVI   GLTWORKD,GLTSPOOL                                                
         LA    R2,HEDSPECS                                                      
         ST    R2,SPECS                                                         
         LA    R2,HOOK                                                          
         ST    R2,GLAHOOK                                                       
         MVI   GLFHEADL,13                                                      
         MVC   GLOPTS,MYOPTS       MOVE MYOPTS DATA TO GLOPTS                   
***      MVI   GLTRACE,C'Y'         TRACE                                       
         OI    GLINDS,X'04'         PRINT ALL TOTALS                            
         MVI   GLLFTOPT,C'Y'        LEFT OPTION                                 
         MVI   GLBOXOPT,C'N'        NO BOX OPTION                               
         MVI   GLGAP,2              GAP=2                                       
         MVI   NDNAROPT,C'Y'        NARROW OPTION                               
         BAS   RE,GETAAANM          GET BILLING ADDRESS                         
         EJECT                                                                  
*              NOW CONTROL NETIO                                                
         SPACE 3                                                                
         MVI   RCSUBPRG,1                                                       
         MVC   NBPEROVR,PEROPT     OPTIONAL PROFILE OVERRIDE                    
         MVI   NBSPLOPT,X'C0'      OPTION TO SPLIT EVEN IF POL                  
         SPACE 1                                                                
         SPACE 1                                                                
         MVI   GLMODE,GLINIT       INITIALIZE DRIVER                            
         GOTO1 DRIVER,DMCB,(R6)                                                 
         EJECT                                                                  
*              INPUT - PROGRAM I/O                                              
         SPACE 3                                                                
GETUNIT  NETGO NSNETIO,DMCB,NETBLOCK                                            
         CLI   NBMODE,NBPROCUN                                                  
         BNE   *+8                                                              
         BAS   RE,GOTOBHRD                                                      
         CLI   NBMODE,NBREQLST                                                  
         BE    ALLDONE                                                          
         CLI   NBERROR,0                                                        
         BE    GETUNIT                                                          
         DC    H'0'                                                             
                                                                                
*                                  GO AND DO THE OUTPUT                         
ALLDONE  MVI   GLMODE,GLOUTPUT                                                  
         GOTO1 DRIVER,DMCB,(R6)                                                 
         B     XIT                                                              
         EJECT                                                                  
*                                                                               
MYHEAD   NTR1                                                                   
         B     XIT                                                              
                                                                                
*              HEADLINE SPECS                                                   
         SPACE 3                                                                
         PRINT NOGEN                                                            
HEDSPECS SPROG 0,1,2,3                                                          
         SSPEC H4,1,C'MEDIA    NETWORK T.V.'                                    
         SSPEC H5,1,C'CLIENT'                                                   
         SSPEC H6,1,C'PRODUCT'                                                  
         SSPEC H7,1,C'ESTIMATE'                                                 
         DC    X'00'                                                            
         EJECT                                                                  
*                                                                               
* - HOOK FROM DRIVER COMES HERE                                                 
HOOK     NTR1                                                                   
*                                                                               
         CLI   GLHOOK,GLHEAD              HEADLINES                             
         BNE   HK10                                                             
HOOK10   DS    0H                                                               
         B     HKXIT                                                            
         SPACE                                                                  
*                                                                               
* - EXPECTS R1 POINT TO OUT AREA                                                
* - MOVES IN 4 LINES OF BILLADDRESS/ 30 CHARACTERS PER LINE                     
MOVIT    NTR1                                                                   
         LA    R3,4                SET BILL ADDRESS                             
         LA    R2,BILADRSV                                                      
MV10     MVC   0(30,R1),0(R2)                                                   
         LA    R1,132(R1)                                                       
         LA    R2,30(R2)                                                        
         BCT   R3,MV10                                                          
         B     XIT                                                              
         EJECT                                                                  
*                                                                               
* - RESOLVE ROUTINE ADDRESSES                                                   
HK10     CLI   GLHOOK,GLRESOLV                                                  
         BNE   HK14                                                             
         LA    R2,ROUTABLE         GET TABLE OF ROUTINES                        
HK11     CLC   0(8,R2),GLLABEL                                                  
         BE    HK12                                                             
         LA    R2,12(R2)                                                        
         CLI   0(R2),0             IF EOF                                       
         BE    HKXIT               GET OUT                                      
         B     HK11                                                             
HK12     MVC   GLAROUT,8(R2)       SET ROUTINE ADDRESS                          
         B     HKXIT                                                            
*                                                                               
* - GO TO ROUTINE                                                               
HK14     CLI   GLHOOK,GLROUT                                                    
         BNE   HKXIT                                                            
         CLI   GLMODE,GLOUTPUT                                                  
         BNE   HK30                                                             
         LA    R2,ROUTABLE                                                      
HK15     CLC   0(8,R2),GLLABEL                                                  
         BE    HK16                                                             
         LA    R2,12(R2)                                                        
         CLI   0(R2),0                                                          
         BE    HKXIT                                                            
         B     HK15                                                             
HK16     L     RF,GLAROUT                                                       
         BR    RF                                                               
HK20     DS    0H                                                               
*                                                                               
HK30     CLI   GLMODE,GLINPUT                                                   
         BNE   HKXIT                                                            
         LA    R2,ROUTABLE                                                      
HK35     CLC   0(8,R2),GLLABEL                                                  
         BE    HK40                                                             
         LA    R2,12(R2)                                                        
         CLI   0(R2),0                                                          
         BE    HKXIT                                                            
         B     HK15                                                             
HK40     L     RF,GLAROUT                                                       
         BR    RF                                                               
                                                                                
HKXIT    B     XIT                                                              
         EJECT                                                                  
         DS    0F                                                               
ROUTABLE DS    0CL12                                                            
         DC    C'MYOTCLI ',A(MYOTCLI)                                           
         DC    C'MYOTCLI2',A(MYOTCLI2)                                          
         DC    C'MYOTCLI3',A(MYOTCLI3)                                          
         DC    C'MYOTPRD ',A(MYOTPRD)                                           
         DC    C'MYOTEST ',A(MYOTEST)                                           
         DC    C'MYOMED  ',A(MYOMED)                                            
         DC    C'MYOITOT ',A(MYOITOT)                                           
         DC    C'MYRCSUB ',A(MYRCSUB)                                           
         DC    C'MYRCSUB1',A(MYRCSUB1)                                          
         DC    C'MYRCSUB3',A(MYRCSUB3)                                          
         DC    C'MYDETOUT',A(MYDETOUT)                                          
         DC    C'MYDETOT2',A(MYDETOT2)                                          
         DC    C'MYINBFR ',A(MYINBFR)                                           
         DC    C'MYOTBFR ',A(MYOTBFR)                                           
         DC    C'MYD30   ',A(MYD30)                                             
         DC    C'HDREP1  ',A(HDREP1)                                            
         DC    C'HDREP2  ',A(HDREP2)                                            
         DC    C'HDREP3  ',A(HDREP3)                                            
         DC    X'00'                                                            
         EJECT                                                                  
* IN ROUTINES                                                                   
*                                                                               
* - BILL FORMULA                                                                
MYINBFR  DS    0H                                                               
         L     R2,GLAIFLD                                                       
         MVC   0(5,R2),BILLFRML                                                 
         B     XIT                                                              
                                                                                
         EJECT                                                                  
*                                                                               
* - CLIENT OUT FOR FIRST REPORT                                                 
MYOTCLI  DS    0H                                                               
         BAS   RE,DOCLT            CLIENT DETAILS                               
         B     HDREP1              REPORT 1 HEADLINES                           
                                                                                
* - CLIENT OUT FOR SECOND REPORT                                                
MYOTCLI2 DS    0H                                                               
         BAS   RE,DOCLT            CLIENT DETAILS                               
         B     HDREP2              REPORT 2 HEADLINES                           
                                                                                
* - CLIENT OUT FOR THIRD REPORT                                                 
MYOTCLI3 DS    0H                                                               
***      OI    GLINDS2,GLPWHOLE    DONT COLLAPSE LINES                          
         BAS   RE,DOCLT            CLIENT DETAILS                               
         B     HDREP3              REPORT 3 HEADLINES                           
                                                                                
* - COMMON CLIENT-OUT ROUTINE                                                   
DOCLT    NTR1                                                                   
         L     R3,GLAOFLD                                                       
         L     R2,GLAIFLD                                                       
         MVC   0(3,R3),3(R2)       CLIENT CODE                                  
         MVI   WORK,1              PASS CLIENT FLAG                             
         MVC   WORK+1(3),0(R2)     AND A/M + CLT CODE                           
         BAS   RE,GETNAMES         RETURNS CLIENT NAME IN WORK                  
         MVC   4(20,R3),WORK                                                    
         B     XIT                                                              
                                                                                
* - PRODUCT OUT                                                                 
MYOTPRD  DS    0H                                                               
         L     R3,GLAOFLD                                                       
         L     R2,GLAIFLD                                                       
         MVC   0(3,R3),3(R2)                                                    
         MVI   WORK,2              PASS PROD FLAG                               
         MVC   WORK+1(3),3(R2)     AND PROD CODE                                
         BAS   RE,GETNAMES         RETURNS PROD NAME IN WORK                    
         MVC   4(20,R3),WORK                                                    
         B     XIT                                                              
                                                                                
* - ESTIMATE OUT                                                                
MYOTEST  DS    0H                                                               
         L     R2,GLAIFLD                                                       
         L     R3,GLAOFLD                                                       
         EDIT  (B1,0(R2)),(3,0(R3)),ALIGN=LEFT   ESTIMATE NUMBER                
         CLI   POLRUN,C'Y'         IF NO PROD BREAK                             
*        BE    XIT                 EXIT/ SINCE PRODUCT NOT IN SORT              
         BNE   *+10                EXIT/ SINCE PRODUCT NOT IN SORT              
         MVC   MYKEY+4(3),=C'POL'                                               
         MVI   WORK,3              ELSE/ PASS EST FLAG                          
         MVC   WORK+1(1),0(R2)     AND EST NUMBER                               
         BAS   RE,GETNAMES         RETURNS EST NAME IN WORK                     
         MVC   4(20,R3),WORK                                                    
         B     XIT                                                              
                                                                                
         EJECT                                                                  
* - MEDIA OUT                                                                   
MYOMED   DS    0H                                                               
         L     R2,GLAIFLD                                                       
         L     R3,GLAOFLD                                                       
         CLI   0(R2),C'N'                                                       
         BNE   *+14                                                             
         MVC   0(18,R3),=C'NATIONAL BROADCAST'                                  
         B     MOMX                                                             
         CLI   0(R2),C'S'                                                       
         BNE   *+14                                                             
         MVC   0(11,R3),=C'SYNDICATION'                                         
         B     MOMX                                                             
         MVC   0(14,R3),=C'NATIONAL CABLE'                                      
         CLI   0(R2),C'C'                                                       
         BE    MOMX                                                             
         MVC   0(14,R3),=C'****UNKNOWN***'                                      
MOMX     B     XIT                                                              
                                                                                
* - DOLLAR DETAILS                                                              
MYDETOUT DS    0H                                                               
         L     R2,GLAIFLD                                                       
         L     R3,GLAOFLD                                                       
         TM    GLINDS,GLTOTLIN     IF IT'S TOTALS                               
         BNO   MYDT10                                                           
* - PROD=POL DROPS PRODUCT FROM KEY SO MEDIA BREAK = LEVEL 5                    
         CLI   POLRUN,C'Y'         IF POL                                       
         BNE   MDED5                                                            
         CLI   GLLEVEL,5           LEVEL = 5 = MEDIA TOTAL ONLY                 
         BE    MYDT5                                                            
         B     MDTX                                                             
* - PROD = ALL HAS PROD IN KEY SO MEDIA BREAK = LEVEL 6                         
MDED5    CLI   GLLEVEL,6           PROD=ALL=LEVEL=6                             
         BE    MYDT5                                                            
         B     MDTX                                                             
                                                                                
MYDETOT2 DS    0H                                                               
         L     R2,GLAIFLD                                                       
         L     R3,GLAOFLD                                                       
         TM    GLINDS,GLTOTLIN     IF IT'S TOTALS                               
         BNO   MYDT10                                                           
* - IF POL, PROD NOT IN KEY SO ONE LESS LEVEL FOR INVOICE BREAK                 
         CLI   POLRUN,C'Y'                                                      
         BE    MYDT4                                                            
         CLI   GLLEVEL,4                                                        
         BE    MYDT5                                                            
         B     MDTX                                                             
MYDT4    CLI   GLLEVEL,3           INVOICE TOTALS ONLY                          
         BNE   MDTX                                                             
MYDT5    CLI   PREBILL,C'Y'                                                     
         BE    MYD30                                                            
         B     MYD50                                                            
                                                                                
* - COMMON CODE FOR MYDETOUT AND MYDETOT2                                       
MYDT10   MVI   0(R3),0                                                          
         CLI   SUPRSDET,C'Y'       SUPPRESS DETAILS(DEFAULT)                    
         BE    MDTX                                                             
         CLI   PREBILL,C'Y'                                                     
         BNE   MYD50                                                            
                                                                                
* SPECIAL CODE FOR PREBILLING                                                   
MYD30    DS    0H                   ONLY GETS HERE FOR PRE-BILLING              
         L     R2,GLAIFLD                                                       
         L     R3,GLAOFLD                                                       
         ZAP   PAKWRK,1(8,R2)           PUT COST INTO PAK WORK AREA             
         CLI   BFRFLAG,1           IS THIS 2ND COLUMN?                          
         BNE   MYD32                                                            
         MVI   BFRFLAG,0           YES- CLEAR FLAG                              
         ICM   R1,15,9(R2)         GET BILLFORMULA                              
         B     MYD40                                                            
MYD32    MVI   BFRFLAG,1           NO 1ST COLUUMN                               
         ICM   R1,15,18(R2)        GET BILL FORMULA                             
*                                                                               
MYD40    CVD   R1,DUB                                                           
         MP    PAKWRK,DUB+4(4)        RATE X COST                               
         DP    PAKWRK,=P'1000000'     DROP PERCENT                              
         ZAP   DUB,1(8,R2)            GET COST                                  
         AP    DUB,PAKWRK+4(8)        ADD RATE                                  
         ZAP   PAKWRK(8),DUB                                                    
         LA    R2,PAKWRK              FUDGE R2                                  
         B     MYD50                                                            
*                                                                               
BFRFLAG  DS    CL1                                                              
* END SPECIAL CODE FOR PRE-BILLING                                              
                                                                                
                                                                                
*                                                                               
MYD50    EDIT  (P8,0(R2)),(12,0(R3)),2,MINUS=YES,ZERO=NOBLANK                   
***      GOTO1 =V(PRNTBL),DMCB,=C'EDT',0(R2),C'DUMP',20,=C'1D'                  
MDTX     B     XIT                                                              
                                                                                
                                                                                
                                                                                
* BILL FORMULA - DON'T PRINT IT - CARRIED FOR USE BY MYDETOT                    
MYOTBFR  DS    0H                                                               
         B     XIT                                                              
                                                                                
         EJECT                                                                  
                                                                                
* - INVOICE TOTAL LITERALS                                                      
MYOITOT  DS    0H                                                               
         L     R3,GLAOFLD                                                       
         MVC   0(24,R3),=C'** INVOICE AMOUNT DUE **'                            
         LA    R3,198(R3)                                                       
         MVI   0(R3),9                                                          
         LA    R3,198(R3)                                                       
         MVC   0(28,R3),=C'DUE AND PAYABLE ON RECEIPT  '                        
         MVC   30(10,R3),UNBILDUD                                               
         B     XIT                                                              
                                                                                
* - BUMP RCSUBPRG                                                               
MYRCSUB  DS    0H                                                               
         ZIC   R1,RCSUBPRG                                                      
         LA    R1,1(R1)                                                         
         STC   R1,RCSUBPRG                                                      
         B     XIT                                                              
MYRCSUB1 DS    0H                                                               
         MVI   RCSUBPRG,1                                                       
         B     XIT                                                              
MYRCSUB3 DS    0H                                                               
         MVI   RCSUBPRG,3                                                       
         B     XIT                                                              
                                                                                
         EJECT                                                                  
                                                                                
HDREP1   DS    0H                                                               
         MVC   H1+26(L'USERNAME),USERNAME                                       
         MVC   H2+24(L'USERADDR),USERADDR                                       
         CLI   PREBILL,C'Y'                                                     
         BE    HDREP12                                                          
         MVC   H1+59(10),=C'INVOICE N-'                                         
         MVC   H6+29(12),=C'INVOICE DATE'                                       
         MVC   H6+42(8),INVDATSV                                                
HDREP12  MVC   H4+31(21),=C'NETWORK MEDIA BILLING'                              
         MVC   H8(17),=C'FOR THE PERIOD OF'                                     
         MVC   H8+19(6),PERIODSV                                                
         MVC   H9(38),=C'**ORDERED AMOUNTS ARE BASED ON NETWORK'                
         MVC   H9+40(20),=C'INVOICE CLEARANCES**'                               
         LA    R1,H3                                                            
         LA    R1,59(R1)                                                        
         BAS   RE,MOVIT            SET IN NAME/ADDR                             
         B     XIT                                                              
HDREP2   DS    0H                                                               
         MVC   H6+29(22),=C'*** NATIONAL RECAP ***'                             
         CLI   PREBILL,C'Y'                                                     
         BE    *+10                                                             
         MVC   H8(10),=C'INVOICE N-'                                            
         MVC   H3+29(17),=C'FOR THE PERIOD OF'                                  
         MVC   H3+47(6),PERIODSV                                                
         MVC   H1+28(27),=C'WESTERN INTERNATIONAL MEDIA'                        
         MVC   H2+28(27),=C'----------------------------'                       
         LA    R1,H1                                                            
         LA    R1,59(R1)                                                        
         BAS   RE,MOVIT            SET IN NAME/ADDR                             
         B     XIT                                                              
*                                                                               
HDREP3   DS    0H                                                               
         MVC   H1+29(27),=C'WESTERN INTERNATIONAL MEDIA'                        
         MVC   H2+29(27),=C'----------------------------'                       
         MVC   H3+29(17),=C'FOR THE PERIOD OF'                                  
         MVC   H3+47(6),PERIODSV                                                
         MVC   H5+29(7),=C'VENDOR:'                                             
         CLI   PREBILL,C'Y'                                                     
         BE    *+10                                                             
         MVC   H8(10),=C'INVOICE N-'                                            
         LA    R1,H1                                                            
         LA    R1,59(R1)                                                        
         BAS   RE,MOVIT            SET IN NAME/ADDR                             
         B     XIT                                                              
         EJECT                                                                  
*                                                                               
*          DATA SET NEWRI82    AT LEVEL 031 AS OF 06/02/94                      
* -  READS UNIT BILLING ELEMENTS AND PASS TO DRIVER                             
*                                                                               
*                                                                               
* - RDBELEM READS THROUGH THE UNIT BILLING ELEMENTS AND GETS                    
* - BILLING RECORD THAT MATCHES. THEN IT GOES TO DRIVER.                        
*                                                                               
* - THUS IT GOES TO DRIVER MULTIPLE TIMES FOR A SINGLE UNIT.                    
*                                                                               
* - NBRDBELS = 2 AFTER 1ST DRIVER CALL TO TURN OFF ACCGEN ROUTINES              
* - IN NEWRIDRIVE SO ADDITIVE COLUMNS (E.G. ACTUAL COST) ARE                    
* - ONLY PASSED ONCE FOR EACH UNIT.                                             
*                                                                               
*                                                                               
**********************************************************************          
GOTOBHRD NTR1                                                                   
*                                                                               
         TM    NBUNITST,X'42'      SKIP MISSED/PREEMPT                          
         BNZ   XIT                                                              
*                                                                               
RDBELEM  DS    0H                                                               
* - SET UP I/O AREAS                                                            
         MVC   AIO2,AIO                                                         
         USING BHBLOCK,R3                                                       
         L     R3,AMYIO                                                         
         ST    R3,AIO                                                           
         A     R3,=F'2000'                                                      
         ST    R3,NDCIDTBL       PASS ADDR TO DRIVER                            
*                                SHARE COMMERCIAL CLASS TABLE ADDR              
         LR    RE,R3                                                            
         L     RF,=F'1000'                                                      
         XCEF                                                                   
                                                                                
         MVI   NBRDBELS,0          CLEAR FLAG                                   
         L     R4,NBAIO            GET FIRST BILLING ELEMENT                    
         MVI   ELCODE,X'10'                                                     
         BAS   RE,GETEL                                                         
         BE    RDB5                                                             
         CLI   PREBILL,C'Y'        NO BILLING/IF REQUESTING PREBILL             
         BE    GB58                GO ON                                        
         BNE   XIT                 ELSE EXIT                                    
                                                                                
RDB5     ST    R4,ABELEM           SAVE ADDRESS OF BILLING ELEM                 
         USING NUBILD,R4                                                        
*        BAS   RE,BHFILTRS         SEEKS/SETS BH FILTERS                        
* - MOBILE SETS UP MONTH OF SERVICE TABLE                                       
         GOTO1 =A(GOMOBILX),DMCB,(RC),0                                         
         B     RDB15                                                            
                                                                                
* - GETS NEXT ELEMENT                                                           
RDB10    L     R4,ABELEM           GET ADDRESS OF BILLING ELEM                  
         MVI   ELCODE,X'10'                                                     
         BAS   RE,NEXTEL                                                        
         BNE   GBX                                                              
         ST    R4,ABELEM                                                        
* - MATCH BILLING ELEM PROD WITH UNIT PROD                                      
RDB15    CLC   NUBILPRD,NBSPLPRN                                                
         BE    RDB17               YES                                          
         CLC   NUBILPRD,NBPRD      NO/MAKE SURE PROD ON BILL ELEM               
         BE    RDB10                 MATCHES ONE OF UNITS PRODS                 
         CLC   NUBILPRD,NBPRD2       THEY CAN CHANGE UNIT PROD AFTER            
         BE    RDB10                 BILLING/ SKIP FOR NOW                      
         B     RDB10                                                            
*                                                                               
RDB17    CLI   PREBILL,C'Y'        IF REQUESTING PREBILLING                     
         BE    RDB10               SKIP THIS BILLED PRODUCT                     
         GOTO1 DATCON,DMCB,(2,NUBILDAT),(0,RUNDATE)                             
         MVC   INVNUMBR(2),RUNDMM               MONTH (2)                       
         MVC   INVNUMBR+2(4),NUBILNUM           INVNO (4)                       
         MVC   WORKINV,INVNUMBR         ..GETBYMN RETURNS                       
         MVC   WORKYMD,RUNDATE                                                  
         BAS   RE,GETBYMN                                                       
         MVC   CURRBINV,WORK            ..3 BYTE(BINARY Y/M + INV NO)           
         BAS   RE,GETMOS           ..GET MO OF SERVICE OF NBACTDAT              
*                                  .. RETURNED IN BYMOS YR/MOS                  
*                                                                               
         EJECT                                                                  
* - MATCH BILLING ELEMENT TO BILLING RECORD                                     
         LA    R2,KEY                                                           
         USING BILLREC,R2                                                       
         XC    BKEY,BKEY           BUILD BILL REC KEY                           
         MVC   BKEYAM,NBACTAM      FROM UNIT BILLING ELEM                       
         MVC   BKEYCLT,NBACTCLI                                                 
         MVC   WORK(1),NUBILPRD                                                 
         BAS   RE,GETPRD3                                                       
         MVC   BKEYPRD,WORK                                                     
         MVC   BKEYEST,NBACTEST                                                 
         MVC   BKEYYSRV,BYMOS              BYMOS SET FROM NBACTDAT              
         MVC   BKEYMSRV,BYMOS+1                                                 
         MVC   BKEYMBIL(3),CURRBINV        M/Y + 2 BYTE INV NO                  
         MVC   KEYSAVE,KEY                                                      
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'SPTDIR  ',KEY,KEY                     
         CLC   KEY(13),KEYSAVE                                                  
         BE    RDB20                                                            
* - CAN NOT MATCH TO BILLING RECORD                                             
* - TRY UNITS NBACTDAT AS MOS WITHOUT GOING TO MOBILE                           
* - SEE IF THIS WILL FIX                                                        
         GOTO1 DATCON,DMCB,(2,NBACTDAT),(3,MYWORK)                              
         MVC   BYMOS(2),MYWORK                                                  
         XC    KEY,KEY                                                          
         MVC   KEY(13),KEYSAVE                                                  
         MVC   BKEYYSRV(2),BYMOS                                                
         MVC   KEYSAVE(13),KEY                                                  
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'SPTDIR  ',KEY,KEY                     
         CLC   KEY(13),KEYSAVE                                                  
         BE    RDB20                                                            
*          DATA SET NEWRI82    AT LEVEL 037 AS OF 06/06/94                      
* - BUMP MOS UP BY ONE AND TRY                                                  
         ZIC   R1,BYMOS+1                                                       
         LA    R1,1(R1)                                                         
         STC   R1,BYMOS+1                                                       
         XC    KEY,KEY                                                          
         MVC   KEY(13),KEYSAVE                                                  
         MVC   BKEYYSRV(2),BYMOS                                                
         MVC   KEYSAVE(13),KEY                                                  
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'SPTDIR  ',KEY,KEY                     
         CLC   KEY(13),KEYSAVE                                                  
         BE    RDB20                                                            
         DC    H'0'                                                             
         DROP  R2                                                               
                                                                                
RDB20    MVC   BHUBTYPE,NUBILTYP        SET BILL TYPE FROM UNIT ELEM            
*                                       FOR CASH FLOW REJPORTING                
* - GET BILL REC / NEXT ROUTINE EXPECTS R4 - BILLING RECORD                     
         DROP R4                                                                
         L     R4,AIO                                                           
         GOTO1 DATAMGR,DMCB,=C'GETREC',=C'SPTFILE ',KEY+14,(R4),MYDM            
*                                                                               
         CLI   NBRDBELS,0          SET FLAG                                     
         BNE   *+8                                                              
         MVI   NBRDBELS,1                                                       
***      BAS   RE,TSTBHFLT       TEST BILL REC AGAINST BH FILTERS               
***      BNE   RDB10             NO-GET NEXT BILLING ELEMENT                    
         B     GB30              OK-ENTER BILLING HEADER READ ROUTINE           
*                                   TO FILL IN BHBLOCK                          
         EJECT                                                                  
*                                                                               
* - FILL IN BILLING RECORD BLOCK                                                
GB30     L     R4,AIO                                                           
         USING BILLREC,R4                                                       
* ATTEMPT TO TAKE CARE OF MULTIPLE INVOICES ON SAME UNIT                        
* AND NBRDBELS=2 WITH NIUNITS PROBLEM                                           
*                                                                               
         CLI   NBRDBELS,2          HAVE WE GONE TO DRIVER                       
*                                  WITH THIS UNIT ALREADY?                      
         BNE   GB30D               NO                                           
         CLC   BINVNO,DUPINNO      YES/SAME INVO NUMBER ?                       
         BE    GB30D                   YES-SO DON'T COUNT UNIT AGAIN            
         OI    NBVARIND,X'10'          NO-COUNT UNIT IN NIUNIT                  
                                                                                
GB30D    MVC   BHACTKEY,KEY                                                     
         MVC   BHACTCLT(13),BHACTKEY+2                                          
         MVC   BHMNSERV,BMONSERV                                                
         MVC   BHMNSVX,=C'01'      FUDGE FOR MONTH OF SERVICE YMD               
         GOTO1 DATCON,DMCB,(0,BHMNSERV),(2,BHMNSVX)                             
         MVC   BHBPOST,BILPOST                                                  
         MVC   BHINVNO,BINVNO                                                   
         MVC   BHBDATE,BDATE                                                    
         XC    BHBTYPE,BHBTYPE       ..CLEAR BILL TYPE FIELD                    
         MVC   BHBTYPE(2),BTYPE      ..SET BILL TYPE                            
         TM    BILSTAT,BSTTAORQ      ..IF AOR                                   
         BNO   GB32                                                             
         MVC   BHBTYPE(3),=C'AOR'    ..SET AOR                                  
         MVC   BHBTYPE+3(1),BTYPE+1  ..AND NUMERIC BILL TYPE                    
GB32     MVC   BHNET,BLMGR                                                      
         MVC   BHDPT,BLDPT                                                      
         MVC   BHMED,BLMED                                                      
         MVC   BHBSTAT,BILSTAT                                                  
         MVC   BHQDATE,BQDATE                                                   
         MVC   BHDUEDT,BDUEDATE                                                 
         MVC   BHCTYP,BILCTYP      COST TYPE (TIME,CUT-IN ETC)                  
         MVI   BHPKNUM,0           PACKAGE NUMBER                               
         CLC   =C'930830',BQDATE   IGNORE BEFORE THIS DATE                      
         BH    *+10                                                             
         MVC   BHPKNUM,BLPKG       PACKAGE NUMBER                               
*                                                                               
* - GET DATA FROM UNIT ELEM THROUGH BILL FORMULA                                
***      OC    BILBFB(5),BILBFB    DO WE HAVE BILL FORMULA                      
***      BNZ   *+10                                                             
***      MVC   BILBFB(5),BILLFRML  MOVE IT IN FROM AAA PROD                     
                                                                                
         BAS   RE,GETBFRML         RETURNS BILL F0RMULA IN BILLFRML             
         OC    BILLFRML,BILLFRML   ,,IF NO CAN FIND                             
         BZ    *+10                ,,USE WHAT'S ALREADY IN BILBFB               
         MVC   BILBFB(5),BILLFRML                                               
                                                                                
         L     R2,ABELEM           UNIT BILLING ELEMENT                         
         USING NUBILD,R2                                                        
         MVC   BHCTYP,NUBILTYP     COST TYPE (TIME,INT ETC.)                    
         GOTO1 =V(SPBVAL),DMCB,(C'U',0(R2)),MYWORK,BILBFB                       
         B     GB36                                                             
         DROP  R2                                                               
* - MYWORK FILLED IN BY SPBVAL                                                  
GB36     LA    R2,MYWORK                                                        
         USING SPBVALD,R2                                                       
         L     R1,SPBVEGRS                                                      
         CVD   R1,BHBGRS                                                        
         L     R1,SPBVENET                                                      
         CVD   R1,BHBNET                                                        
         L     R1,SPBVACT                                                       
         CVD   R1,BHBACT                                                        
         TM    BILSTAT,BSTTAORQ    ..IF TRUE AOR                                
         BNO   GB40                ..CLEAR NET                                  
         ZAP   BHBNET,=P'0'                                                     
*                                                                               
* - READ B1X PROFILE TO CHECK IF AGENCY USES START INVOICE YEAR                 
GB40     L     R4,ANETWS1          CLIENT HEADER SITS IN ANETWS1                
         USING CLTHDR,R4                                                        
         CLC   CKEYCLT,BHACTCLT    ...DO WE NEED NEW CLIENT HEADER              
         BE    GB50                                                             
         XC    KEY,KEY             ...YES/GET IT                                
         MVC   KEY+1(3),BHACTKEY+1     AGY/MED + CLIENT                         
         MVC   FILENAME,=C'SPTDIR  '                                            
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   FILENAME,=C'SPTFILE '                                            
         MVC   AIO,ANETWS1         CLIENT REC SITS IN ANETWS1                   
         GOTO1 GETREC                                                           
         L     R1,AMYIO         RESET AIO AREA FOR BHREAD                       
         ST    R1,AIO                                                           
GB50     XC    WORK,WORK           READ B1X PROFILE                             
         MVC   WORK(4),=C'SB1X'                                                 
         NI    WORK,X'BF'              LOWER CASE                               
         MVC   WORK+4(2),NBSELAGY      AGENCY                                   
         MVI   WORK+6,C'N'             MEDIA                                    
         GOTO1 NBCLUNPK,DMCB,(CPROF+6,BHACTCLT),WORK+7                          
         MVC   CLTSAV,WORK+7       SAVE 3 BYTE CLI CODE                         
         CLI   COFFICE,0                                                        
         BE    GB52                                                             
         MVI   WORK+10,C'*'                                                     
         MVC   WORK+11(1),COFFICE                                               
GB52     GOTO1 GETPROF,DMCB,WORK,WORK+16,DATAMGR                                
         CLI   WORK+20,0           CHECK IF INVOICE START YEAR                  
         BE    GB55                NO/                                          
         ZIC   R1,WORK+20          YES/GET INV START YR INTO BYTE               
         PACK  DUB,BHBDATE(2)      YEAR OF BILL INVOICE                         
         CVB   R2,DUB                                                           
         SR    R2,R1               GET NUMBER OF YEARS DIFFERENCE               
         MH    R2,=H'12'           NUMBER OF YRS X 12 MONTHS                    
         PACK  DUB,BHBDATE+2(2)    MONTH OF BILL INVOICE                        
         CVB   R1,DUB                                                           
         AR    R2,R1     INV BILL MONTH + MONTHS FROM START INV YEAR            
         CVD   R2,DUB                                                           
         UNPK  WORK+5(5),DUB+6(3)                                               
         MVC   BHINVNO(2),WORK+6   SET TO START OF INV NUMBER                   
*                                                                               
         EJECT                                                                  
GB55     DS    0H                                                               
*                                                                               
GB57     CLI   BHMED,X'40'                                                      
         BH    *+8                                                              
         MVI   BHMED,C'N'          DEFAULT TO NETWORK                           
         MVC   NBPOSTYP,BHMED                                                   
         MVC   NBSTATYP,BHMED                                                   
         OI    NBSPLOPT,X'C0'      SET SPLIT NETIO SWITCH                       
* - READ PACKAGE RECORD TO GET PACKAGE NAME                                     
         CLI   BHPKNUM,0           IF PKG NUMBER                                
         BE    GB58                                                             
         XC    KEY,KEY             GO FOR PKG RECORD                            
         LA    R1,KEY                                                           
         USING NPRECD,R1                                                        
         MVI   NPKTYPE,2                                                        
         MVC   NPKAM,BHACTKEY+1                                                 
         MVC   NPKCLT,BHACTCLT                                                  
         MVC   NPKNET,BHNET        NETWORK                                      
         MVC   NPKEST,BHACTEST     ESTIMATE                                     
         MVC   NPKPACK,BHPKNUM     PACKAGE NUMBER                               
         NETGO NVSETUNT,DMCB                                                    
         MVC   FILENAME,=C'UNTDIR  '                                            
         GOTO1 HIGH                                                             
         CLC   KEY(20),KEYSAVE                                                  
         BNE   GB58                                                             
         MVC   FILENAME,=C'UNTFIL  '                                            
         L     R1,AMYIO                                                         
         ST    R1,AIO                                                           
         GOTO1 GETREC                                                           
         USING NPRECD,R1                                                        
         MVC   BHPKNAM,NPAKNAME                                                 
         DROP  R1,R4                                                            
*                                                                               
GB58     DS    0H                  GET VENDOR NAME                              
         BAS   RE,GETVNDNM                                                      
*                                                                               
* PASS TO DRIVER                                                                
GB60     MVI   GLMODE,GLINPUT                                                   
         GOTO1 DRIVER,DMCB,(R6)                                                 
         MVI   NBRDBELS,2        ..SET FLAG SO NEWRIDRIVE SKIPS ACCGEN          
         NI    NBVARIND,X'FF'-X'10'   TURN OFF FLAG FOR NIUNIT                  
         MVC   DUPINNO,BHINVNO        SAVE INVNO                                
         B     RDB10             ..GET NEXT BILLING ELEM                        
                                                                                
*                                                                               
* IF UNIT HAS MULTIPLE INVOICE NUMBERS ON BILLING ELEMENTS                      
* IF INVOICE NUMBER IS IN KEY (ROW) AND UNIT KEYWORD IS IN COLUMN               
* ( AS IS THE CASE IN HDBILL REPORT )                                           
* ROWS AFTER THE FIRST INVOICE NUMBER FOR SAME UNIT WILL SHOW 0 UNITS           
* BECAUSE NBRDBELS IS SET TO 2 AND THUS SKIPS NIUNIT IN NEWRIDRIVE              
*  USE DUPINNO BELOW TO TRY TO AVOID THE PROBLEM                                
DUPINNO  DS    CL6                                                              
                                                                                
*                                                                               
GBX      DS    0H                                                               
         MVC   AIO,AIO2           RESET UNT FILE/RD SEQ/AIO AREA                
         XC    FILENAME,FILENAME                                                
         XC    DUPINNO,DUPINNO                                                  
         NETGO NVSETUNT,DMCB                                                    
         MVC   KEY,NBKEY           ..REREAD UNIT KEY                            
         GOTO1 HIGH                                                             
         CLC   KEY(20),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
GBXX     B     XIT                                                              
         EJECT                                                                  
* - READ COMMERCIAL CLASS RECORD FOR VENDOR NAME FOR HOME DEPOT                 
*                                                                               
GETVNDNM NTR1                                                                   
         XC    CMMLIDSV,CMMLIDSV                                                
         L     R4,NBAIO            UNIT RECORD                                  
         MVI   ELCODE,X'21'        COMMERCIAL ELEMENT                           
         BAS   RE,GETEL                                                         
         B     *+8                                                              
GVN10    BAS   RE,NEXTEL                                                        
         BNE   GVN20                                                            
         USING NUCMLEL,R4                                                       
         TM    NUCMLFLG,X'C8'      IS CMML ELEM VALID                           
         BNZ   GVN10                                                            
         CLC   NBSPLPRN,NBPRD      FIRST PROD                                   
         BNE   *+10                                                             
         MVC   CMMLIDSV(8),NUCML1                                               
         CLC   NBSPLPRN,NBPRD2     SECOND PROD                                  
         BNE   *+10                                                             
         MVC   CMMLIDSV(8),NUCML2                                               
         MVI   ELCODE,X'23'        ANY FEEDS                                    
GVN15    BAS   RE,NEXTEL                                                        
         BNE   GVN20                                                            
         USING NUFDCEL,R4                                                       
         TM    NUFDCFL2,X'80'      DELETED FEED                                 
         BO    GVN15                                                            
         CLC   NBSPLPRN,NBPRD                                                   
         BNE   *+14                                                             
         MVC   CMMLIDSV(8),NUFDCML1     FIRST PROD FEED                         
         B     GVN20                                                            
         CLC   NBSPLPRN,NBPRD2                                                  
         BNE   GVN20                                                            
         MVC   CMMLIDSV(8),NUFDCML2                                             
         DROP  R4                                                               
GVN20    XC    KEY,KEY             READ CMML PROFILE RECORD FIRST               
         LA    R2,KEY              TO GET COMMERCIAL CLASS                      
         USING CMLRECD,R2                                                       
         MVC   CMLKID,=X'0A21'                                                  
         MVC   CMLKAM,NBACTAM                                                   
         MVC   CMLKCLT,NBACTCLI                                                 
         MVC   CMLKCML,CMMLIDSV                                                 
GVN30    NETGO NVSETSPT,DMCB                                                    
         MVC   DATADISP,=H'24'                                                  
         MVC   FILENAME,=C'SPTDIR  '                                            
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   FILENAME,=C'SPTFIL  '                                            
         L     R1,AMYIO                                                         
         ST    R1,AIO                                                           
         GOTO1 GETREC                                                           
         L     R4,AIO                                                           
         MVI   ELCODE,X'10'              GET CMML DATA ELEM FIRST               
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING CMLDTAEL,R4                                                      
         CLC   CMLCLASS,=4X'00'    IF THERE                                     
         BE    GVN50                                                            
         MVC   WORK(4),CMLCLASS    USE IT                                       
         B     GVN60                                                            
GVN50    L     R4,AIO                                                           
         MVI   ELCODE,X'21'        ELSE  GET COMMERCIAL CLASS ELEM              
         BAS   RE,GETEL                                                         
         BE    *+8                                                              
         BNE   GVNX                                                             
*        DC    H'0'                                                             
         USING CMLCLSEL,R4                                                      
         MVC   WORK(4),CMLCLS      COMMERCIAL CLASS                             
* - READ COMMERCIAL CLASS RECORD FOR CLASS DESCRIPTION(VENDOR)                  
GVN60    XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING CLSKEY,R4                                                        
         MVC   CLSKID,=X'0A44'                                                  
         MVC   CLSKAM,NBACTAM                                                   
         MVC   CLSKCLAS,WORK                                                    
         OC    CLSKCLAS,=X'40404040'    NEED X'40' FILLER                       
         MVC   CLSKCLT,NBACTCLI                                                 
         MVC   WORK(1),NBSPLPRN    NEED 3 BYTE PROD CODE                        
         BAS   RE,GETPRD3                                                       
         MVC   CLSKPROD,WORK     .READ FOR SPECIFIC PRODUCT FIRST               
         MVC   FILENAME,=C'SPTDIR  '                                            
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    GVN70                                                            
         MVC   KEY(13),KEYSAVE    .IF NOT FOUND                                 
         XC    CLSKPROD,CLSKPROD  .TRY PROD=0                                   
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BNE   GVNX                                                             
GVN70    MVC   FILENAME,=C'SPTFIL  '                                            
         GOTO1 GETREC                                                           
         L     R4,AIO                                                           
         MVI   ELCODE,X'10'        CLASS DESCRIPTION ELEM                       
         BAS   RE,GETEL                                                         
         BNE   GVN80               NO ELEM/SKIP CLASS DESC                      
         USING CLSDSCEL,R4                                                      
         MVC   BHCLSDES,CLSDESC                                                 
GVN80    LA    R4,KEY                                                           
         USING CLSKEY,R4                                                        
         MVC   BHVENDCD,CLSKCLAS           SET CMML CLASS CODE                  
         CLI   NBRDBELS,0                                                       
         BNE   *+8                                                              
         MVI   NBRDBELS,1                                                       
GVNX     DS    0H                                                               
         XC    FILENAME,FILENAME                                                
         NETGO NVSETUNT,DMCB                                                    
         B     XIT                                                              
*                                                                               
         EJECT                                                                  
TESTMASK NTR1                                                                   
         LA    R2,NBESTMSK                                                      
         SR    R0,R0                                                            
         SLDL  R0,29                                                            
         SRL   R1,29                                                            
         AR    R2,R0                                                            
         LA    R1,BITLIST(R1)                                                   
         MVC   BITTEST,0(R2)                                                    
         NC    BITTEST,0(R1)                                                    
         CLI   BITTEST,0                                                        
         B     XIT                                                              
***      BE    NO                                                               
***      B     YES                                                              
         SPACE 1                                                                
BITLIST  DC    X'8040201008040201'                                              
BITTEST  DS    CL1                                                              
         EJECT                                                                  
                                                                                
* INPUT:   WORKINV=MONTH(2BYTES)+INVNO(4BYTES)                                  
*          WORKYMD=YYMMDD OF BILLING RUN DATE                                   
*                                                                               
* OUTPUT:  3BYTE 1(Y/M) + 2(INVNO)                                              
GETBYMN  NTR1                                                                   
         PACK  DUB,WORKINV+2(4)   .GET BINARY VALUE OF INVNO                    
         CVB   R1,DUB                                                           
         STCM  R1,3,WORK+1                                                      
* - CONVERT BILLING RUN MONTH TO BINARY                                         
         PACK  DUB,WORKINV(2)     .GET BINARY VALUE OF MONTH                    
         CVB   R1,DUB                                                           
         STCM  R1,1,WORK                                                        
* GET YEAR INTO ZONE OF MONTH BYTE                                              
         PACK  BYTE,WORKYMD+1(1)   .SWITCH FN TO NF  (YYMMDD)                   
         NI    BYTE,X'F0'          .MAKE IT N0                                  
         OC    WORK(1),BYTE        .SET IT INTO FIRSTHALF OF MONTH BYTE         
         B     XIT                                                              
                                                                                
                                                                                
* - ROUTINE GETS MONTH OF SERVICE OF UNIT DATE                                  
GETMOS   DS    0H                                                               
         LA    RF,PERLIST                                                       
GETM4    DS    0H                                                               
         CLC   NBACTDAT,2(RF)      TEST DATE VS START                           
         BE    GETM8                                                            
         BNL   *+6                                                              
         DC    H'0'                TIME TO LENGTHEN PERLIST                     
         CLC   NBACTDAT,4(RF)                                                   
         BNH   GETM8                                                            
         LA    RF,6(RF)            NEXT ENTRY                                   
         B     GETM4               NOTE- LIST ENDS IN FF                        
GETM8    DS    0H                                                               
         MVC   BYMOS,0(RF)         SET YR/MOS                                   
         BR    RE                                                               
*                                                                               
         EJECT                                                                  
*                                                                               
BHFILTRS NTR1                                                                   
***      LA    R2,SPLOPTH                                                       
         CLI   5(R2),0                                                          
         BE    *+8                                                              
         BAS   RE,SCANFIND         SCAN, FIND AND SET BH FILTERS                
***      LA    R2,SPLOTHH                                                       
         CLI   5(R2),0                                                          
         BE    BHFX                                                             
         BAS   RE,SCANFIND         SCAN, FIND AND SET BH FILTERS                
BHFX     XIT1                                                                   
                                                                                
*                                                                               
* - FINDS AND SETS ANY FILTERS IN OPTION/OTHER LINE FOR BH                      
SCANFIND NTR1                                                                   
         LA    R4,SCANBLK                                                       
         GOTO1 SCANNER,DMCB,(20,(R2)),(10,SCANBLK),0                            
         ZIC   R2,DMCB+4                                                        
SCN10    CLC   12(7,R4),=C'BHRDATE'                                             
         BNE   *+12                                                             
         MVI   BHDTYPF,C'R'        SET RDATE FILTER                             
         B     SCN12                                                            
         CLC   12(7,R4),=C'BHPDATE'                                             
         BNE   *+12                                                             
         MVI   BHDTYPF,C'P'        SET PDATE FILTER                             
         B     SCN12                                                            
         CLC   12(7,R4),=C'BHIDATE'                                             
         BNE   *+12                                                             
         MVI   BHDTYPF,C'I'        SET IDATE FILTER                             
         B     SCN12                                                            
         CLC   12(7,R4),=C'BHDDATE'                                             
         BNE   SCN20                                                            
         MVI   BHDTYPF,C'D'        SET DDATE FILTER                             
SCN12    GOTO1 DATVAL,DMCB,(0,22(R4)),WORK                                      
*        L     R1,DMCB                                                          
*        LTR   R1,R1                                                            
*        BNZ   *+6                                                              
*        DC    H'0'        BAD BUG/NEWRIGEN SHOULD HAVE GOT IT                  
*        MVC   WORK+6(6),WORK      ASSUME END = START                           
*        LA    R1,22(R1,R4)                                                     
*        CLI   0(R1),C' '                                                       
*        BE    SCN15                                                            
*        CLI   0(R1),C'-'          CHECK DELIMITER                              
*        BE    *+6                                                              
*        DC    H'0'                BAD BUG                                      
*        LA    R1,1(R1)                                                         
*        ST    R1,DMCB                                                          
*        GOTO1 DATVAL,DMCB,,WORK+6                                              
*        L     R1,DMCB                                                          
*        LTR   R1,R1                                                            
*        BNZ   *+6                                                              
*        DC    H'0'                BAD BUG                                      
*        CLC   WORK+6(6),WORK      CHECK END V START                            
         BNL   *+6                                                              
         DC    H'0'                BAD BUG                                      
SCN15    GOTO1 DATCON,DMCB,(0,WORK),(0,BHBDATFS)                                
         GOTO1 DATCON,DMCB,(0,WORK+6),(0,BHBDATFE)                              
         B     SCN100                                                           
*                                                                               
SCN20    CLC   12(6,R4),=C'BHTYPE'                                              
         BNE   SCN100                                                           
         CLC   22(4,R4),=C'-AOR'                                                
         BE    SCN21                                                            
         CLC   22(3,R4),=C'AOR'                                                 
         BNE   SCN22                                                            
SCN21    MVC   BHAOR,22(R4)                                                     
         B     SCN100                                                           
*                                                                               
SCN22    CLI   22(R4),C'M'         MANUAL BILL FILTER                           
         BE    SCN22B                                                           
         CLC   22(2,R4),=C'-M'                                                  
         BNE   SCN23                                                            
         MVI   BHMANFLG,C'-'       SET EXCLUDE MANUAL BILLS                     
         CLI   24(R4),X'40'        ..IF FURTHER FILTER                          
         BNH   SCN100                                                           
         MVI   BHBFLG,C'B'         ..SET IT                                     
         MVC   BHBFLG+1(1),24(R4)                                               
         B     SCN100                                                           
SCN22B   MVI   BHMANFLG,C'Y'       SET MANUAL BILL FLAG                         
         CLI   23(R4),X'40'        ..IF B1 ETC ALSO FILTER                      
         BNH   SCN100                                                           
         MVI   BHBFLG,C'B'         .. SET IT                                    
         MVC   BHBFLG+1(1),23(R4)  SET B1 ETC                                   
         B     SCN100                                                           
*                                                                               
SCN23    CLI   22(R4),C'B'         B1 ETC ONLY FILTER                           
         BNE   SCN100              NO MATCH/SKIP FILTER                         
         MVC   BHBFLG,22(R4)                                                    
         B     SCN100                                                           
*                                                                               
SCN100   LA    R4,42(R4)           BUMP TO NEXT SCANNER ENTRY                   
         BCT   R2,SCN10                                                         
         B     XIT                                                              
         EJECT                                                                  
* - TESTS BH FILTERS AGAINST BILLING RECORD                                     
*                                                                               
TSTBHFLT NTR1                                                                   
         L     R4,AIO                                                           
         USING BILLREC,R4                                                       
         CLI   BHSELMED,X'40'      MEDIA                                        
         BNH   *+14                                                             
         CLC   BHSELMED,BLMED                                                   
         BNE   TSTNO                                                            
*                                                                               
         CLI   BHMANFLG,C'Y'       MANUAL BILLS ONLY                            
         BNE   *+12                                                             
         TM    BILSTAT,BSTMANQ     ...ONLY MANUAL BILLS                         
         BNO   TSTNO                                                            
         CLI   BHMANFLG,C'-'       ...EXCLUDE MANUAL BILLS                      
         BNE   *+12                                                             
         TM    BILSTAT,BSTMANQ                                                  
         BO    TSTNO                                                            
*                                                                               
         CLI   BHBFLG,X'40'        B1 EST FILTER                                
         BNH   *+14                                                             
         CLC   BTYPE,BHBFLG        ...IS IT MATCH                               
         BNE   TSTNO               ...NO/SKIP                                   
*                                                                               
         CLI   BHAOR,C'-'          EXCLUDE AOR                                  
         BNE   GB28                                                             
         TM    BILSTAT,BSTTAORQ    ..IF TRUE AOR                                
         BO    TSTNO               ..SKIP                                       
*                                                                               
GB28     CLI   BHAOR,C'A'          ONLY AOR RECORDS                             
         BNE   GB29                                                             
         TM    BILSTAT,BSTTAORQ    ..IF NOT AOR                                 
         BNO   TSTNO               ..SKIP                                       
*                                                                               
GB29     CLI   BHBDATFS,0          BILL DATE FILTER                             
         BE    TSTOK                                                            
         MVC   WORK(6),BDATE                                                    
         CLI   BHDTYPF,C'R'        BHRDATE                                      
         BE    GB29D                                                            
         CLI   BHDTYPF,C'P'        POST DATE                                    
         BNE   GB29A                                                            
         GOTO1 DATCON,DMCB,(2,BILPOST),(0,WORK)                                 
         B     GB29D                                                            
GB29A    CLI   BHDTYPF,C'I'        INVOICE DATE                                 
         BNE   GB29B                                                            
         MVC   WORK(6),BQDATE      REQUESTED BILL PRINT DATE                    
         B     GB29D                                                            
GB29B    CLI   BHDTYPF,C'D'        DUE DATE                                     
         BNE   TSTOK                                                            
         GOTO1 DATCON,DMCB,(3,BDUEDATE),(0,WORK)                                
*                                                                               
GB29D    CLC   WORK(6),BHBDATFS                                                 
         BL    TSTNO                                                            
         CLC   WORK(6),BHBDATFE                                                 
         BH    TSTNO                                                            
                                                                                
TSTOK    SR    RE,RE                                                            
TSTNO    LTR   RE,RE                                                            
         B     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
* - CLIENT GROUP FILTERING FOR BILL RECORDS                                     
*   KEY HAS BILLING HEADER RECORD                                               
CGRPFILT NTR1                                                                   
         L     R4,ANETWS1          CLIENT HEADER SITS IN ANETWS1                
         USING CLTHDR,R4                                                        
         CLC   CKEYCLT,KEY+2       ...DO WE NEED NEW CLIENT HEADER              
         BE    VCL2                                                             
         MVC   MYKEY,KEY           ...YES/SAVE CURRENT KEY                      
         XC    KEY,KEY                                                          
         MVC   KEY+1(3),MYKEY+1     AGY/MED + CLIENT                            
         MVC   FILENAME,=C'SPTDIR  '                                            
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   FILENAME,=C'SPTFILE '                                            
         MVC   AIO,ANETWS1         CLIENT REC SITS IN ANETWS1                   
         GOTO1 GETREC                                                           
         L     R1,AMYIO         RESET AIO AREA FOR BHREAD                       
         ST    R1,AIO                                                           
         XC    KEY,KEY            RESET SEQ READ FOR BILL HEADER RECORD         
         MVC   KEY,MYKEY                                                        
         MVC   FILENAME,=C'SPTDIR  '                                            
         GOTO1 HIGH                                                             
* - CHECK CLIENT GROUP FILTERING                                                
VCL2     LA    R0,5                                                             
         LA    RF,CGRP1                                                         
         CLC   NBSELCGR(1),0(RF)       CHECK SCHEME LETTER                      
         BE    VCL5                                                             
VCL4     LA    RF,3(RF)                                                         
         BCT   R0,*-14                                                          
         B     VCLNO                                                            
VCL5     UNPK  DUB(5),1(3,RF)      UNPK PWOS                                    
         LA    R3,DUB                                                           
         LA    RE,NBSELCGR+1                                                    
         LA    R1,4                                                             
VCL6     CLI   0(RE),X'C1'         IF LETTER OR NUMBER                          
         BL    VCL7                                                             
         CLC   0(1,RE),0(R3)       MUST MATCH                                   
         BNE   VCL4                IF NO MATCH,TEST AGAINST NXT CGRP            
VCL7     LA    RE,1(RE)                                                         
         LA    R3,1(R3)                                                         
         BCT   R1,VCL6                                                          
         MVC   NBACTCGR(2),1(RF)   SET CLIENT GROUP CODE  PWOS                  
VCLYES   SR    RE,RE               CLIENT PASSED TESTS                          
VCLNO    LTR   RE,RE                                                            
VCLX     B     XIT                                                              
*                                                                               
MYKEY    DS    CL13                                                             
         EJECT                                                                  
* - FILTER BILLING HEADER                                                       
FILTBH   NTR1                                                                   
         LA    R4,KEY                                                           
         USING BKEY,R4                                                          
         CLI   BHSELCLI,0                                                       
         BE    *+14                                                             
         CLC   BKEYCLT,BHSELCLI                                                 
         BNE   FBX                                                              
         CLI   BHSELPRD,0                                                       
         BE    *+14                                                             
         CLC   BKEYPRD,BHSELPRD                                                 
         BNE   FBX                                                              
         CLI   BHSELEST,0                                                       
         BE    FB8                                                              
         CLI   BHSELESE,0          IS IT ESTIMATE RANGE                         
         BE    FB7                 NO                                           
         CLC   BKEYEST,BHSELEST    YES                                          
         BL    FBNO                                                             
         CLC   BKEYEST,BHSELESE                                                 
         BH    FBNO                                                             
         B     FBYE                                                             
FB7      CLC   BKEYEST,BHSELEST    NO RANGE/MUST MATCH ESTIMATE                 
         BNE   FBX                                                              
FB8      CLI   BHSELSTB,0          YEAR/MONTH OF SERVICE                        
         BE    FB10                                                             
         CLC   BKEYYSRV(2),BHSELSTB                                             
         BL    FBNO                                                             
FB10     CLI   BHSELENB,0                                                       
         BE    FBYE                                                             
         CLC   BKEYYSRV(2),BHSELENB                                             
         BH    FBNO                                                             
FBYE     SR    R4,R4                                                            
*                                                                               
FBNO     LTR   R4,R4                                                            
*                                                                               
FB20     DS    0H                                                               
*                                                                               
FBX      B     XIT                                                              
         DROP  R3,R4                                                            
*                                                                               
         GETEL (R4),DATADISP,ELCODE                                             
*                                                                               
         EJECT                                                                  
*                                                                               
GETPRD3  NTR1                  RETURNS 3 BYTE PROD IN WORK                      
         L     R1,ANETWS1                                                       
         USING CLTHDR,R1                                                        
         LA    R1,CLIST                                                         
         LA    R2,220                                                           
GP5      CLC   WORK(1),3(R1)                                                    
         BE    GOTPRD3                                                          
         LA    R1,4(R1)                                                         
         BCT   R2,GP5                                                           
         MVC   WORK(3),=C'***'                                                  
         B     *+10                                                             
GOTPRD3  MVC   WORK(3),0(R1)                                                    
         B     XIT                                                              
                                                                                
*                                                                               
         USING BHBLOCK,R3                                                       
GETPRD1  NTR1                  USES BHACTPRD AND RETURNS                        
         L     R1,ANETWS1      1 BYTE PROD IN WORK                              
         USING CLTHDR,R1                                                        
         LA    R1,CLIST                                                         
         LA    R2,220                                                           
GTP5     CLC   BHACTPRD,0(R1)                                                   
         BE    GOTPRD                                                           
         LA    R1,4(R1)                                                         
         BCT   R2,GTP5                                                          
         MVC   WORK(1),0                                                        
         B     *+10                                                             
GOTPRD   MVC   WORK(1),3(R1)                                                    
         B     XIT                                                              
         DROP  R1,R3                                                            
         EJECT                                                                  
* - READ AAA PRODUCT REC TO GET BILLING ADDRESS                                 
GETAAANM NTR1                                                                   
         XC    KEY,KEY                                                          
         MVC   KEY+1(1),NBACTAM                                                 
         MVC   KEY+2(2),NBEFFCLI                                                
         MVC   KEY+4(3),=C'AAA'                                                 
         MVC   KEYSAVE(13),KEY                                                  
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'SPTDIR  ',KEY,KEY                     
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R4,=A(MYIO)                                                      
         GOTO1 DATAMGR,DMCB,=C'GETREC',=C'SPTFILE ',KEY+14,(R4),MYDM            
         USING PRDHDR,R4                                                        
         MVC   BILADRSV,PADDR1     BILLING ADDRESS                              
******   MVC   BILLFRML,PBILLBAS   BILLING FORMULA                              
*                                                                               
         MVI   NBFUNCT,NBFRDHI                                                  
         B     XIT                                                              
         EJECT                                                                  
*                                                                               
* BILL RECORD SITS IN AIO                                                       
GETBFRML NTR1                                                                   
         XC    BILLFRML,BILLFRML                                                
         L     R1,AIO              POINT TO BILLING RECORD                      
* READ FOR ESTIMATE  RECORD                                                     
         XC    KEY,KEY                                                          
         MVC   KEY+1(7),1(R1)      AGENCY/MEDIA+CLT+PROD+EST                    
                                                                                
*                                  DO WE ALREADY HAVE BILL FORMULA?             
         LA    R1,100              NUMBER OF PRODS IN LIST                      
         L     RE,=A(BLFRMLST)                                                  
BFR10    CLC   KEY,0(RE)                                                        
         BE    BFR12                                                            
         CLI   0(RE),0                                                          
         BE    BFR15                                                            
         LA    RE,18(RE)                                                        
         BCT   RE,BFR10                                                         
         L     RE,=A(BLFRMLST)         NO MATCH AND LIST IS FULL                
         LA    RF,BLFRMLEQ         CLEAR LIST AND LET'S START OVER              
         XCEF                                                                   
         B     BFR15                                                            
BFR12    MVC   BILLFRML,13(RE)     YES/KEYS MATCH - MOVE IN FORMULA             
         B     XIT                                                              
*                                                                               
BFR15    BAS   RE,BFRHI                                                         
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R4,=A(MYIO2)                                                     
         BAS   RE,BFRGETRC                                                      
         USING ESTHDR,R4                                                        
         OC    EBILLBAS(5),EBILLBAS   IS THERE BILL FORMULA?                    
         BZ    BFR20                                                            
         MVC   BILLFRML,EBILLBAS    YES/PASS IT                                 
         LA    RE,100                                                           
         L     RF,=A(BLFRMLST)                                                  
BFR17    CLI   0(RF),0             EMPTY SLOT?                                  
         BE    BFR18                                                            
         LA    RF,18(RF)                                                        
         BCT   RE,BFR17                                                         
         DC    H'0'                SHOULD NEVER GET HERE                        
BFR18    MVC   0(13,RF),KEY                                                     
         MVC   13(5,RF),BILLFRML                                                
         B     BFRX                                                             
*                                                                               
BFR20    XC    KEY,KEY                                                          
* READ FOR PRODUCT RECORD                                                       
         L     R1,AIO                                                           
         MVC   KEY+1(6),1(R1)      AGENCY/MEDIA+CLT+PROD                        
         BAS   RE,BFRHI                                                         
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R4,=A(MYIO2)                                                     
         BAS   RE,BFRGETRC                                                      
         USING PRDHDR,R4                                                        
         OC    PBILLBAS(5),PBILLBAS   IS THERE BILL FORMULA?                    
         BZ    BFR30                                                            
         MVC   BILLFRML,PBILLBAS    YES/PASS IT                                 
         B     BFRX                                                             
*                                                                               
BFR30    DS    0H                                                               
* OK TRY PRODUCT CODE AAA                                                       
         MVC   KEYSAVE+4(3),=C'AAA'                                             
         MVC   KEY,KEYSAVE                                                      
         BAS   RE,BFRHI                                                         
         CLC   KEY(13),KEYSAVE                                                  
         BNE   BFRX                                                             
         L     R4,=A(MYIO2)                                                     
         BAS   RE,BFRGETRC                                                      
         USING PRDHDR,R4                                                        
         OC    PBILLBAS(5),PBILLBAS                                             
         BZ    BFRX                                                             
         MVC   BILLFRML,PBILLBAS                                                
         B     BFRX                                                             
*                                                                               
BFRX     B     XIT                                                              
*                                                                               
                                                                                
BFRHI    NTR1                                                                   
         MVC   KEYSAVE,KEY                                                      
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'SPTDIR  ',KEY,KEY                     
         B     XIT                                                              
*                                                                               
BFRGETRC NTR1                                                                   
         GOTO1 DATAMGR,DMCB,=C'GETREC',=C'SPTFILE ',KEY+14,(R4),MYDM            
         B     XIT                                                              
         EJECT                                                                  
GETNAMES NTR1                                                                   
         NETGO NVSETSPT                                                         
         CLI   WORK,1                                                           
         BNE   GTNAM5                                                           
* CLIENT RECORD                                                                 
         XC    MYKEY,MYKEY                                                      
         MVC   MYKEY+1(3),WORK+1   SET A/M+CLT CODE                             
         BAS   RE,READIT                                                        
         L     R2,AIO                                                           
         USING CLTHDR,R2                                                        
         MVC   WORK(20),CNAME                                                   
         B     GTNAMX                                                           
* PRODUCT RECORD                                                                
GTNAM5   CLI   WORK,2                                                           
         BNE   GTNAM10                                                          
         MVC   MYKEY+4(3),WORK+1                                                
         CLC   =C'999',MYKEY+4     UNALLOCATED                                  
         BNE   GTNAM5B                                                          
         MVC   WORK(20),=CL20'UNALLOCATED'                                      
         B     GTNAMX                                                           
GTNAM5B  BAS   RE,READIT                                                        
         L     R2,AIO              (IS THIS NEEDED? DEIS: FEB5/03)              
         MVC   WORK(20),PNAME                                                   
         B     GTNAMX                                                           
* ESTIMATE RECORD                                                               
GTNAM10  CLI   WORK,3                                                           
         BE    *+6                                                              
         DC    H'0'                                                             
         CLC   =C'999',MYKEY+4                                                  
         BNE   GTNAM20                                                          
         XC    WORK,WORK                                                        
         B     GTNAMX                                                           
GTNAM20  MVC   MYKEY+7(1),WORK+1                                                
         BAS   RE,READIT                                                        
         L     R2,AIO                                                           
         USING ESTHDR,R2                                                        
         MVC   WORK(20),EDESC                                                   
         B     GTNAMX                                                           
*                                                                               
GTNAMX   NETGO NVSETUNT                                                         
         XC    FILENAME,FILENAME                                                
         MVI   NBFUNCT,NBFRDHI                                                  
         B     XIT                                                              
         DROP  R2                                                               
*                                                                               
READIT   NTR1                                                                   
         XC    KEY,KEY                                                          
         MVC   KEY(13),MYKEY       SET KEY                                      
         MVC   FILENAME,=C'SPTDIR  '                                            
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   FILENAME,=C'SPTFILE '                                            
         GOTO1 GETREC                                                           
         B     XIT                                                              
         EJECT                                                                  
*              LTORG                                                            
         LTORG                                                                  
*                                                                               
MYIO     DS    CL3000                                                           
UDEFLST  DS    CL250                                                            
NETBUFF  DS    CL4000                                                           
MYIO2    DS    CL3000                                                           
BLFRMLST DS    CL1800        KEY(13)+BILFRML(5)  X  100 PRODUCTS                
BLFRMLEQ EQU   *-BLFRMLST                                                       
         EJECT                                                                  
* - READS SPOT00 PROFILE TO SET UP DATE LIST                                    
GOMOBILX NMOD1 0,**MBLX**                                                       
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         B     GOMOB00                                                          
XITX     XIT1                                                                   
                                                                                
* - HAVE WE ALREADY SET UP DATE LIST                                            
GOMOB00  CLC   NBCLICOD,PROFILSV                                                
         BE    XITX                                                             
         MVC   PROFILSV(3),NBCLICOD                                             
                                                                                
* - NEED TO PASS THESE ADDS TO MOBILE                                           
* - NOT ENOUGH ROOM TO LINK THEM IN                                             
         LA    R2,MOBILADS                 ADDRESSES FOR MOBILE                 
         XC    0(4,R2),0(R2)               (GETBROAD) LINKED                    
         MVC   4(4,R2),ADDAY                                                    
         MVC   8(4,R2),GETDAY                                                   
         MVC   12(4,R2),DATCON                                                  
*                                                                               
         XC    WORK(30),WORK                                                    
         MVC   WORK(2),=C'SP'                                                   
         MVC   WORK+2(2),=C'B3'           GET B3 PROFILE                        
         MVC   WORK+4(2),NBEFFAGY         AGENCY ALPHA                          
         MVC   WORK+6(1),NBSELMFL         ..IF FILTERING                        
         CLI   NBSELMFL,0                                                       
         BNE   *+10                                                             
         MVC   WORK+6(1),NBSELMED         ..ELSE SET IT TO NETWORK              
         MVC   WORK+7(3),NBCLICOD         CLIENT                                
         CLI   NBEFFOFF,X'40'                                                   
         BH    *+14                                                             
         MVI   WORK+10,C'*'                                                     
         MVC   WORK+11(1),NBEFFOFF        OFFICE                                
         L     R2,DATAMGR                                                       
         L     R3,ATWA                                                          
         XC    DMCB(16),DMCB                                                    
         GOTO1 GETPROF,DMCB,(0,WORK),MYWORK2,(0,(R2))                           
*                                                                               
         XC    WORK(30),WORK                                                    
         MVC   WORK(2),=C'SP'                                                   
         MVC   WORK+2(2),=C'00'                                                 
         MVC   WORK+4(2),NBEFFAGY         AGENCY ALPHA                          
         MVC   WORK+6(1),NBSELMFL         ..MEDIA FILTER                        
         CLI   NBSELMFL,0                                                       
         BNE   *+10                                                             
         MVC   WORK+6(1),NBSELMED         ..ELSE SET IT TO NETWORK              
         MVC   WORK+7(3),NBCLICOD         CLIENT                                
         CLI   NBEFFOFF,X'40'                                                   
         BH    *+14                                                             
         MVI   WORK+10,C'*'                                                     
         MVC   WORK+11(1),NBEFFOFF        OFFICE                                
         L     R2,DATAMGR                                                       
         L     R3,ATWA                                                          
         XC    DMCB(16),DMCB                                                    
         GOTO1 GETPROF,DMCB,(0,WORK),MYWORK,(0,(R2))                            
* - SET B3 PROFILE VALUES INTO 00 DATE AREAS                                    
         MVC   MYWORK+2(1),MYWORK2                                              
         MVC   MYWORK+6(3),MYWORK2+1                                            
         IC    R0,MYWORK+2       DATE CONTROL                                   
* - SET (NBSELSTR - 1 YEAR)  TO DUB FOR BROAD MOBILE DATELIST                   
         L     R4,=F'-450'                                                      
         GOTO1 ADDAY,DMCB,NBSELSTR,MYWORK2,(R4)                                 
* - ADD 100 DAYS TO NBSELEND FOR DATELIST                                       
         L     R4,=F'100'                                                       
         GOTO1 ADDAY,DMCB,NBSELEND,MYWORK2+6,(R4)                               
*        MVC   MYWORK2+6(6),NBSELEND                                            
                                                                                
* - BUILD LONG LIST OF DATE PAIRS                                               
         L     R2,AMYIO                                                         
         LA    R4,MYWORK         PASS ADDRESS OF 00 PROFILE                     
         GOTO1 =V(MOBILE),DMCB,(208,MYWORK2),((R0),(R2)),MOBILADS,(R4)          
                                                                                
*                                  FIND FIRST PERIOD OF A NEW YEAR              
SETD4    DS    0H                                                               
         BAS   RE,CKNEWYR          TEST NEW YEAR                                
         BZ    SETD6               YES                                          
         CLI   5(R2),0             IF ZERO WE GET LOOP IN SETD8                 
         BE    SETD6                                                            
         LA    R2,4(R2)                                                         
         B     SETD4                                                            
*                                  BUILD  A LIST OF YM, START-END               
SETD6    DS    0H                                                               
         LA    R3,PERLIST                                                       
SETD7    DS    0H                                                               
         ZIC   R0,2(R2)                                                         
         SRL   R0,1                                                             
         STC   R0,BYTE             YEAR                                         
         SR    R4,R4               FOR PER SEQUENCE WITHIN YR                   
SETD8    DS    0H                                                               
         LA    R4,1(R4)                                                         
         MVC   0(1,R3),BYTE        YEAR                                         
         STC   R4,1(R3)            MONTH                                        
         MVC   2(4,R3),0(R2)       START-END OF PER                             
         LA    R3,6(R3)                                                         
         LA    R2,4(R2)                                                         
         CLI   0(R2),X'FF'                                                      
         BE    SETD12              EOL                                          
         BAS   RE,CKNEWYR          TEST NEW YEAR                                
         BZ    SETD7               YES                                          
         B     SETD8                                                            
*                                                                               
SETD12   DS    0H                                                               
*                                                                               
SETDATEX DS    0H                                                               
         B     XITX                                                             
         SPACE 1                                                                
*                                  FIND START OF NEW YEAR                       
*                                  1) A PERIOD THAT SPANS YEAR CHANGE           
*                                     AND BEGINS NO FURTHER AWAY                
*                                     FROM 12/31 THAN IT ENDS                   
*                             OR   2) A PERIOD THAT STARTS BEFORE 1/14          
*                                                                               
CKNEWYR  DS    0H                                                               
         MVC   DUB(4),0(R2)                                                     
         NI    DUB,X'01'           STRIP YEAR                                   
         CLC   DUB(2),NEWYRLO                                                   
         BL    CKNYYES                                                          
*                                                                               
         CLC   DUB(2),PDDEC                                                     
         BNH   CKNYNO                                                           
*                                                                               
         NI    DUB+2,X'01'                                                      
         CLC   DUB+2(2),PDDEC                                                   
         BH    CKNYNO                                                           
*                                                                               
         NI    DUB+1,X'1F'         ISOLATE DAY                                  
         ZIC   RF,DUB+1                                                         
         LA    R0,30                                                            
         SR    R0,RF                                                            
         BNP   CKNYYES             STARTS ON 30TH OR 31ST                       
         STC   R0,DUB+4                                                         
*                                                                               
         NI    DUB+3,X'1F'         ISOLATE DAY                                  
         CLC   DUB+4(1),DUB+3                                                   
         BNH   CKNYYES                                                          
*                                                                               
CKNYNO   DS    0H                                                               
         LTR   RE,RE                                                            
         BR    RE                                                               
CKNYYES  DS    0H                                                               
         SR    R0,R0                                                            
         BR    RE                                                               
*                                                                               
*                                                                               
NEWYRLO  DC    X'002E'             JAN14                                        
PDDEC    DC    X'0180'             DEC00                                        
*                                                                               
MOBILADS DS    4F                  ADDRESSES PASSED TO MOBILE                   
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*              WORKING STORAGE FOR PROGRAM                                      
         SPACE 3                                                                
MYD      DSECT                                                                  
*                                                                               
RELO     DS    A                                                                
DRIVER   DS    A                                                                
ADRONE   DS    A                                                                
*                                                                               
MYOPTS   DS    0CL20               OPTIONS FOR GLOPTS                           
DOUBLNET DS    CL1                 DOUBLE SPACE NETWORK                         
SUPRSDET DS    CL1                 SUPPRESS DETAILS                             
PMKTRECP DS    CL1                 PRINT MARKET REPORT                          
PINVREP  DS    CL1                 PRINT INVOICE REPORT                         
PREBILL  DS    CL1                                                              
POLRUN   DS    CL1                 Y=NO PROD BREAK                              
         DS    CL14                SPARE GLOPTS                                 
*                                                                               
DOWNOPT  DS    CL1          DOWNLOAD OPTION                                     
PEROPT   DS    CL1                                                              
PERTYPE  DS    CL4                                                              
SCH      DS    CL1                                                              
*                                                                               
                                                                                
* - WORK AREA FOR RDBELEM                                                       
WRKAREA  DS    0D                                                               
MYDM     DS    CL96                                                             
ABELEM   DS    F                                                                
AMYIO    DS    F                                                                
COMPLEN  DS    CL1                                                              
BHMANFLG DS    CL1                 MANUAL BILL ONLY                             
BHBFLG   DS    CL2                 B1,B2,B3,ETC FLAG                            
BHDTYPF  DS    CL1                 DATE TYPE FILTER                             
CLTSAV   DS    CL3                                                              
PERIODSV DS    CL6                 MMM/YY                                       
UNBILDUD DS    CL10                UNBILLDUEDATE                                
INVDATSV DS    CL8                 MM/DD/YY INVOICE DATE                        
                                                                                
BILLFRML DS    CL5                                                              
PAKWRK   DS    CL16                PAK WORK AREA                                
SVKEY    DS    CL13                                                             
PROFILSV DS    CL10                                                             
BYMOS    DS    CL2                 BILLING YR/MOS FROM GETMOS                   
RUNDATE  DS    0CL6                                                             
RUNDYY   DS    CL2                                                              
RUNDMM   DS    CL2                                                              
RUNDDD   DS    CL2                                                              
INVNUMBR DS    CL6         CURRENT INV NUMBER MONTH(2) + NUMBER                 
WORKINV  DS    CL6                 TEMP INVOICE WORK AREA                       
WORKYMD  DS    CL6                 TEMP YYMMDD WORK AREA                        
CURRBINV DS    CL3                 TEMP CL1(Y/M) + CL2(INVNO)                   
CMMLIDSV DS    CL18                CMMLID(8)+PRD(1) X 2                         
MYWORK   DS    CL100                                                            
MYWORK2  DS    CL100                                                            
SCANBLK  DS    CL420                                                            
BILADRSV DS    CL120               BILLING ADDRESS SAVE 4 LINES X 30            
PERLIST  DS    XL(15*13*6+1)       15YRS X 13MNTHS X 6                          
WRKLEN   EQU   *-WRKAREA                                                        
*                                                                               
         PRINT OFF                                                              
       ++INCLUDE DRGLOBAL                                                       
       ++INCLUDE NEGENINCLS                                                     
       ++INCLUDE BHBLOCKD                                                       
       ++INCLUDE NEDATELSTD                                                     
       ++INCLUDE NEGENCOM                                                       
       ++INCLUDE NECOMBLOK                                                      
       ++INCLUDE SPGENBILL                                                      
       ++INCLUDE SPBVALD                                                        
       ++INCLUDE NEGBLOCKD                                                      
       ++INCLUDE NEGENUNIT                                                      
       ++INCLUDE NEGENPACK                                                      
       ++INCLUDE SPGENCLT                                                       
       ++INCLUDE SPGENPRD                                                       
       ++INCLUDE SPGENEST                                                       
       ++INCLUDE SPTRCMLCLS                                                     
       ++INCLUDE SPTRCMML                                                       
         PRINT ON                                                               
         EJECT                                                                  
       ++INCLUDE NEWRIFFD                                                       
         ORG   CONTAGH                                                          
       ++INCLUDE NEWRIE1D                                                       
       ++INCLUDE CTGENDIC                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'028NEWRI73   07/12/04'                                      
         END                                                                    
